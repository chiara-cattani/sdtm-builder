/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : /SAS/NUTRI-IRON/Files/15 SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : QS.sas
 PURPOSE       : Create SDTM data for Questionnaires
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-09 - Kirill Novichkov - Initial version
 2025-12-17 - Kirill Novichkov - v1.1: Update after QC findings
 2025-12-22 - Kirill Novichkov - v1.2: For QSTESTCD = 'FIVC08' QSSTRESC collected as Yes/No
 2026-02-06 - Kirill Novichkov - v1.3: missing FIVC08 records added; 
                                 '≥' and '≤' in QSORRES remapped as '>=' and '<=';
                                 QSLOBXFL added;
******************************************************************************/

proc datasets library=work kill nolist;
quit;

%nutricia_config;

%let domain = %unquote(&_progname);

* Prepare raw datasets ;
* ASQ-3 ;
%copy_import(inset=asq3);
proc sort data=asq3_1; 
  by subjectid eventid asq3d asq31; 
run;
proc transpose data=asq3_1 (where=(missing(asq3_ndnd))) out=asq3_tests;
  by subjectid eventid asq3d asq31;
  var asq3c1 -- asq3pe1;
run;

* Feedback Image and Video Collection ;
%copy_import(inset=fivc);
proc sort data=fivc_1; 
  by subjectid eventid fivc9; 
run;
proc transpose data=fivc_1 out=fivc_tests;
  by subjectid eventid eventdate fivc9;
  var fivc1 -- fivc8;
run;

* Iron Intake Questionnaire ;
%copy_import(inset=ir);
proc sort data=ir_1; 
  by subjectid eventid; 
run;
proc transpose data=ir_1 (where=(missing(irndnd))) 
  out=ir_tests (where=(^(irbf1="N" and _name_ = "IRBF2"))); /* drop empty IRBF2 record when IRBF1 = N */
  by subjectid eventid ir1 irbf1;
  var irbf1 -- iroth;
run;

* Create formats: ;
* Raw variable name -> QSTESTCD ;
proc format;
  value $rawvar_to_qstestcd
  "ASQ3C1"  = "ASQ301"
  "ASQ3GM1" = "ASQ302"
  "ASQ3FM1" = "ASQ303"
  "ASQ3PS1" = "ASQ304"
  "ASQ3PE1" = "ASQ305"
  "IRBF1"   = "IRIQ01A"
  "IRBF2"   = "IRIQ01B"
  "IRGUM"   = "IRIQ02"
  "IRCER"   = "IRIQ03"
  "IRLIV"   = "IRIQ04"
  "IRMTS"   = "IRIQ05"
  "IRCFE"   = "IRIQ06"
  "IROTH"   = "IRIQ07";
  
  value $yesno
  "Y" = "Yes"
  "N" = "No";
run;

* QSTESTCD -> QSTEST ;
data cntlin;
  set metadata.sdtm_study_codelists_terms (where=(CODELIST_ID in ("ASQ3TC", "FIVCTC", "IRIQTC"))); 
  fmtname = 'qstest';
  start   = submission_value;
  label   = decode;
  type    = 'C';
  keep fmtname start label type;
run;
proc sort data=cntlin; 
  by _all_; 
run;
proc format cntlin=work.cntlin;
run;

* Append all raw datasets ;
data qs_all;
  length _name_ _label_ $200; 
  set asq3_tests (rename=col1=asq3res)
      asq3_1 (where=(^missing(asq3_ndnd)))
      fivc_tests (rename=col1=fivcres)
      ir_tests (rename=col1=irres)
      ir_1 (where=(^missing(irndnd)))
  indsname=_inds;
  
  subjid = subjectid;
  sourceds  = scan(_inds,2,'.');
run;

proc sort data=qs_all; 
  by subjid; 
run;

* Merge SDTM.DM variables and create SDTM.QS variables ;
data &domain.0;
  length qstestcd qsreasnd qsdat qsorres qsstresc qsscat $200; 
  merge qs_all (in=a drop=usubjid) 
        sdtm.dm (keep=subjid studyid usubjid rfstdtc rfxstdtc);
  by subjid;
  if a;
  
  qscat     = put(scan(sourceds,1,'_'), $form.);
  sourceid  = "CRF: " !! strip(qscat);
  qscat = upcase(qscat);
  
  domain    = "&domain";
  subjid    = subjectid;
  qsorig    = "CRF";
  qseval    = "INVESTIGATOR";  
  
  if cmiss(asq3_ndnd,irndnd)<2 then do;
    qstestcd = "QSALL";
    qsstat   = "NOT DONE";
    qsreasnd = strip(coalescec(asq3_rnd, ir3));
  end;
  else do;
    qstestcd = put(tranwrd(_name_,"FIVC","FIVC0"), $rawvar_to_qstestcd.);
    qsdat    = strip(coalescec(asq3d, ir1, eventdate));
    %sdtm_dt2dtc(dt=qsdat, dtc=qsdtc);
    %sdtm_dtc2dy(dtc=qsdtc);
  end;
  
  qstest    = tranwrd(put(qstestcd, qstest.), "FIVC06-there", "FIVC06-There");
  if ^missing(asq31) then qsscat = catx(' ', asq31, 'QUESTIONNAIRE');
  
  %derive_visit_vars();
  
  qsorres   = coalescec(put(asq3res,best.-l), fivcres, irres);
  * make sentence case, keeping < and >= symbols ;
  if qstestcd in ("FIVC01","FIVC02","FIVC04","FIVC07","FIVC08") or qscat = "IRON INTAKE QUESTIONNAIRE" then do;
    if compress(substr(qsorres,1,1),,'a') = " " and qsorres ^= "RARELY/NEVER" then qsorres = propcase(scan(qsorres,1)) !! lowcase(substr(qsorres, index(qsorres," ")));
    else qsorres = lowcase(qsorres);
  end;
  if qstestcd = 'IRIQ01A' then qsorres   = put(qsorres, $yesno.);
  
  qsorres   = tranwrd(tranwrd(qsorres, "≥", ">="), "≤", "<=");
  qsstresc  = qsorres;
  
  if qstestcd = 'FIVC08' then qsothx = fivc9;
  if qscat = 'ASQ-3' then qsstresn = input(qsorres, ??best.);
run;

* Derive QSLOBXFL ;
%derive_lobxfl(
    domain=QS,
    inds=&domain.0,
    outds=&domain.,
    subjid=USUBJID,
    testcd=QSTESTCD,
    dtc=QSDTC,
    orres=QSORRES,
    stat=QSSTAT,
    dm_ds=sdtm.dm,
    base_before_exp=Y
);

* Get the list of key variables from the metadata ;
proc sql noprint;  
  select tranwrd(compress(keys), ',', ' ') into: keyvars from metadata.sdtm_study_domains where domain="&domain";
quit;

* Apply metadata and store into SDTM folder ;
%sdtm_create_domain_v2 ( domain           = &domain.
                        , inset           = &domain.
                        , keys            = studyid usubjid qscat qstestcd visitnum /*&keyvars.*/
                        , deldupkeys      = N
                        , metavarsset     = metadata.sdtm_study_variables
                        , metadomainsset  = metadata.sdtm_study_domains);