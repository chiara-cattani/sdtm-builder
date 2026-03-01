/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : /SAS/NUTRI-IRON/Files/15 SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : PR.sas
 PURPOSE       : Create SDTM data for Procedures
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-15 - Kirill Novichkov - Initial version
 2025-12-22 - Kirill Novichkov - v1.1: PREVAL, PRORIG, PRREASOC, PRSOCLST added
 2026-01-28 - Chiara Cattani   - Minor update: keep version for meddra.
 2026-02-02 - Chiara Cattani   - Added PRCODED, PRINITD, PRLTEDID.
 2026-02-04 - Chiara Cattani   - Fixed audit variables.
******************************************************************************/

%nutricia_config;

%let domain = %unquote(&_progname);

* Prepare RAW data ;
%copy_import (inset=pr);
%copy_import (inset=pr_meddra);
%copy_import (inset=lbs_img);
%copy_import (inset=lbs_simg);

proc sort data=pr_1; 
  by subjectid prspid; 
run;
proc sort data=pr_meddra_1; 
  by subjectid prspid; 
run;
proc sort data=lbs_simg_1 out=lbs_simg_all; 
  by subjectid lbs_dimg; 
run;
proc sort data=lbs_img_1; 
  by subjectid eventid; 
run;

proc format;
  value $lbs_img_prtrt
  "IMDHNDND" = "IMAGE DORSAL VIEW HANDS"
  "IMPHNDND" = "IMAGE PALM VIEW HANDS"
  "VLLNDND"  = "VIDEO CHILD'S LOWER LIP"
  "VLELNDND" = "VIDEO CHILD'S LOWER EYELID"
  "VTNDND"   = "VIDEO CHILD'S TONGUE";
run;

* Create 1 record per each OCCUR-REASOC-DATE ;
data lbs_img_all;
  length prtrt proccur prreasoc prstdat prcoded prinitd prltedid $200;
  set lbs_img_1;
  
  array occur  [5] lbs_imdhndnd  lbs_imphndnd  lbs_vllndnd lbs_vlelndnd  lbs_vtndnd;
  array reasoc [5] lbs_imdhndr   lbs_imphndr   lbs_vllndr  lbs_vllelndr  lbs_vtndr;
  array date   [5] lbs_dhdate    lbs_phdate    lbs_vlldate lbs_vlyldate  lbs_vtdate;
  
  do i=1 to 5;
    prtrt     = put(scan(upcase(vname(occur[i])), 2, '_'), $lbs_img_prtrt.);
    proccur   = ifc(^missing(occur[i]), "N", "Y");
    prreasoc  = reasoc[i];
    prstdat   = date[i];
    /* Audit variables */
    prcoded = '';
    %sdtm_dt2dtc(dt = InitiatedDate,dtc = prinitd);
    %sdtm_dt2dtc(dt = LastEditedDate,dtc = prltedid);
    output;
  end;
  
  keep subjectid eventid prtrt proccur prreasoc prstdat event: prcoded prinitd prltedid;
run;

* Merge all raw AE related datasets into a single dataset ;
data pr_all;
	length prcoded prinitd prltedid $200;
  merge pr_1(drop=initiateddate lastediteddate) pr_meddra_1 (keep=subjectid prspid soc_: pt_: hlgt_: hlt_: llt_: DictInstance version codedondate initiateddate lastediteddate);
  by subjectid prspid;
    
  /* Audit variables */
  %sdtm_dt2dtc(dt = CodedOnDate,dtc = prcoded);
  %sdtm_dt2dtc(dt = InitiatedDate,dtc = prinitd);
  %sdtm_dt2dtc(dt = LastEditedDate,dtc = prltedid);

run;

%get_meddra_version(indata = pr_all);

data lbs_simg_all;
		length prcoded prinitd prltedid $200;
	set lbs_simg_all;
	  /* Audit variables */
  prcoded = '';
  %sdtm_dt2dtc(dt = InitiatedDate,dtc = prinitd);
  %sdtm_dt2dtc(dt = LastEditedDate,dtc = prltedid);

run;

* Append all raw data before merging with SDTM.DM ;
data pr_all2;
  length sourceds prtrt prstdat prendat $200;
  set pr_all lbs_simg_all (rename=lbs_dimg=prstdat) lbs_img_all
  indsname = _inds;
  
  sourceds = tranwrd(scan(_inds,2,'.'),"_ALL","");
run;

proc sort data=pr_all2;
  by subjectid;
run;      

* Merge required SDTM.DM variables and derive SDTM.PR variables ;
proc format;
  value $prcat
  "PR" = "MEDICAL PROCEDURE"
  "LBS_IMG" = "IMAGES AND VIDEOS"
  "LBS_SIMG" = "COLLECTION STOOL IMAGE";
run;

data &domain.; 
  length prlnkid prdict prcat preval prorig $200;
  label prreasoc  = 'Reason for Occur Value'
        preval    = 'Evaluator'
        prorig    = 'Origin';
  merge pr_all2 (rename=(subjectid=subjid prspid=_prspid) drop=usubjid in=a) 
        sdtm.dm (keep=subjid studyid usubjid rfstdtc);
  by subjid;
  if a;
  
  domain    = "&domain";
  prspid    = put(_prspid, best.-l);
  sourceid  = "CRF: " !! put(sourceds, $form.);  
  prcat     = put(sourceds, $prcat.);
  prllt     = llt_name;
  prlltcd   = input(llt_code, best.);
  prdecod   = pt_name;
  prdict    = ifc(^missing(prdecod), "MedDRA &meddra_version_data.", " ");
  prptcd    = input(pt_code, best.);
  prhlt     = hlt_name;
  prhltcd   = input(hlt_code, best.);
  prhlgt    = hlgt_name;
  prhlgtcd  = input(hlgt_code, best.);
  prbodsys  = soc_name;
  prbdsycd  = input(soc_code, best.);
  prsoc     = pt_soc_name;
  prsoccd   = input(pt_soc_code, best.);
  PRSOCLST  = soc_list;
  
  * CRF 'Collection Stool Image' page specific derivations ;
  if sourceds = "LBS_SIMG" then do;
    prtrt  = 'STOOL SAMPLE IMAGE';
    preval ='PARENT';
    prorig ='ePRO';
  end;
  * CRF 'Images and Videos' page specific derivations ;
  if sourceds = "LBS_IMG" then do;
    prpresp = 'Y';
    prorig  = 'CRF';
    preval  = 'INVESTIGATOR';
  end;  
  * CRF 'Medical Procedure' page specific derivations ;
  if sourceds = "PR" then do;
    prorig  = 'CRF';
    preval  = 'INVESTIGATOR';
    
    prindc = prindicat;
    if upcase(prindc) = "PREVENTIVE / FOR SCREENING PURPOSES" then do;
      prindc = "OTHER";
      prindcx = prindc;
    end;
    
    if ^missing(prongo) then do;
      if prongo = "Y" then prenrtpt  ='ONGOING';
      if prongo = "N" then prenrtpt  ='BEFORE';
      prentpt = 'END OF STUDY';
    end;
  end;
  
  %sdtm_dt2dtc(dt=  prstdat, dtc=prstdtc); 
  %sdtm_dtc2dy(dtc= prstdtc); 
  %sdtm_dt2dtc(dt=  prendat, dtc=prendtc);  
  %sdtm_dtc2dy(dtc= prendtc); 
  %derive_visit_vars();

  if sourceds = "LBS_SIMG" then prlnkid = catx('-', prtrt, prstdtc);
run;

* Get the list of key variables from the metadata ;
proc sql noprint;  
  select tranwrd(compress(keys), ',', ' ') into: keyvars from metadata.sdtm_study_domains where domain="&domain";
quit;

* Apply metadata and store into SDTM folder ;
%sdtm_create_domain_v2 ( domain           = &domain.
                        , inset           = &domain.
                        , keys            = STUDYID USUBJID PRCAT PRTRT VISITNUM PRSTDTC /* metadata keys do not contain PRCAT and VISITNUM */
                        , deldupkeys      = N
                        , addsuppvars     = PREVAL PRORIG PRREASOC
                        , metavarsset     = metadata.sdtm_study_variables
                        , metadomainsset  = metadata.sdtm_study_domains);