/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : IE.sas
 PURPOSE       : Create SDTM data for Inclusion/Exclusion Criteria Not Met
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-05 - Chiara Cattani - Initial program (Copied and adapted from SONA)
 2026-01-30 - Chiara Cattani - Consider only IE at screening.
******************************************************************************/

/* Configuration */
%nutricia_config;
%let sdtm_domain = %unquote(&_progname);

/* Import source data */
%copy_import(inset = ie);

/* Step 1: Check consistency: no results reported when IEYN = "Y" */
data work.check_ie(drop=eventid);
  set work.ie_1 (keep = usubjid ieyn ieintestcd: ieextestcd: eventid where=(strip(eventid)='SCR'));
  array incl {*} ieintestcd:;
  array excl {*} ieextestcd:;
  
  do i = 1 to dim(incl);
    if lowcase(strip(ieyn)) = "y" and not missing(incl[i]) then do;
      put '###WARNING: Subject [' usubjid '] was considered eligible but failed inclusion criterion [IEINTESTCD' i ']';
    end;
  end;

  do j = 1 to dim(excl);
    if lowcase(strip(ieyn)) = "y" and not missing(excl[j]) then do;
      put '###WARNING: Subject [' usubjid '] was considered eligible but failed exclusion criterion [IEEXTESTCD' j ']';
    end;
  end;
run;

/* Step 2: Identify non-eligible subjects */
%let not_eligible_subjects     = ;
%let not_eligible_subjects_num = 0;

proc sql noprint;
  select distinct subjectid,
         count(distinct subjectid)
    into :not_eligible_subjects separated by ",",
         :not_eligible_subjects_num trimmed
    from work.ie_1
    where lowcase(strip(ieyn)) ne "y" and strip(eventid)='SCR';
quit;

/* Step 3: Subset IE criteria not met */
data work.ie_2 (keep = usubjid subjectid eventdate eventid iein: ieex:);
  set work.ie_1;
  where lowcase(ieyn) = 'n' and strip(eventid)='SCR';
run;

/* Step 4: Get number of inclusion/exclusion criteria */
proc sql noprint;
  select max(input(compress(name, , 'a'), best.)) into :max_in
  from dictionary.columns
  where libname = 'WORK' and memname = 'IE_2' and name ? 'IEIN';

  select max(input(compress(name, , 'a'), best.)) into :max_ex
  from dictionary.columns
  where libname = 'WORK' and memname = 'IE_2' and name ? 'IEEX';
quit;

/* Step 5: Transpose criteria not met */
%macro transpose_ie(prefix=, cat=);
  data work.ie_&prefix.;
    length studyid domain ietestcd iecat ieorres iestresc subjid sourceid $200;
    set work.ie_2;
    studyid = "&studyid.";
    domain  = "&sdtm_domain.";
    subjid  = strip(subjectid);
    sourceid = catx(' ', "CRF:", put(domain, $form.));

    %do i = 1 %to &&max_&prefix.;
      if not missing(ie&prefix.testcd&i.) then do;
        ietestcd = upcase("&prefix.CL" || put(&i., z2.));
        iecat    = upcase("&cat.");
        ieorres  = ifc("&prefix." = "in", "N", "Y");
        iestresc = ieorres;
        output;
      end;
    %end;
  run;
%mend;

/* Call macro for inclusion and exclusion separately */
%transpose_ie(prefix=in, cat=inclusion);
%transpose_ie(prefix=ex, cat=exclusion);

/* Combine both into ie_3 */
data work.ie_3;
  set work.ie_in work.ie_ex;
  %derive_visit_vars;
run;

/* Step 6: Create format for IETEST from TI */
data ie_fmt;
  length fmtname start label $1100;
  set sdtm.ti;
  fmtname = '$ietestf';
  start   = ietestcd;
  label   = ietest;
run;

proc format cntlin=ie_fmt; run;

/* Step 7: Derive IETEST, IEDTC, IEDY if needed */
%if &not_eligible_subjects_num. > 0 %then %do;
  proc sql;
    create table work.ie_4 as
    select a.*, b.rfstdtc
    from work.ie_3 as a
    left join sdtm.dm as b
    on a.usubjid = b.usubjid;
  quit;

  data work.&sdtm_domain.;
    length iedtc $200 ietest $1100 iedy 8;
    set work.ie_4;
    ietest = put(ietestcd, $ietestf.);
    if index(ietest, "’") > 0 then put "###WARNING: ietest=" ietest "has a curved apostrophe.";
    %sdtm_dt2dtc(dt=eventdate, dtc=iedtc);
    %sdtm_dtc2dy(dtc=iedtc, dy=iedy, RFDTC=rfstdtc, SUBJID=USUBJID);
  run;
%end;
%else %do;
  data work.&sdtm_domain.;
    length ietest $1100;
    set work.ie_3;
    ietest = put(ietestcd, $ietestf.);
    if index(ietest, "’") > 0 then put "###WARNING: ietest=" ietest "has a curved apostrophe.";
  run;
%end;

/* Finalize */
%sdtm_create_domain_v2(
  domain     = &sdtm_domain.,
  inset      = &sdtm_domain.,
  keys       = studyid usubjid ietestcd,                       
  metavarsset    = metadata.sdtm_study_variables,
  metadomainsset = metadata.sdtm_study_domains,
  deldupkeys = N
);