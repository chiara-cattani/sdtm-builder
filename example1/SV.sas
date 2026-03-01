/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : SV.sas
 PURPOSE       : Create SDTM dataset for Subject Visits
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-03 - Chiara Cattani - Initial program (copied from SONA and adjusted)
 2025-12-04 - Chiara Cattani - Added SVCNTMOD.
******************************************************************************/

/* Configuration */
%nutricia_config;
%let sdtm_domain = %unquote(&_progname);

/* Import source data */
%copy_import(inset = event_dates);

/* SV */
data work.sv1;
  length domain studyid subjid svstdtc svendtc sourceid svcntmod $200;
  set work.event_dates_1 (keep = usubjid subjectid eventid eventinitiateddate eventstatus);
  where upcase(eventid) in ("SCR", "V1", "PW1", "PW5", "V2", "PW15", "PW20", "V3", "PFU") and upcase(eventstatus) = "INITIATED";

  /* Assign study and domain identifiers */
  studyid = "&studyid";
  domain  = "&sdtm_domain";
  subjid  = strip(subjectid);

  /* Derive SVSTDTC from eventinitiateddate */
  %sdtm_dt2dtc(dt = eventinitiateddate, dtc = _svstdtc);

  /* If time is 00:00, keep only date part */
  if substr(scan(_svstdtc, 2, "T"), 1) = "00:00" then svstdtc = substr(scan(_svstdtc, 1, "T"), 1);
  else svstdtc = _svstdtc;

  /* SVENDTC is same as SVSTDTC */
  svendtc = svstdtc;

  /* Source identifier */
  sourceid = "Viedoc Event";
  
  /* Contact mode */
  if upcase(eventid) in ("SCR", "V1", "V2", "V3") then svcntmod = "IN PERSON";
  else if upcase(eventid) in ("PW1", "PW5", "PW15", "PW20", "PFU") then svcntmod = "TELEPHONE CALL";
  else call missing(svcntmod);

  drop eventinitiateddate eventstatus _:;
run;

/* Merge with DM to get RFSTDTC for study day derivation */
proc sql;
  create table work.sv2 as
    select a.*, b.rfstdtc
    from work.sv1 as a
    left join sdtm.dm as b
    on a.usubjid = b.usubjid;
quit;

/* Final derivations */
data &sdtm_domain;
  length svstdy svendy 8
         svpresp svoccur $200;
  set work.sv2;

  /* Derive SVSTDY and SVENDY */
  %sdtm_dtc2dy(dtc = svstdtc);
  %sdtm_dtc2dy(dtc = svendtc);

  /* Derive VISIT and VISITNUM */
  %derive_visit_vars();

  /* Assign SVPRESP and SVOCCUR */
  svpresp = "Y";
  if not missing(svstdtc) then svoccur = "Y";

  drop rfstdtc;
run;

/* Finalize */
%sdtm_create_domain_v2(
  domain     = &sdtm_domain,
  inset      = &sdtm_domain,
  keys       = studyid usubjid visitnum,
  addsuppvars    = SVCNTMOD,
  metavarsset    = metadata.sdtm_study_variables,
  metadomainsset = metadata.sdtm_study_domains,
  deldupkeys = N
);