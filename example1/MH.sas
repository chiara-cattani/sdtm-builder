/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : MH.sas
 PURPOSE       : Create SDTM dataset for Medical History
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-03 - Chiara Cattani - Initial program (copied from SONA and adjusted)
 2026-02-02 - Chiara Cattani - Added MHCODED, MHINITD, MHLTEDID.
******************************************************************************/

/* Configuration */
%nutricia_config;
%let sdtm_domain = %unquote(&_progname);

/* Import source data */
%copy_import (inset = mh_meddra);
%get_meddra_version (indata = work.mh_meddra_1);

/* Keep variables required for the SDTM conversion */
data work.mh_meddra1;
  set work.mh_meddra_1 (rename = (mhspid = _mhspid));
  keep initiateddate lastediteddate codedondate soc_code soc_name mhcurmed mhpr pt_name dictinstance 
       mhendat mhongo hlgt_name hlgt_code hlt_name hlt_code llt_name llt_code pt_code 
       pt_soc_name pt_soc_code soc_list _mhspid mhstdat mhterm subjectid usubjid;
  if not missing(mhterm);
run;

/* Derive MH variables */
data work.mh_meddra2 ;
  length studyid domain usubjid mhspid mhterm mhllt mhdecod mhhlt mhhlgt mhcat mhbodsys mhsoc
         mhstdtc mhendtc mhenrtpt mhentpt mhcurmed mhdict mhmedint mhsoclst subjid sourceid mhcoded mhinitd mhltedid $200
         mhlltcd mhptcd mhhltcd mhhlgtcd mhbdsycd mhsoccd 8.;
  set work.mh_meddra1;
                              
  studyid  = "&studyid";
  domain   = "&sdtm_domain"; 
  mhcat    = "RELEVANT MEDICAL HISTORY AND PRE-EXISTING CONDITIONS";  
  subjid   = strip(subjectid);
  sourceid = catx(' ', "CRF:", put(domain, $form.));  
  
  mhterm   = strip(mhterm);
  mhllt    = strip(llt_name);
  mhdecod  = strip(pt_name);
  mhhlt    = strip(hlt_name);
  mhhlgt   = strip(hlgt_name);
  mhbodsys = strip(soc_name);
  mhsoc    = strip(pt_soc_name);
  mhsoclst = strip(soc_list);
  mhcurmed = strip(mhcurmed);
  mhmedint = strip(mhpr);
  
  if ^missing(_mhspid) then mhspid = strip(put(_mhspid,best.));
    
  %sdtm_dt2dtc(dt = mhstdat, dtc = mhstdtc);
  %sdtm_dt2dtc(dt = mhendat, dtc = mhendtc);
  
  if ^missing(mhongo) then do;
      mhentpt  = "INFORMED CONSENT";
      if      lowcase(mhongo) = "y" then mhenrtpt = "ONGOING";
      else if lowcase(mhongo) = "n" then mhenrtpt = "BEFORE";
  end;
  
  if ^missing(mhdecod) then mhdict = "MedDRA &meddra_version_data.";  
  
  if ^missing(llt_code)    then mhlltcd  = input(llt_code, best.);
  if ^missing(pt_code)     then mhptcd   = input(pt_code, best.);
  if ^missing(hlt_code)    then mhhltcd  = input(hlt_code, best.);
  if ^missing(hlgt_code)   then mhhlgtcd = input(hlgt_code, best.);
  if ^missing(soc_code)    then mhbdsycd = input(soc_code, best.);
  if ^missing(pt_soc_code) then mhsoccd  = input(pt_soc_code, best.);
  
    /* Audit variables */
  %sdtm_dt2dtc(dt = CodedOnDate,dtc = mhcoded);
  %sdtm_dt2dtc(dt = InitiatedDate,dtc = mhinitd);
  %sdtm_dt2dtc(dt = LastEditedDate,dtc = mhltedid);

run;

/* Derive MHSTDY and MHENDY */
proc sql;
  create table work.mh_meddra3 as
    select a.* ,b.rfstdtc
      from 
        work.mh_meddra2 as a
      left join 
        sdtm.dm         as b 
      on a.usubjid = b.usubjid;
quit;

data work.&sdtm_domain;
	length mhstdy mhendy 8.;
  set work.mh_meddra3;
  %sdtm_dtc2dy(dtc= mhstdtc);
  %sdtm_dtc2dy(dtc= mhendtc);
run;

/* Finalize */
%sdtm_create_domain_v2 ( domain     = &sdtm_domain
                       , inset      = &sdtm_domain
                       , keys       = studyid usubjid mhdecod mhterm mhstdtc mhendtc
                       , metavarsset    = metadata.sdtm_study_variables
                       , metadomainsset = metadata.sdtm_study_domains
                       , deldupkeys = N);