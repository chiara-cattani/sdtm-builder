/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : TV.sas
 PURPOSE       : Create SDTM data for Trial Visits
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-02 - Chiara Cattani - Initial program (copied from SONA and adjusted)
 2026-02-05 - Chiara Cattani - Updated TVSTRL.
******************************************************************************/

/* Configuration */
%nutricia_config;

%let sdtm_domain = %unquote(&_progname);
 
data work.tv1;
  length studyid 
         domain
         visitval $200;

  /* Set STUDYID and DOMAIN. */ 
  studyid = "&studyid";
  domain  = "&sdtm_domain";
  
  /* VISITVAL is generated. */
  visitval = "SCR";    output;
  visitval = "V1";     output;
  visitval = "PW1";    output;
  visitval = "PW5";    output;
  visitval = "V2";     output;
  visitval = "PW15";   output;
  visitval = "PW20";   output;
  visitval = "V3";     output;
  visitval = "PFU";    output;
run;

data &sdtm_domain;
  length tvstrl armcd arm $200;
  set work.tv1 (rename=(visitval=eventid));
  
  options noquotelenmax;
  
  %derive_visit_vars();
   
  /* TVSTRL set according to study protocol */ 
  if      visitnum = 1  then tvstrl = "Between Day -3 and Day 1";
  else if visitnum = 2  then tvstrl = "Day 1";
  else if visitnum = 3  then tvstrl = "1 week +/- 2 days from Visit 1";
  else if visitnum = 4  then tvstrl = "5 weeks +/- 2 days from Visit 1"; 
  else if visitnum = 5  then tvstrl = "10 weeks +/- 3 days from Visit 1";
  else if visitnum = 6  then tvstrl = "15 weeks +/- 3 days from Visit 1";
  else if visitnum = 7  then tvstrl = "Within 7 days before Visit 3";
  else if visitnum = 8  then tvstrl = "20 weeks +/- 7 days from Visit 1"; 
  else if visitnum = 9  then tvstrl = "2 weeks +/- 3 days after last study product intake";
  
  ARMCD = "";
  ARM = "";
 
  drop eventid;
run;

options quotelenmax;
  
/* Design is applied and checks are made. */
%sdtm_create_domain_v2 ( domain      = &sdtm_domain
                    , inset          = &sdtm_domain
                    , keys           = studyid armcd visitnum
                    , metavarsset    = metadata.sdtm_study_variables
                    , metadomainsset = metadata.sdtm_study_domains
                    , deldupkeys     = N);
