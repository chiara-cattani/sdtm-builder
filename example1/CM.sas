/*******************************************************************************
                             Danone Nutricia Research
********************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : CM.sas
 PURPOSE       : Create SDTM data for Concomitant Medications
********************************************************************************
  Input       : ..\04 Import Data\cm_whodrug.sas7bdat
                ..\41 SDTM Data\dm.sas7bdat
  Output      : ..\41 SDTM data\cm.sas7bdat
********************************************************************************
  Modification history:     
  2025-12-01 - Radmila Ishchenko - Initial program
  2026-02-03 - Radmila Ishchenko - technical coding variables added CMCODED CMINITD CMLTEDID
/********************************************************************************/

/* Configuration */
%nutricia_config;

%let sdtm_domain = %unquote(&_progname);

/*****************************************************************************
  - Use macro %copy import to copy the raw dataset to the work folder,
    with name work.&inset_1
  - Creates USUBJID and removes formats and informats
  - by = can be used to sort, default is USUBJID
*****************************************************************************/
%copy_import (inset = cm_whodrug);

/* Derive the WhoDrug version for CMDICT to be used in the following data step */
%get_whodrug_version (indata = work.cm_whodrug_1);

/* Keep variables required for the SDTM conversion */
proc sql noprint;
  create table work.cm1 as
  select cm.*, dm.RFSTDTC
  from work.cm_whodrug_1 (where = (^missing(cmtrt))) as cm
  left join sdtm.dm as dm
  on cm.usubjid = dm.usubjid;
quit;

data cm;
  length STUDYID DOMAIN CMSPID CMTRT CMDECOD CMCAT 
         CMINDC CMCLAS CMCLASCD CMDOSTXT CMDOSU CMDOSFRQ 
         CMROUTE CMSTDTC CMENDTC CMENRTPT CMENTPT 
         CMATC1 CMATC2 CMATC3 CMATC4 CMDICT CMDSFRQX CMINDCX SOURCEID SUBJID  $200;
  set cm1(rename = (cmspid = o_cmspid));
  STUDYID  = "&studyid";
  DOMAIN   = "&sdtm_domain";
  CMSPID   = strip(put(o_cmspid,best.));
  CMTRT    = CMTRT;
  CMDECOD  = CMDECOD;
  CMCAT    = "CONCOMITANT MEDICATIONS AND NUTRITIONAL SUPPLEMENTS";
  CMINDC   = INDICAT;
  CMCLAS   = CMCLAS;
  CMCLASCD = CMCLASCD;
  CMDOSE   = ifn(compress(cmdostxt,"1234567890.,")  = "", input(tranwrd(cmdostxt,",","."),best.), .);
  CMDOSTXT = ifc(compress(cmdostxt,"1234567890.,") ^= "", cmdostxt, "");
  CMDOSU   = CMDOSU;
  CMDOSFRQ = CMFRQ;
  CMROUTE  = CMROUTE;
  %sdtm_dt2dtc(dt = cmstdat,dtc = cmstdtc);
  %sdtm_dt2dtc(dt = cmendat,dtc = cmendtc);
  %sdtm_dtc2dy(dtc= cmstdtc);
  %sdtm_dtc2dy(dtc= cmendtc);
  /*--------
  CMENRTPT , CMENTPT
  -------*/
  if CMONGO = "Y" then CMENRTPT = "ONGOING" ;
  if CMONGO = "N" then CMENRTPT = "BEFORE";
  if CMONGO ne "" then CMENTPT = "END OF STUDY";
  CMATC1   = CMATC1;
  CMATC2   = CMATC2;
  CMATC3   = CMATC3;
  CMATC4   = CMATC4;
  CMDICT   = ifc(cmdecod ne "", "WHODrug &whodrug_version_data.","");
  CMDSFRQX = CMFRQSP;
  CMINDCX  = CMINDSP;
  SOURCEID = catx(' ',"CRF:",put(domain,$form.));
  SUBJID   = strip(subjectid);
  
  
  /*--------
  add technical coding variables CMCODED CMINITD CMLTEDID
  -------*/
  %sdtm_dt2dtc(dt = CodedOnDate,dtc = CMCODED);
  %sdtm_dt2dtc(dt = InitiatedDate,dtc = CMINITD);
  %sdtm_dt2dtc(dt = LastEditedDate,dtc = CMLTEDID);
run;  

/* Design is applied and checks are made. */
%sdtm_create_domain_v2 ( domain      = &sdtm_domain.
                    , inset          = &sdtm_domain.
                    , keys           = studyid usubjid cmtrt cmstdtc cmendtc cmcoded cminitd cmltedid
                    , metavarsset    = metadata.sdtm_study_variables
                    , metadomainsset = metadata.sdtm_study_domains); 
  