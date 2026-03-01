/*******************************************************************************
                             Danone Nutricia Research
********************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : EX.sas
 PURPOSE       : Create SDTM data for Exposure
********************************************************************************
  Input       : ..\04 Import Data\ex.sas7bdat
                ..\04 Import Data\ex2.sas7bdat
                ..\04 Import Data\exi.sas7bdat
                ..\41 SDTM Data\dm.sas7bdat
  Output      : ..\41 SDTM data\ec.sas7bdat
********************************************************************************
  Modification history:     
  2025-12-15 - Radmila Ishchenko - Initial program
  2025-01-09 - Radmila Ishchenko - Labels for EXEVAL, EXORIG are added and EXENDTC added for Diary records
  2025-01-09 - Radmila Ishchenko - EXENDY fixed                                  
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
%copy_import (inset = ex);
%copy_import (inset = ex2);
%copy_import (inset = exi);

/*-----------------------------------------
Info from "Study Product Intake" CRF page
----------------------------------------*/
data ex_p1;
  set ex_1(where = (EXSTDAT ne ''));
  length EXSTDTC EXEVAL EXORIG SOURCEID $200;
  %sdtm_dt2dtc(dt = EXSTDAT,dtc = EXSTDTC);
  EXEVAL   = "INVESTIGATOR";
  EXORIG   = "CRF";
  SOURCEID = catx(' ',"CRF:",put("EX",$form.));
run;

/*------------------------------------------------
Info from "Study Product Intake - Diary" CRF page
-------------------------------------------------*/
data ex_p2;
  set ex2_1(where = (EX2_STDAT ne ""));
  length EXDOSU EXEVAL EXORIG EXDOSFRQ SOURCEID $200;
  array EX2_PR[7] EX2_PR1-EX2_PR7;
  array EX2_LO[7] EX2_LO1-EX2_LO7;
  EXDOSE = 0;
  do i = 1 to 7;
    if EX2_PR[i] = . then EX2_PR[i] = 0;
    if EX2_LO[i] = . then EX2_LO[i] = 0;
    if EX2_PR[i] < EX2_LO[i]  then put "##" "#DQC:: volume prepared < volume left for " usubjid = EX2_STDAT = "serving " i =; 
    EXDOSE = EXDOSE + EX2_PR[i] - EX2_LO[i];
  end;
  EXDOSU="mL";
  %sdtm_dt2dtc(dt = EX2_STDAT,dtc = EXSTDTC);
  %sdtm_dt2dtc(dt = EX2_STDAT,dtc = EXENDTC);
  EXEVAL   = "PARENT";
  EXORIG   = "pPRO";
  EXDOSFRQ = "QD";
  SOURCEID = catx(' ',"CRF:",put("EX2",$form.));
  %derive_tpt_vars(eventid_var = ActivityID);
run;

/*------------------------------------------------
Info from "Study Product Interruptions" CRF page
-------------------------------------------------*/
data ex_p3;
  set exi_1(rename = (exspid=_exspid));
  length EXSTDTC EXEVAL EXORIG SOURCEID $200;
  EXEVAL = "INVESTIGATOR";
  EXORIG = "CRF";
  SOURCEID = catx(' ',"CRF:",put("EXI",$form.));
  
  *last SP date before interruption;
  %sdtm_dt2dtc(dt = EXENDAT1,  dtc = DTbeforeINT);
  delRecord = 1; /*if there is no restart date then this record will be deleted later and only interruption date will be used*/
  
  *Re-start data;
  if EXSTARTYN = "Y" then do;
    %sdtm_dt2dtc(dt = EXSTDAT1,  dtc = EXSTDTC);
    delRecord = 0;
  end;
run;

data ex_all1;
  set ex_p:;
  length EXTRT EXCAT STUDYID DOMAIN EXDOSFRM SUBJID $200;
  EXTRT    = "Blinded";
  EXCAT    = "STUDY PRODUCT";
  STUDYID  = "&studyid";
  DOMAIN   = "&sdtm_domain";
  EXDOSFRM = "POWDER, FOR SUSPENSION";
  SUBJID   = strip(subjectid);
run;

proc sort data = ex_all1;
	by usubjid EXEVAL descending EXSTDTC;
run;

data ex_all2;
  set ex_all1;
  by usubjid EXEVAL descending EXSTDTC;
  retain _DTbeforeINT;
  length _DTbeforeINT $10 ;
  if first.EXEVAL then _DTbeforeINT="";
  if EXEVAL = "INVESTIGATOR" then do;
    if _DTbeforeINT ne "" and EXENDTC = "" then EXENDTC = _DTbeforeINT;
    _DTbeforeINT = DTbeforeINT;
  end;
run;

data all3;
  merge ex_all2(in=inEx) sdtm.dm(keep = usubjid RFSTDTC RFXENDTC);
  by usubjid;
  if inEx;
  
  *delete record which contain only interruption date;
  if delRecord then delete;
run;

proc sort data = all3;
	by usubjid EXEVAL EXSTDTC;
run;

data &sdtm_domain.;
	set all3;
	by usubjid EXEVAL EXSTDTC;
	if EXEVAL = "INVESTIGATOR"  and last.EXEVAL then EXENDTC = RFXENDTC;
    %sdtm_dtc2dy(dtc= EXstdtc);
    %sdtm_dtc2dy(dtc= EXendtc);
	label EXORIG = "Origin"
	      EXEVAL = "Evaluator"
	      ;
run;

/* Design is applied and checks are made. */
%sdtm_create_domain_v2 ( domain      = &sdtm_domain.
                    , inset          = &sdtm_domain.
                    , keys           = usubjid EXtrt EXeval EXstdtc extptref extptnum 
                    , ADDSUPPVARS    = EXEVAL EXORIG
                    , metavarsset    = metadata.sdtm_study_variables
                    , metadomainsset = metadata.sdtm_study_domains); 

