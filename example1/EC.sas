/*******************************************************************************
                             Danone Nutricia Research
********************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : EC.sas
 PURPOSE       : Create SDTM data for Exposure as Collected
********************************************************************************
  Input       : ..\04 Import Data\ex.sas7bdat
                ..\04 Import Data\ex2.sas7bdat
                ..\04 Import Data\exi.sas7bdat
                ..\41 SDTM Data\dm.sas7bdat
  Output      : ..\41 SDTM data\ec.sas7bdat
********************************************************************************
  Modification history:     
  2025-12-05 - Radmila Ishchenko - Initial program
  2025-12-16 - Radmila Ishchenko - (1) ECENDY fixed; (2) ECDOSE(U) assigned to missing
  2025-12-17 - Radmila Ishchenko - RFXENDTC source is changed from SDTM.DM to IMPORT.EOS for traceability
  2025-12-18 - Radmila Ishchenko - ECPRESP='Y' added for Restart cases.
  2026-01-30 - Radmila Ishchenko - ECEVINTX should be "TODAY" for all records from EX2
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
%copy_import (inset = eos);

/*-----------------------------------------
Info from "Study Product Intake" CRF page
----------------------------------------*/
data ex_p1;
  set ex_1;
  length ECPRESP ECOCCUR ECREASOC ECSTAT ECREASND ECSTDTC ECEVAL ECORIG SOURCEID $200;
  ECPRESP  = "Y";
  if EXYN = 'Y' then ECOCCUR = 'Y';
  if EXYN = 'N' then do;
    ECOCCUR = 'N';
    ECREASOC = EXYNR;
  end;
  if EXYN = 'UNKNOWN' then do;
    ECSTAT = 'NOT DONE';
    ECREASND = EXYNR;
  end;
  %sdtm_dt2dtc(dt = EXSTDAT,dtc = ECSTDTC);
  ECEVAL   = "INVESTIGATOR";
  ECORIG   = "CRF";
  SOURCEID = catx(' ',"CRF:",put("EX",$form.));
run;

/*------------------------------------------------
Info from "Study Product Intake - Diary" CRF page
-------------------------------------------------*/
data ex_p2;
  set ex2_1;
  length ECPRESP ECOCCUR ECOCCUR ECSTDTC ECEVAL 
         ECORIG ECVPREPU ECVLFTU ECDOSU SOURCEID $200;
  array EX2_PR[7] EX2_PR1-EX2_PR7;
  array EX2_LO[7] EX2_LO1-EX2_LO7;
  ECPRESP  = "Y";
  ECEVINTX = "TODAY";
  if EX2_NDNA = "NOT APPLICABLE" then ECOCCUR ="N";
  if EX2_NX ne "" then ECOCCUR ="Y";
  %sdtm_dt2dtc(dt = EX2_STDAT,dtc = ECSTDTC);
  ECEVAL   = "PARENT";
  ECORIG   = "pPRO";
  SOURCEID = catx(' ',"CRF:",put("EX2",$form.));
  
  *create record for each of the servings;
  n_servings = input(scan(EX2_NX,1," "),best.); 
  do i = 1 to 7;
    if i le n_servings then do;
      ECSPID = strip(put(i,best.));
      ECVPREP = EX2_PR[i];
      ECVLFT  = EX2_LO[i];
      ECVPREPU = "mL";
      ECVLFTU  = "mL";
      output;
      call missing(ECVPREP,ECVLFT);
      call missing(ECVPREPU,ECVLFTU,ECDOSU,ECSPID);
    end;
  end;
  if ECOCCUR ="N" then output;
run;

/*------------------------------------------------
Info from "Study Product Interruptions" CRF page
-------------------------------------------------*/
data ex_p3;
  set exi_1(rename = (exspid=_exspid));
  length ECOCCUR ECSTDTC ECEVAL ECORIG InterruptReas SOURCEID ECPRESP $200;
  ECEVAL = "INVESTIGATOR";
  ECORIG = "CRF";
  SOURCEID = catx(' ',"CRF:",put("EXI",$form.));
  
  *last SP date before interruption;
  %sdtm_dt2dtc(dt = EXENDAT1,  dtc = DTbeforeINT);
  InterruptReas = EXSPSP;
  delRecord = 1; /*if there is no restart date then this record will be deleted later and only interruption date will be used*/
  
  *Re-start data;
  if EXSTARTYN = "Y" then do;
    ECPRESP = "Y";
    %sdtm_dt2dtc(dt = EXSTDAT1,  dtc = ECSTDTC);
    ECOCCUR ="Y" ;
    delRecord = 0;
  end;
run;

data ex_all1;
  set ex_p:;
  length ECTRT ECCAT STUDYID DOMAIN ECDOSFRM SUBJID $200;
  ECTRT    = "STUDY PRODUCT";
  ECCAT    = "STUDY PRODUCT";
  STUDYID  = "&studyid";
  DOMAIN   = "&sdtm_domain";
  ECDOSFRM = "POWDER, FOR SUSPENSION";
  SUBJID   = strip(subjectid);
  %derive_tpt_vars(eventid_var = ActivityID);
run;

proc sort data = ex_all1;
	by usubjid ECEVAL descending ECSTDTC;
run;

data ex_all2;
  set ex_all1;
  by usubjid ECEVAL descending ECSTDTC;
  retain _DTbeforeINT _InterruptReas;
  length _DTbeforeINT ECENDTC $10 _InterruptReas ECENREAS $200;
  if first.ECEVAL then _DTbeforeINT="";
  if ECEVAL = "INVESTIGATOR" then do;
    if _DTbeforeINT ne "" and ECENDTC = "" then do;
      ECENDTC = _DTbeforeINT;
      ECENREAS = _InterruptReas;
    end;
    _DTbeforeINT = DTbeforeINT;
    _InterruptReas = InterruptReas;
  end;
run;

data all3;
  merge ex_all2(in=inEx) sdtm.dm(keep = usubjid RFSTDTC) eos_1(keep=usubjid RFXENDAT);
  by usubjid;
  if inEx;
run;

proc sort data = all3;
	by usubjid ECEVAL ECSTDTC;
run;

data &sdtm_domain.;
  set all3;
  by usubjid ECEVAL ECSTDTC;
  %sdtm_dt2dtc(dt = RFXENDAT,  dtc = RFXENDTC);
  if ECEVAL = "INVESTIGATOR"  and last.ECEVAL then ECENDTC = RFXENDTC;
  %sdtm_dtc2dy(dtc= ecstdtc);
  %sdtm_dtc2dy(dtc= ecendtc);
  
  *delete record which contain only interruption date;
  if delRecord then delete;
run;

/* Design is applied and checks are made. */
%sdtm_create_domain_v2 ( domain      = &sdtm_domain.
                    , inset          = &sdtm_domain.
                    , keys           = usubjid ectrt eceval ecstdtc ectptref ectptnum ecspid
                    , ADDSUPPVARS    = ecevintx
                    , metavarsset    = metadata.sdtm_study_variables
                    , metadomainsset = metadata.sdtm_study_domains); 
