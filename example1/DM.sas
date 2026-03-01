/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : DM.sas
 PURPOSE       : Create SDTM data for Demographics
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-01 - Radmila Ishchenko - Initial program (copied from SONA and adjusted)
 2025-12-16 - Radmila Ishchenko - ETHNIC source variable has been updated
 2025-12-18 - Chiara Cattani    - Minor: Use CETHNIC instead of ETHNIC.
 2026-01-27 - Chiara Cattani    - Minor: drop ETHNICC
******************************************************************************/

/* Configuration */
%nutricia_config;

%let sdtm_domain = %unquote(&_progname);

/*****************************************************************************
  - Use macro %copy import to copy the raw dataset to the work folder,
    with name work.&inset_1
  - Creates USUBJID and removes formats and informats
  - by = can be used to sort, default is USUBJID
*****************************************************************************/
%copy_import (inset=dm);
%copy_import (inset=eos, by= usubjid lastediteddate);
%copy_import (inset=ic);
%copy_import (inset=ie);
%copy_import (inset=ex);
%copy_import (inset=ae);
%copy_import (inset=sae);
%copy_import (inset=rand);
%copy_import (inset=vstat); %* used in macro %add_planned_actual_arm;


/* In case multiple EOS records are reported, only take the latest and write a war ning to the log */
data work.eos1;
  set work.eos_1;
  by usubjid lastediteddate;
  if last.usubjid ne first.usubjid then multirec = "Y";
  if last.usubjid;

  if multirec = "Y" then do;
    put "###WAR" "NING: subject = " usubjid " has more than one record, record with latest LastEditedDate is kept" ;
  end;
  drop multirec;
run;

/* Get first EXSTDAT per subject */
proc sql;  
  create table ex_first as
  select usubjid,
         min(exstdat) as ex_rfxstdtc length=20
  from work.ex_1
  where not missing(exstdat)
  group by usubjid
  ;

/* Merge first study product intake data and DM */ 
  create table work.ex as
  select  ex.usubjid,
          ex.ex_rfxstdtc
  from ex_first as ex;
quit;


/* Merge all datasets */
data work.demographics1;
  merge work.dm_1(keep=usubjid sitecode subjectid sex age ageu ethnicc brthdat in=inDM)
        work.eos1(keep = usubjid complyn lcdat rfxendat etdat eventdate rename = (eventdate = eos_eventdate))
        work.ic_1(keep = usubjid icdat) /*for RFICDTC RFSTDTC*/
        work.ae_1(keep =  usubjid aeout where = (upcase(aeout) = "FATAL")) /*For DTH flag*/
        work.sae_1(keep = usubjid aesdth aesdtdat)  /*for DTH flag and date*/
        work.ex;        
  by usubjid;
  if inDM;
run;

/* MAPPING IMPLEMENTATION */
data work.demographics2;
  length STUDYID DOMAIN USUBJID SUBJID 
         RFSTDTC RFENDTC RFXSTDTC RFXENDTC RFICDTC RFPENDTC 
         DTHDTC DTHFL SITEID BRTHDTC AGEU SEX RACE CETHNIC COUNTRY SOURCEID  $200
         age 8.;         
  set work.demographics1;
  STUDYID   = "&studyid.";
  DOMAIN    = "&sdtm_domain.";
  USUBJID   = usubjid;
  SUBJID    = strip(subjectid);
  
  /*   RFSTDTC RFENDTC */
  %sdtm_dt2dtc (dt=icdat, dtc=rfstdtc);
  if lowcase(complyn) = "y" and ^missing(eos_eventdate) then do; 
    %sdtm_dt2dtc (dt=eos_eventdate, dtc=rfendtc); 
  end;
  else if ^missing(etdat) and lowcase(complyn) = "n" then do;
    %sdtm_dt2dtc (dt=etdat, dtc=rfendtc);   /* In case of early termination */ 
  end;
  else call missing(rfendtc);
  
  /*   RFXSTDTC RFXENDTC RFICDTC RFPENDTC */
  %sdtm_dt2dtc (dt=ex_rfxstdtc, dtc=rfxstdtc);
  %sdtm_dt2dtc (dt=rfxendat, dtc=rfxendtc);
  %sdtm_dt2dtc (dt=icdat, dtc=rficdtc);
  %sdtm_dt2dtc (dt=lcdat, dtc=rfpendtc);
  
  /* DTHDTC DTHFL */
  DTHDTC    = aesdtdat;
  if lowcase(aeout) = "fatal" or aesdth eq 'Y' or aesdtdat ne '' then dthfl = "Y";
  
  SITEID    = catx("-", "&studyid",sitecode);
  BRTHDTC   = brthdat;
  AGE       = age;
  AGEU       = ifc(age ne . , ageu, "");
  SEX       = strip(sex);
  RACE      = ""; /*race is not collected for the study, but required variable*/
  CETHNIC   = strip(ethnicc);
  COUNTRY   = strip(scan(subjectid, 1, "-"));
  SOURCEID  = catx(' ',"CRF:",put(domain,$form.));
  drop ethnicc;
run;

/* Macro to add ARM(CD) ACTARM(CD)*/ 
%add_planned_actual_arm (inset=work.demographics2, outset=work.demographics3);
                
data &sdtm_domain.;
  set work.demographics3;
  by usubjid;
  /* If there are multiple USUBJID records, keep the first and issue a war ning */
  if not (first.usubjid and last.usubjid) then do;
    putlog "###WAR" "NING: USUBJID " USUBJID " is not unique, only the first observation kept.";
    if not first.usubjid then delete;
  end;
  label CETHNIC = "Collected Ethnicity";
run;

/* Design is applied and checks are made. */
%sdtm_create_domain_v2 ( domain      = &sdtm_domain.
                    , inset          = &sdtm_domain.
                    , keys           = studyid usubjid
                    , deldupkeys     = Y
                    , addsuppvars    = CETHNIC
                    , metavarsset    = metadata.sdtm_study_variables
                    , metadomainsset = metadata.sdtm_study_domains);