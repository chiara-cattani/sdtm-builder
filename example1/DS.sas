/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : /SAS/NUTRI-IRON/Files/15 SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : DS.sas
 PURPOSE       : Create SDTM data for Disposition
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-03 - Kirill Novichkov - Initial version
 2025-12-08 - Kirill Novichkov - v1.1: Log was cleared
 2025-12-10 - Kirill Novichkov - v1.2: DSDECOD = "ELIGIBILITY CRITERIA NOT MET BUT ENROLLED" scenario added
 2025-12-12 - Kirill Novichkov - v1.3: DSTERM = "INFORMED CONSENT OBTAINED" case removed
 2025-12-15 - Kirill Novichkov - v1.4: DSTERM/DSDECOD = "ELIGIBILITY CRITERIA MET" fixed, 
                                  DSSTDTC updated for DSDECOD = "RANDOMIZED"
 2025-12-15 - Kirill Novichkov - v1.5: Log was cleaned
 2026-01-29 - Kirill Novichkov - v1.6: DSSTDTC source for DSCAT="OTHER EVENT" changed from ICSDAT to ICDAT
******************************************************************************/

proc datasets library=work kill nolist;
quit;

%nutricia_config;

%let domain = %unquote(&_progname);

%copy_import (inset=ic);
%copy_import (inset=ie);
%copy_import (inset=rand);
%copy_import (inset=eos);

* format to apply DSTERM part after '-' for informed consent details records (DSCAT=OTHER EVENT) ;
proc format;
  value $ic_other_dsterm
  "ICCON" = "FOR CONTACTING FOR FUTURE STUDIES"
  "ICIMGS" = "STOOL IMAGE TAKING FOR DIAPER WEARING CHILDREN"
  "ICIMG" = "DEVELOPMENT OF IMAGE RECOGNITION ALGORITHM"
  "ICFUT" = "ENCODED INFORMATION USED FOR FUTURE RESEARCH"
  "ICLTS" = "LEFTOVER SAMPLES AND IMAGES USED FOR FUTURE RESEARCH";
run;

* merge all raw datasets into a single one for further merge with SDTM.DM ;
data &domain.0;
  length source_ds $200;
  set ic_1 ie_1 rand_1 eos_1 indsname=_indsname;
  source_ds = tranwrd(scan(_indsname,2,'.'),'_1','');
run;

proc sort data=&domain.0 (rename=subjectid=subjid); 
  by subjid; 
run;
  
* derive SDTM variables + output records when conditions were met ;
data &domain.1; 
  length dsstdat dsterm dsdecod dscat sourceid $200;
  merge &domain.0 (in=a drop=usubjid randnum)
        sdtm.dm (keep=subjid studyid usubjid rfstdtc randnum);
  by subjid;
  if a;
  
  sourceid = catx(' ', "CRF:", put(source_ds, $form.));
  
  if source_ds = 'IC' then do;
    dscat = "PROTOCOL MILESTONE";
    dsdecod = "INFORMED CONSENT OBTAINED";
    if ^missing(icdat1) then do;
      dsterm = "INFORMED CONSENT OBTAINED - " !! icby1;
      dsstdat = icdat1;
      output;
    end;
    if ^missing(icdat2) then do;
      dsterm = "INFORMED CONSENT OBTAINED - " !! icby2;
      dsstdat = icdat2;
      output;
    end;
    
    dscat = "OTHER EVENT";
    array icynvar [5] iccon icimgs icimg icfut iclts;
    do i=1 to 5;
      if ^missing(icynvar[i]) then do;
        if icynvar[i] = "Y" then dsdecod = "INFORMED CONSENT OBTAINED";
        if icynvar[i] = "N" then dsdecod = "INFORMED CONSENT DECLINED";
        dsterm = catx(" - ", dsdecod, put(upcase(vname(icynvar[i])), $ic_other_dsterm.));
        dsstdat = icdat;
        output;
      end;
    end;
  end;
  
  dscat = "PROTOCOL MILESTONE"; 
  if source_ds = "IE" and ^missing(ieyn) then do;
    if ieyn="N" and ^missing(randnum) then dsdecod = "ELIGIBILITY CRITERIA NOT MET BUT ENROLLED";
    else dsdecod = "ELIGIBILITY CRITERIA" !! ifc(ieyn="N", " NOT ", " ") !! "MET";
    dsterm = dsdecod;
    dsstdat = eventdate;
    output;
  end;
  
  if source_ds = "RAND" then do;
    dsdecod = "RANDOMIZED";
    dsterm = dsdecod;
    dsstdat = randdat;
    output;
  end;

  if source_ds = "EOS" then do;
    dscat = "DISPOSITION EVENT"; 
    if complyn = "Y" then do;
      dsdecod = "COMPLETED";
      dsstdat = eventdate;
    end;
    if complyn = "N" then do;
      dsdecod = etstat;
      dsstdat = etdat;
    end;
    dsterm = ifc(^missing(etsp), etsp, dsdecod);
    output;
  end;
run;

data &domain.; 
  set &domain.1; 
  
  domain = "&domain";
  %sdtm_dt2dtc(dt=dsstdat, dtc=dsstdtc);
  %sdtm_dtc2dy(dtc=dsstdtc);
run;

* Get the list of key variables from the metadata ;
proc sql noprint;  
  select tranwrd(compress(keys), ',', ' ') into: keyvars from metadata.sdtm_study_domains where domain="&domain";
quit;

* Apply metadata and store into SDTM folder ;
%sdtm_create_domain_v2 ( domain           = &domain.
                        , inset           = &domain.
                        , keys            = &keyvars.
                        , deldupkeys      = N
                        , metavarsset     = metadata.sdtm_study_variables
                        , metadomainsset  = metadata.sdtm_study_domains);