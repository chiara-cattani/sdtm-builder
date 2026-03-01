/*******************************************************************************
                             Danone Nutricia Research
********************************************************************************
STUDY:          SONA
PROGRAM PATH:   /15 SDTM Conversion/01 Macros
PROGRAM NAME:   derive_visit_vars.sas
PURPOSE:        Macro to derive variables VISIT VISITNUM 
--------------------------------------------------------------------------------
NOTES:          The macro is supposed to be executed during a data step
--------------------------------------------------------------------------------
PROGRAM HISTORY:
2025-10-06 - Initial program
********************************************************************************/
%macro derive_visit_vars(eventid_var = eventid);
  length visit $200 visitnum 8.;
  if &eventid_var. = "SCR" then do;
    visitnum = 1;
    visit = "Screening";
  end; 
  else if &eventid_var. = "V1" then do;
    visitnum = 2;
    visit = "Visit 1";
  end;  
  else if &eventid_var. = "V2" then do;
    visitnum = 3;
    visit = "Visit 2";
  end;  
  else if &eventid_var. = "V3" then do;
    visitnum = 4;
    visit = "Visit 3";
  end;  
  else if &eventid_var. = "V4"  then do;
    visitnum = 5;
    visit = "Visit 4";
  end;
  else if substr(&eventid_var., 1, 2) = "ED" then call missing(visitnum, visit);
  else  put '###WAR' 'NING: Unexpected value of ' "&eventid_var" ' Review data';

%mend derive_visit_vars;