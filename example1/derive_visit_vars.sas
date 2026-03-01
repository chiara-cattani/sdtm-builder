/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 MACRO NAME   :  add_planned_actual_arm.sas
 PURPOSE      :  Macro to add actual and planned arm for YOFLOW
 ------------------------------------------------------------------------------
NOTES:          The macro is supposed to be executed during a data step
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-02 - Chiara Cattani - Initial program (copied from YOFLOW and adjusted)
 2026-01-27 - Chiara Cattani - Updated VISIT.
 2026-02-02 - Radmila Ishchenko - EventId's V1_IMG->V1, V3_IMG->V3 added
 2026-02-02 - Chiara Cattani - Added VISITDY.
******************************************************************************/

%macro derive_visit_vars(eventid_var = eventid);
  length visit $200 visitnum visitdy 8.;
  if &eventid_var = "SCR" then do;
    visitnum = 1;
    visit = "Visit 0: Screening";
    visitdy = 1;
  end; 
  else if &eventid_var in ("V1" "V1_IMG") then do;
    visitnum = 2;
    visit = "Visit 1";
    visitdy = 1;    
  end; 
  else if &eventid_var = "PW1" then do;
    visitnum = 3;
    visit = "Phone Call 1";
    visitdy = 8;
  end; 
  else if &eventid_var = "PW5" then do;
    visitnum = 4;
    visit = "Phone Call 2";
    visitdy = 36;
  end; 
  else if &eventid_var ="V2" then do;
    visitnum = 5;
    visit = "Visit 2";
    visitdy = 71;
  end; 
  else if &eventid_var = "PW15" then do;
    visitnum = 6;
    visit = "Phone Call 3";
    visitdy = 106;
  end; 
  else if &eventid_var = "PW20" then do;
    visitnum = 7;
    visit = "Phone Call 4";
    visitdy = 140;
  end; 
  else if &eventid_var in ("V3" "V3_IMG") then do;
    visitnum = 8;
    visit = "Visit 3";
    visitdy = 141;
  end;
  else if &eventid_var = "PFU" then do;
    visitnum = 9;
    visit = "Phone Call 5: Follow-up";
    visitdy = 155;
  end; 
  else call missing(visitnum, visit, visitdy);
  
%mend derive_visit_vars;