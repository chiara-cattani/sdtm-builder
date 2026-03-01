/*******************************************************************************
                             Danone Nutricia Research
********************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
MACRO NAME   :  add_planned_actual_arm.sas
PURPOSE      :  Macro to add actual and planned arm for YOFLOW
--------------------------------------------------------------------------------
NOTES:          Nutri-Iron is double blinded study with 2 arms
--------------------------------------------------------------------------------
PROGRAM HISTORY:
2025-12-01  - ishchera - Initial program (copied from YoFlow)
2026-02-09 - cattanch - Add filter to evaluate SCREEN FAILURE only at Screening.
*******************************************************************************/

%macro add_planned_actual_arm (  inset  =
                               , outset =
                              );

  
  %*-- assign arm before unblinding, update after unblinding;
  data arms1(keep = randnum arm: actarm:);
    set rand_1;
    length armcd actarmcd arm actarm $8;
    ARMCD = "TOTAL";
    ACTARMCD = "TOTAL";
    ARM = "Total";
    ACTARM = "Total";
  run;
  proc sort nodupkey data = arms1 out = arms2 dupout = duparms;
    by randnum;
  run;
 
  data _null_;
    set duparms;
    put "WAR" "NING: The same RANDNUM repeated more than once " (randnum) (=);
  run; 
    
  %*-- all randomized subjects with a randomisation number. Dataset rand_1 is created with macro %copy_import in DM program;
  data rand_2;
    set rand_1(where=(randnum ne '')) ;
    by usubjid;
    if not (first.usubjid and last.usubjid) then putlog '###DQC More than 1 randomisation record ' (usubjid randnum randdat) (=);
    if first.usubjid;
  run; 
  
  proc sort data = rand_2;
    by randnum;
  run;

  data arms;
    merge rand_2 arms2(in=in1);
    by randnum;
    if in1;
  run;

  
  %*-- Eligible Y/N after screening. Dataset ie_1 is created with macro %copy_import in DM program;
  proc sort data = ie_1;
    by usubjid ActivityId;
  run;
  
  data ie_2;
    set ie_1;
    by usubjid ActivityId eventid;
    retain ieyn1;
    if first.usubjid then ieyn1 = ieyn;
    if first.usubjid ne last.usubjid and ieyn1 = 'N' then put "WAR" "NING: Screening criteria was failed but V1 IE check was performed" (usubjid) (=);
    if strip(eventid) = "SCR" and last.usubjid then output;
  run;

  
  proc sql noprint;
    create table vstat_2 as
      select distinct usubjid, vstatyn as enrolled
      from vstat_1
      where vstatyn = "Y"
      order by usubjid;
  ;quit;
    
  %*-- Merge &inset with Randomisation and enrolled dataset. Input is sorted by USUBJID and must contain the variables: RFXSTDTC and RFXENDTC;
  data &outset.;
    length actarmcd actarm armnrs actarmud $200;
    merge &inset    (in=_indm)
          arms      (keep = usubjid arm: actarm: randnum)
          ie_2      (keep = usubjid ieyn)
          vstat_2 ;
    by usubjid;
    if _indm;

    if rfxstdtc = '' then call missing(actarm, actarmcd);
    
    %*-- initialise actarmcd, actarm, armnrs and actarmud to missing values. Variables arm and armcd are already filled in dataset rand_2;
    call missing(armnrs, actarmud);

    %*PLANNED ARM - ARMCD, ARMNRS;
    select;
      %*-- Subject randomized, ARM and ARMCD are already filled, 4 out of 10 possibilities done;
      when (not missing (armcd));  
      
      %*-- Subject not randomized + product intake, 6 out of 10 possibilities done;
      when (cmiss(rfxstdtc,rfxendtc) ^= 2) armnrs = "UNPLANNED TREATMENT"; %* CT Code  C142240: Arm Null Reason (C142179);
      
      %*-- Subject enrolled or subject eligible, 8 out of 10 possibilities done;
      when (upcase(enrolled) = 'Y' or upcase(ieyn) = "Y") armnrs = "NOT ASSIGNED"; %* CT Code C142239, CT List: Arm Null Reason (C142179);
      
      %*-- Subject not eligible, 9 out of 10 possibilities done;
      when (upcase(ieyn) = "N") armnrs = "SCREEN FAILURE"; %* CT Code C49628, CT List: Arm Null Reason (C142179);
      
      %*-- Eligibility conclusion not entered yet, 10 out of 10 possibilities done;
      when (missing(ieyn)) armnrs = 'NOT SCREENED YET';
      
      *-- unforeseen;
      otherwise putlog 'WAR' 'NING: Something went wrong for ARM(CD), check input datasets'  (usubjid armcd ieyn rfxstdtc rfxendtc enrolled) (=);
    end;
    
    %* ACTUAL ARM - ARMCD ;
    select;
      %*-- Actual treatement already filled in, in case of a reference group in a study;
      when (not missing(actarm));
      
      %*-- Product intake ;
      when (cmiss(rfxstdtc,rfxendtc) ^= 2) do;
        if not missing(armcd) then do;
          arm   = arm;
        end;
        else do;
           %*-- issue a warning to set ACTARMUD;
           actarmud = "Treatment unknown";
           put "WAR" "NING: UNPLANNED TREATMENT, update value for ACTARMUD" usubjid=;
        end;          
      end;            
      
      %*-- Planned treatment and no product intake;
      when (not missing(armcd)) armnrs = "ASSIGNED, NOT TREATED"; %* CT Code C142238, CT List: Arm Null Reason (C142179);
      
      %*-- No planned and actual treatment, AMNRS must be already be set;
      when (not missing(armnrs));
      
      *-- unforeseen;
      otherwise putlog 'WAR' 'NING: Something went wrong for ACTARM(CD), check input datasets '  (usubjid armcd actarmcd armnrs rfxstdtc rfxendtc enrolled) (=);
    end;
    label armnrs   = "Reason Arm and/or Actual Arm is Null" 
          actarmud = "Description of Unplanned Actual Arm";
  run;
   
%mend add_planned_actual_arm;

