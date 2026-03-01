/*******************************************************************************
                             Danone Nutricia Research
********************************************************************************
STUDY/PROJECT:  SONA
PROGRAM PATH:   /SAS/SONA/Files/15 SDTM Conversion/01 Macros
MACRO NAME:     add_planned_actual_arm.sas
PURPOSE:        Macro to add actual and planned arm for OMENS
--------------------------------------------------------------------------------
NOTES:          SONA is a single arm open label study without randomisation
--------------------------------------------------------------------------------
PROGRAM HISTORY:
2025-10-06 - Vikas Shokeen - Initial Version
*******************************************************************************/

%macro add_planned_actual_arm (  inset  =
                               , outset =
                              );
    %local arm armcd;
    %let   armcd = ONS;
    %let   arm   = 4-week ONS Consumption (2 servings/day);
    
    %*-- all subjects continued into a next visit after screening;
    proc sql noprint;
      create table vstat_yes as
        select distinct usubjid, vstatyn as vstatyn_yes
        from vstat_1 
        where upcase(vstatyn) = "Y"
        order by usubjid;
    ;quit;

    data &outset.;
      length armcd
             arm
             actarmcd
             actarm
             armnrs $200;
      merge &inset    (in=_indm)
            vstat_yes (in=_invstat_yes);
      by usubjid;
      if _indm;
      
      %*-- initialise to missing values;
      call missing(arm,armcd, actarm, actarmcd, armnrs);

/* PLANNED ARM - ARMCD, ARMNRS */
      select;
        %* subject eligible or subject who continued into any assessment after screening;
        when ( upcase(ieyn) = "Y" or _invstat_yes ) do;
          arm   = "&arm.";
          armcd = "&armcd.";
          %* product not started -> ACTARM will be missing;
          if cmiss(rfxstdtc,rfxendtc) = 2 then armnrs = "ASSIGNED, NOT TREATED"; %* CT Code C142238, CT List: Arm Null Reason (C142179);
        end;  
        %* subject not eligible;
        when (upcase(ieyn) = "N") do;
          armnrs    = "SCREEN FAILURE"; %* CT Code C49628, CT List: Arm Null Reason (C142179);
        end;
        %* Eligble conclusion not entered yet;
        when (missing(ieyn)) armnrs = "NOT ASSIGNED"; %* CT Code C142239, CT List: Arm Null Reason (C142179);
      end;

/* ACTUAL ARM - ARMCD */
      select;
        %* start or stop date product not missing;
        when (cmiss(rfxstdtc,rfxendtc) ^= 2 ) do;
          actarm   = "&arm.";
          actarmcd = "&armcd.";
        end;  
        otherwise;
      end; 
      label armnrs = "Reason Arm and/or Actual Arm is Null" ;
   run;
   
%mend add_planned_actual_arm;

