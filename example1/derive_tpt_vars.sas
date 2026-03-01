/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : /SAS/NUTRI-IRON/Files/15 SDTM Conversion/01 Macros/
 MACRO NAME   :  derive_tpt_vars.sas
 PURPOSE      :  Macro to add timepoints in Diary data
 ------------------------------------------------------------------------------
NOTES:          The macro is supposed to be executed during a data step
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-03 - Radmila Ishchenko - Initial program
 2025-12-15 - Radmila Ishchenko - Parameter 'domain' added, TPTREF text updated.
******************************************************************************/

%macro derive_tpt_vars(domain = &sdtm_domain., eventid_var = ActivityID);
	length &domain.TPT &domain.TPTREF &domain.ELTM $200 &domain.TPTNUM 8.;
    %do i = 1 %to 7;
      if ActivityID in ("D&i.W1_SPI" , "GI_W1D&i." ) then do; &domain.TPTREF = "Week after Visit 1"             ; &domain.TPT = "Day &i."; &domain.TPTNUM = &i.; &domain.ELTM = "P&i.D"; end;
      if ActivityID in ("D&i.W5_SPI" , "GI_W5D&i." ) then do; &domain.TPTREF = "Week before Phone Call 2"       ; &domain.TPT = "Day &i."; &domain.TPTNUM = &i.; &domain.ELTM = "P&i.D"; end;
      if ActivityID in ("D&i.W10_SPI", "GI_W10D&i.") then do; &domain.TPTREF = "Week before Visit 2"            ; &domain.TPT = "Day &i."; &domain.TPTNUM = &i.; &domain.ELTM = "P&i.D"; end;
      if ActivityID in ("D&i.W15_SPI", "GI_W15D&i.") then do; &domain.TPTREF = "Week before Phone Call 3"       ; &domain.TPT = "Day &i."; &domain.TPTNUM = &i.; &domain.ELTM = "P&i.D"; end;
      if ActivityID in ("D&i.W20_SPI", "GI_W20D&i.") then do; &domain.TPTREF = "Week before Visit 3"            ; &domain.TPT = "Day &i."; &domain.TPTNUM = &i.; &domain.ELTM = "P&i.D"; end;
    %end;
%mend derive_tpt_vars;