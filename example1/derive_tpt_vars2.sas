/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : SONA / 23REX0061265
 PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
 PROGRAM NAME  : derive_tpt_vars.sas
 PURPOSE       : Macro to derive variables --TPT --TPTNUM
 ------------------------------------------------------------------------------
 NOTES : The macro is supposed to be executed during a data step
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-10-08 - cattanch - Initial program
******************************************************************************/

%macro derive_tpt_vars(eventid_var = eventid, sdtm = );

  length &sdtm.tpt $200 &sdtm.tptnum 8 &sdtm.tptref $200 &sdtm.eltm $20;

  select (&eventid_var);
    when ("BL1") do;
      &sdtm.tpt    = "Day -3";
      &sdtm.tptnum = -3;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "-P3D";
    end;
    when ("BL2") do;
      &sdtm.tpt    = "Day -2";
      &sdtm.tptnum = -2;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "-P2D";
    end;
    when ("BL3") do;
      &sdtm.tpt    = "Day -1";
      &sdtm.tptnum = -1;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "-P1D";
    end;
    when ("A1A") do;
      &sdtm.tpt    = "Day 7";
      &sdtm.tptnum = 7;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "P7D";
    end;
    when ("A1B") do;
      &sdtm.tpt    = "Day 8";
      &sdtm.tptnum = 8;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "P8D";
    end;
    when ("A1C") do;
      &sdtm.tpt    = "Day 9";
      &sdtm.tptnum = 9;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "P9D";
    end;
    when ("A2A") do;
      &sdtm.tpt    = "Day 14";
      &sdtm.tptnum = 14;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "P14D";
    end;
    when ("A2B") do;
      &sdtm.tpt    = "Day 15";
      &sdtm.tptnum = 15;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "P15D";
    end;
    when ("A2C") do;
      &sdtm.tpt    = "Day 16";
      &sdtm.tptnum = 16;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "P16D";
    end;
    when ("A3A") do;
      &sdtm.tpt    = "Day 26";
      &sdtm.tptnum = 26;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "P26D";
    end;
    when ("A3B") do;
      &sdtm.tpt    = "Day 27";
      &sdtm.tptnum = 27;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "P27D";
    end;
    when ("A3C") do;
      &sdtm.tpt    = "Day 28";
      &sdtm.tptnum = 28;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "P28D";
    end;
    when ("A4A") do;
      &sdtm.tpt    = "Day 39";
      &sdtm.tptnum = 39;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "P39D";
    end;
    when ("A4B") do;
      &sdtm.tpt    = "Day 40";
      &sdtm.tptnum = 40;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "P40D";
    end;
    when ("A4C") do;
      &sdtm.tpt    = "Day 41";
      &sdtm.tptnum = 41;
      &sdtm.tptref = "Visit 1";
      &sdtm.eltm   = "P41D";
    end;
    otherwise do;
      put "###WARNING: Unexpected value of " &eventid_var=;
      call missing(&sdtm.tpt, &sdtm.tptnum, &sdtm.tptref, &sdtm.eltm);
    end;
  end;

%mend derive_tpt_vars;