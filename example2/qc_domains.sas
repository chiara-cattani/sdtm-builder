/*/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : SONA / 23REX0061265
 PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/70 QC
 PROGRAM NAME  : run_all_qc.sas
 PURPOSE       : Automatically detect all SDTM domains in 41 SDTM Data folder
                 and run QC comparison for each using %compare_qc.
 ------------------------------------------------------------------------------
 NOTES :
   * Requires macro %compare_qc to be available in the SAS session.
   * Assumes SDTM datasets are stored as .sas7bdat files in the specified folder.
   * Assumes QC datasets are stored as .xpt files in the corresponding /70 QC/XPT folder.
   * Uses %xpt2loc to import XPT files before comparison.
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-10-06 - cattanch - Initial program 
******************************************************************************/

/* Configuration */
%nutricia_config;

/* QC a specific domain */
%let domain_to_qc = relrec;
%compare_qc(domain=&domain_to_qc.
/* 			, cmp_options= criterion=1e-7 method=absolute */
			, visible=Y);

/* QC all the domains present in 41 SDTM Data */
/* %run_all_qc; */
