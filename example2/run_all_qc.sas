/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : SONA / 23REX0061265
 PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
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

/* Build dynamic list of domains from the SDTM library */
proc sql noprint;
  select memname
  into :domain_list separated by ' '
  from dictionary.tables
  where libname='SDTM';
quit;

%put NOTE: Domains detected in SDTM: &domain_list;

/* Loop through detected domains */
%macro run_all_qc;
  %local i dom;
  %do i=1 %to %sysfunc(countw(&domain_list));
    %let dom = %scan(&domain_list,&i);
    %put NOTE: === Running QC compare for &dom ===;
    %compare_qc(domain=&dom, sdtm_lib=sdtm);
  %end;
%mend;