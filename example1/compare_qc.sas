/******************************************************************************
Danone Nutricia Research
*******************************************************************************
STUDY/PROJECT : SONA / 23REX0061265
PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
PROGRAM NAME  : compare_qc.sas
PURPOSE       : Import QC XPT and compare to SDTM dataset; if differences exist,
               generate a PDF report with the issues. Optionally print a
               visible PROC COMPARE table even when no differences.
------------------------------------------------------------------------------
NOTES :
* Derives QC XPT path from the SDTM lib path (…/41 SDTM Data → …/70 QC/XPT). 
* Uses %xpt2loc2 to read XPT into WORK. 
* First pass is NOPRINT to set &SYSINFO; PDF is created only if &SYSINFO>0 or forced. 
* Optional ID variables help align rows (e.g., USUBJID --SEQ). 
* Optional VISIBLE listing mirrors manual PROC COMPARE output. 
* PDF goes to <qc_root>/10 QC Reports/compare_qc_<DOMAIN>_<YYYYMMDD>_<HHMMSS>.pdf 
* SAS7BDAT file saved to <qc_root>/SAS7BDAT/qc_<domain>.sas7bdat if requested. 
------------------------------------------------------------------------------
PROGRAM HISTORY :
2025-10-06 - cattanch - Initial program
2025-10-17 - cattanch - Added saving of sas7bdat.
******************************************************************************/

%macro compare_qc(
   domain=             
 , sdtm_lib=sdtm       
 , idvars=             
 , qc_root=            
 , xpt_subdir=XPT      
 , cmp_options=listall 
 , pdf=Y               
 , pdf_dir=            
 , visible=Y           
 , save_sas7bdat=Y     
);

 %local DOMAIN domain_lc sdtm_root sdtm_root_n qc_root_eff xpt_path cmp_ds
        _cmp_sysinfo _date _time _pdfdir _pdfpath _mkdir_rc _pdf_name _dir_ok
        nobs_base nobs_cmp nvars_base nvars_cmp;

 %let DOMAIN    = %upcase(&domain);
 %let domain_lc = %lowcase(&domain);

 %let sdtm_root   = %sysfunc(pathname(&sdtm_lib));
 %let sdtm_root_n = %sysfunc(translate(&sdtm_root,%str(/),%str(\)));

 %if %sysevalf(%superq(qc_root)=, boolean) %then %do;
   %let qc_root_eff = %sysfunc(tranwrd(&sdtm_root_n,%str(/41 SDTM Data),%str(/70 QC)));
 %end;
 %else %do;
   %let qc_root_eff = %sysfunc(translate(&qc_root,%str(/),%str(\)));
 %end;

 %let xpt_path = &qc_root_eff./&xpt_subdir./qc_&domain_lc..xpt;

 %if %sysfunc(exist(&sdtm_lib..&DOMAIN))=0 %then %do;
   %put ERROR: [compare_qc] Dataset not found -> &sdtm_lib..&DOMAIN (domain=&DOMAIN). Skipping.;
   %return;
 %end;

 filename _chk "&xpt_path";
 %if %sysfunc(fexist(_chk))=0 %then %do;
   %put WARNING: [compare_qc] QC XPT not found -> &xpt_path . Skipping &DOMAIN.;
   filename _chk clear;
   %return;
 %end;
 filename _chk clear;

 %put NOTE: [compare_qc] BASE   = &sdtm_lib..&DOMAIN ;
 %put NOTE: [compare_qc] XPT    = &xpt_path ;

 filename xptfile "&xpt_path";
 %xpt2loc2(filespec=xptfile, libref=work, memlist=&DOMAIN);

 %let cmp_ds = work.&domain_lc;
 %if %sysfunc(exist(&cmp_ds))=0 %then %let cmp_ds = work.&DOMAIN;
 %if %sysfunc(exist(&cmp_ds))=0 %then %do;
   %put ERROR: [compare_qc] Expected dataset WORK.&domain_lc or WORK.&DOMAIN not found after XPT import.;
   proc datasets lib=work nolist; contents data=_all_; quit;
   filename xptfile clear;
   %return;
 %end;

 %put NOTE: [compare_qc] COMPARE= &cmp_ds ;
 
/* Save imported QC dataset as .sas7bdat in SAS7BDAT folder */

%let sas7bdat_dir = &qc_root_eff./SAS7BDAT;
%let sas7bdat_lib = sasqc; /* must be <=8 chars */

%macro save_qc_sas7bdat;
  /* Ensure folder exists */
  %if %sysfunc(fileexist(&sas7bdat_dir)) = 0 %then %do;
    %let _sas7bdat_mkdir_rc = %sysfunc(dcreate(SAS7BDAT, %substr(&sas7bdat_dir, 1, %length(&sas7bdat_dir)-9)));
  %end;

  /* Clear libref if already assigned */
  %if %sysfunc(libref(&sas7bdat_lib)) = 0 %then %do;
    %let _clear_rc = %sysfunc(libname(&sas7bdat_lib));
  %end;

  /* Assign libref */
  %let _librc = %sysfunc(libname(&sas7bdat_lib, &sas7bdat_dir));

  %if &_librc = 0 %then %do;
    /* Delete existing dataset if present */
    %if %sysfunc(exist(&sas7bdat_lib..qc_&domain_lc)) %then %do;
      proc datasets lib=&sas7bdat_lib nolist;
        delete qc_&domain_lc;
      quit;
    %end;

    /* Save dataset */
    data &sas7bdat_lib..qc_&domain_lc;
      set &cmp_ds;
    run;

    %put NOTE: [compare_qc] Saved QC dataset to &sas7bdat_dir/qc_&domain_lc..sas7bdat;

    /* Clear libref after use */
    %let _librc = %sysfunc(libname(&sas7bdat_lib));
  %end;
  %else %do;
    %put WARNING: [compare_qc] Could not assign libref for &sas7bdat_dir. Dataset not saved.;
  %end;
%mend save_qc_sas7bdat;

%if %upcase(&save_sas7bdat)=Y %then %do;
  %save_qc_sas7bdat;
%end;

 /* Count observations and variables */
 proc sql noprint;
   select count(*) into :nobs_base from &sdtm_lib..&DOMAIN;
   select count(*) into :nobs_cmp from &cmp_ds;
   select count(*) into :nvars_base from dictionary.columns where libname=upcase("&sdtm_lib") and memname=upcase("&DOMAIN");
   select count(*) into :nvars_cmp from dictionary.columns where libname="WORK" and memname=upcase("&domain_lc");
 quit;

 /* Check variable count mismatch */
 %if &nvars_base ne &nvars_cmp %then %do;
   %put WARNING: [compare_qc] Variable count mismatch: SDTM=&nvars_base vs QC XPT=&nvars_cmp;
   %let _cmp_sysinfo = 9999;
 %end;
 %else %do;
   /* Run PROC COMPARE */
   %if &nobs_base=0 and &nobs_cmp=0 %then %do;
     %put NOTE: [compare_qc] Both datasets are empty. Comparing structure only.;
     proc printto log="%sysfunc(pathname(work))/null.log" new; run;
     proc compare base=&sdtm_lib..&DOMAIN compare=&cmp_ds noprint;
       %if %sysevalf(%superq(idvars)^=, boolean) %then %do; id &idvars; %end;
     run;
     proc printto; run;
     %let _cmp_sysinfo = 0;
   %end;
   %else %do;
     proc compare base=&sdtm_lib..&DOMAIN compare=&cmp_ds noprint;
       %if %sysevalf(%superq(idvars)^=, boolean) %then %do; id &idvars; %end;
     run;
     %let _cmp_sysinfo = &sysinfo;
   %end;
 %end;

 %put NOTE: [compare_qc] &DOMAIN -> SYSINFO=&_cmp_sysinfo;

 /* Visible compare */
 %if %upcase(&visible)=Y %then %do;
   title "Visible PROC COMPARE (requested) — &DOMAIN";
   %if &nobs_base=0 and &nobs_cmp=0 %then %do;
     proc printto log="%sysfunc(pathname(work))/null.log" new; run;
   %end;
   proc compare base=&sdtm_lib..&DOMAIN compare=&cmp_ds &cmp_options;
     %if %sysevalf(%superq(idvars)^=, boolean) %then %do; id &idvars; %end;
   run;
   %if &nobs_base=0 and &nobs_cmp=0 %then %do;
     proc printto; run;
   %end;
   title;
 %end;

 /* Success message */
 %if &_cmp_sysinfo=0 %then %do;
   %put NOTE: [compare_qc] &DOMAIN: SDTM and QC XPT match (no differences).;
 %end;

 /* PDF report if differences or mismatch */
 %if %upcase(&pdf)=Y and &_cmp_sysinfo>0 %then %do;
   %if %sysevalf(%superq(pdf_dir)=, boolean) %then %do;
     %let _pdfdir = &qc_root_eff./10 QC Reports;
   %end;
   %else %do;
     %let _pdfdir = %sysfunc(translate(&pdf_dir,%str(/),%str(\)));
   %end;

   filename _pdfchk "&_pdfdir";
   %let _dir_ok = %sysfunc(fexist(_pdfchk));
   %if &_dir_ok=0 %then %do;
     %let _mkdir_rc = %sysfunc(dcreate(%scan(&_pdfdir,-1,/),
                        %substr(&_pdfdir,1,%length(&_pdfdir)-%length(%scan(&_pdfdir,-1,/))-1)));
     filename _pdfchk clear;
     filename _pdfchk "&_pdfdir";
     %let _dir_ok = %sysfunc(fexist(_pdfchk));
   %end;
   filename _pdfchk clear;

   %let _date = %sysfunc(today(), yymmddn8.);
   %let _time = %sysfunc(translate(%sysfunc(time(), hhmmss.),%str(),%str(:)));
   %let _pdf_name = compare_qc_&DOMAIN._&_date._&_time..pdf;
   %let _pdfpath  = &_pdfdir./&_pdf_name;

   %put NOTE: [compare_qc] Creating PDF report -> &_pdfpath ;

   ods pdf file="&_pdfpath" notoc;
     title1 "QC Compare Report for &DOMAIN";
     title2 "BASE: &sdtm_lib..&DOMAIN    COMPARE: &cmp_ds";
     title3 "XPT: &xpt_path";
     footnote1 "Generated: %sysfunc(datetime(), datetime19.)  SYSINFO=&_cmp_sysinfo";

     proc compare base=&sdtm_lib..&DOMAIN compare=&cmp_ds &cmp_options;
       %if %sysevalf(%superq(idvars)^=, boolean) %then %do; id &idvars; %end;
     run;

     ods listing close;

     proc compare base=&sdtm_lib..&DOMAIN compare=&cmp_ds
                  out=__cmp_out outbase outcomp outdif outnoequal noprint;
       %if %sysevalf(%superq(idvars)^=, boolean) %then %do; id &idvars; %end;
     run;

     ods listing;

     proc sql noprint;
       create table __cmp_summary as
       select
         "&DOMAIN" as domain length=8,
         count(*)                  as n_rows_with_output,
         sum(_type_='DIF')         as n_diff_rows
       from __cmp_out;
     quit;

     title4 "Quick Summary (rows in OUT= with differences)";
     proc print data=__cmp_summary noobs label; run;
     proc datasets lib=work nolist; delete __cmp_out __cmp_summary; quit;
   ods pdf close;
   title; footnote;
 %end;
 %else %if %upcase(&pdf)=Y and &_cmp_sysinfo=0 %then %do;
   %put NOTE: [compare_qc] No differences for &DOMAIN.. PDF report not created. ✅;
 %end;

 filename xptfile clear;
%mend compare_qc;
