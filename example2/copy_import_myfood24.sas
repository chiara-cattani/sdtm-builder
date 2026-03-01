/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : SONA / 23REX0061265
 PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
 PROGRAM NAME  : copy_import_myfood24.sas
 PURPOSE       : Copy a CRF data set to work, create USUBJID, and remove formats and informat
 ------------------------------------------------------------------------------
 NOTES : Adapted for myfood24 data.
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-10-03 - cattanch - Initial program
******************************************************************************/

%macro copy_import_myfood24 (
    inset = myfood24,
    inlib = import,
    outset = work.&inset._1,
    by = usubjid,
    where =,
    noformats = Y
);

   proc sql;
      create table &outset. as
      select 
         respondent_id as SUBJID length=50,
         /* Build standardized USUBJID */
         upcase(
            case
               /* Case 1: Already starts with SONA-NL: normalize */
               when prxmatch('/^SONA-NL-/i', respondent_id) then
                    catx('-', 'SONA', 'NL', scan(respondent_id, 3, '-'),
                         put(input(scan(respondent_id, -1, '-'), best.), z4.))

               /* Case 2: Starts with NL-: prepend SONA- */
               when prxmatch('/^NL-/i', respondent_id) then
                    catx('-', 'SONA', 'NL', scan(respondent_id, 2, '-'),
                         put(input(scan(respondent_id, -1, '-'), best.), z4.))

               /* Case 3: Starts with SONA- but missing NL: insert NL */
               when prxmatch('/^SONA-/i', respondent_id) then
                    catx('-', 'SONA', 'NL', scan(respondent_id, 2, '-'),
                         put(input(scan(respondent_id, -1, '-'), best.), z4.))

               /* Default: assume format like XXX-YYY-ZZZ: normalize */
               else
                    catx('-', 'SONA', 'NL', coalescec(scan(respondent_id, 2, '-'), '101'),
                         put(input(scan(respondent_id, -1, '-'), best.), z4.))
            end
         ) as USUBJID length=50,
         *
      from &inlib..&inset.
      %if %length(&where.) > 0 %then where &where.;
   quit;

   %if %sysfunc(exist(&outset.)) %then %do;
      proc datasets lib=%scan(&outset.,1,.) mt=data nolist;
         modify %scan(&outset.,2,.);
         informat _all_;
         %if %sysfunc(indexc(%upcase(&noformats.),Y1)) > 0 %then format _all_ %str(;);
      run;
   %end;

%mend copy_import_myfood24;