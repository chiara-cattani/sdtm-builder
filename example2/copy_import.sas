/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : SONA / 23REX0061265
 PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/01 Macros
 PROGRAM NAME  : copy_import.sas
 PURPOSE       : Copy a CRF data set to work, create USUBJID, and remove formats and informat
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-10-03 - cattanch - Initial program
******************************************************************************/

%macro copy_import (inset =                   /* input datset */        
                  , inlib = import            /* input library */
                  , outset= work.&inset._1    /* output datset, added USUBJID */        
                  , by    = usubjid           /* optional sort item, default USUBJID */
                  , where =                   /* where clause */  
                  , noformats = Y             /* remove formats (Y/N, 1/0) */
                  );

   %util_mparams (params = inset inlib outset by where noformats);          
   
   proc sql;
      create table &outset. as 
      select catx('-', "&studyid", scan(subjectid, 1, '-'), scan(subjectid, 2, '-'), put(input(scan(subjectid, 3, '-'), best.), z4.)) as USUBJID length=50
      , *
      from &inlib..&inset. 
      %if %length(&where.) > 0 %then where &where.;
      %if %length(&by.) > 0 %then order by %commas(&by.);
      ;
   quit;
   
   proc datasets lib = %scan(WORK.&outset,-2,%str(.)) mt = data nolist;
      modify %scan(WORK.&outset,-1,%str(.));
        informat _all_;
        %if %sysfunc(indexc(%upcase(&noformats. ),Y1)) > 0 %then format _all_ %str(;);
      run;
   quit;
     
%mend copy_import;
