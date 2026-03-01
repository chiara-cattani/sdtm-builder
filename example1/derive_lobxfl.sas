/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : /SAS/NUTRI-IRON/Files/15 SDTM Conversion/01 Macros/
 MACRO NAME   :  derive_lobxfl.sas
 PURPOSE      :  Macro to derive Last Observation Before Exposure Flag (--LOBXFL)
 ------------------------------------------------------------------------------
 NOTES:
 Identifies and flags the last valid, non-missing observation before exposure
 (RFXSTDTC) for each subject-test. Valid observations exclude missing, "ND", 
 or "NOT DONE" results and "NOT DONE" status. If RFXSTDTC is missing, flags 
 the last valid observation overall.
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2026-02-02 - Chiara Cattani - Initial version
******************************************************************************/

%macro derive_lobxfl(
    domain=,                /* REQUIRED: Domain code (VS, LB, QS, EG) */
    inds=,                  /* REQUIRED: Input dataset */
    outds=,                 /* REQUIRED: Output dataset */
    testcd=,                /* REQUIRED: Test code variable (e.g., VSTESTCD) */
    dtc=,                   /* REQUIRED: Date/time variable (e.g., VSDTC) */
    orres=,                 /* REQUIRED: Result variable (e.g., VSORRES) */
    subjid=USUBJID,         /* Subject ID variable (default: USUBJID) */
    stat=,                  /* Status variable (e.g., VSSTAT) - optional */
    dm_ds=sdtm.dm,          /* DM dataset with RFXSTDTC (default: sdtm.dm) */
    ref_var=RFXSTDTC,       /* Reference date from DM (default: RFXSTDTC) */
    base_before_exp=Y,      /* Y/N: Same-day obs counted before exposure (default: Y) */
    sort_by=,               /* Tie-breaking vars for same datetime (e.g., REPNUM) */
    key_vars=               /* Additional grouping vars (e.g., LBSPEC for per-specimen flagging) */
) / minoperator;

    /* Validate required parameters */
    %if %length(&domain.)=0 or %length(&inds.)=0 or %length(&outds.)=0 or 
        %length(&testcd.)=0 or %length(&dtc.)=0 or %length(&orres.)=0 %then %do;
        %put ERROR: derive_lobxfl - DOMAIN, INDS, OUTDS, TESTCD, DTC, and ORRES are required.;
        %return;
    %end;

    /* Clean and validate inputs */
    %let domain = %upcase(%sysfunc(strip(&domain.)));
    %let subjid = %upcase(%sysfunc(strip(&subjid.)));
    %let testcd = %upcase(%sysfunc(strip(&testcd.)));
    %let dtc = %upcase(%sysfunc(strip(&dtc.)));
    %let orres = %upcase(%sysfunc(strip(&orres.)));
    %let ref_var = %upcase(%sysfunc(strip(&ref_var.)));
    %let base_before_exp = %upcase(%sysfunc(strip(&base_before_exp.)));
    %if not (&base_before_exp. in Y N) %then %let base_before_exp = Y;
    
    /* Handle optional parameters that may be empty */
    %if %length(&stat.)>0 %then %let stat = %upcase(%sysfunc(strip(&stat.)));
    %else %let stat = ;
    
    %if %length(&sort_by.)>0 %then %let sort_by = %upcase(%sysfunc(compress(&sort_by.,%str( ))));
    %else %let sort_by = ;
    
    %if %length(&key_vars.)>0 %then %let key_vars = %upcase(%sysfunc(compress(&key_vars.,%str( ))));
    %else %let key_vars = ;
    
    %let lobxfl = &domain.LOBXFL;
    
    %put NOTE: derive_lobxfl - Domain=&domain., Flag=&lobxfl., Base_before_exp=&base_before_exp.;

    /* Parse dataset library and member name */
    %let inds = %sysfunc(strip(&inds.));
    %let outds = %sysfunc(strip(&outds.));
    %let dm_ds = %sysfunc(strip(&dm_ds.));
    
    %if %index(&inds., .)>0 %then %do;
        %let inlib = %upcase(%scan(&inds., 1, .));
        %let inmem = %upcase(%scan(&inds., 2, .));
    %end;
    %else %do;
        %let inlib = WORK;
        %let inmem = %upcase(&inds.);
    %end;
    
    /* Validate input dataset exists */
    %if not %sysfunc(exist(&inds.)) %then %do;
        %put ERROR: derive_lobxfl - Input dataset &inds. does not exist.;
        %return;
    %end;

    /* Check if reference variable exists in input */
    %let ref_exists = 0;
    proc sql noprint;
        select count(*) into :ref_exists trimmed
        from dictionary.columns
        where libname="&inlib." and memname="&inmem." and upcase(name)=upcase("&ref_var.");
    quit;

    /* Validate DM dataset exists */
    %if not %sysfunc(exist(&dm_ds.)) %then %do;
        %put WARNING: derive_lobxfl - DM dataset &dm_ds. does not exist. Using input data only.;
        %if &ref_exists.=0 %then %do;
            %put ERROR: derive_lobxfl - &ref_var. not in input and DM dataset not found.;
            %return;
        %end;
        data _temp1; set &inds.; run;
    %end;
    %else %do;
        /* Merge with DM - avoid duplicate variable warning */
        %if &ref_exists.=0 %then %do;
            proc sql noprint;
                create table _temp1 as
                select a.*, b.&ref_var.
                from &inds. as a 
                left join &dm_ds.(keep=usubjid &ref_var.) as b
                on upcase(strip(a.&subjid.))=upcase(strip(b.usubjid));
            quit;
        %end;
        %else %do;
            proc sql noprint;
                create table _temp1 as
                select a.*, b.&ref_var. as _dm_ref
                from &inds. as a 
                left join &dm_ds.(keep=usubjid &ref_var.) as b
                on upcase(strip(a.&subjid.))=upcase(strip(b.usubjid));
            quit;
            data _temp1; set _temp1;
                if missing(&ref_var.) and not missing(_dm_ref) then &ref_var.=_dm_ref;
                drop _dm_ref;
            run;
        %end;
    %end;

    /* Parse dates and validate observations */
    data _temp2;
        set _temp1;
        length _obs_dtm _rfx_dtm 8 _obs_date _rfx_date 8 _valid _before_exp 3;
        
        /* Parse observation datetime (ISO8601 or date formats) */
        if not missing(&dtc.) then do;
            _obs_dtm = coalesce(input(strip(&dtc.), ?? anydtdtm.), 
                                input(tranwrd(strip(&dtc.),'T',' '), ?? anydtdtm.),
                                input(strip(&dtc.), ?? e8601dt.),
                                input(strip(&dtc.), ?? e8601da.),
                                dhms(input(strip(&dtc.), ?? yymmdd10.),0,0,0),
                                dhms(input(strip(&dtc.), ?? date9.),0,0,0));
            if not missing(_obs_dtm) then _obs_date = datepart(_obs_dtm);
            _has_time = (length(compress(&dtc.))>10 and (index(upcase(&dtc.),'T')>0 or index(&dtc.,':')>0));
        end;
        
        /* Parse reference datetime */
        if not missing(&ref_var.) then do;
            _rfx_dtm = coalesce(input(strip(&ref_var.), ?? anydtdtm.), 
                                input(tranwrd(strip(&ref_var.),'T',' '), ?? anydtdtm.),
                                input(strip(&ref_var.), ?? e8601dt.),
                                input(strip(&ref_var.), ?? e8601da.),
                                dhms(input(strip(&ref_var.), ?? yymmdd10.),0,0,0),
                                dhms(input(strip(&ref_var.), ?? date9.),0,0,0));
            if not missing(_rfx_dtm) then _rfx_date = datepart(_rfx_dtm);
            _rfx_has_time = (length(compress(&ref_var.))>10 and (index(upcase(&ref_var.),'T')>0 or index(&ref_var.,':')>0));
        end;
        
        /* Validate observation (exclude missing, ND, NOT DONE) */
        _valid = (not missing(&orres.) and upcase(strip(&orres.)) not in ('ND','NOT DONE','','NOTDONE'));
        %if %length(&stat.)>0 %then %do;
            if not missing(&stat.) then 
                _valid = _valid and upcase(strip(&stat.)) not in ('NOT DONE','NOTDONE');
        %end;
        
        /* Flag observations before exposure */
        _before_exp = 0;
        if _valid then do;
            /* No exposure date - all valid obs are "before exposure" */
            if missing(&ref_var.) or missing(_rfx_dtm) then _before_exp=1;
            /* Observation date before exposure date */
            else if not missing(_obs_date) and not missing(_rfx_date) and _obs_date < _rfx_date then _before_exp=1;
            /* Same date - check time or use parameter */
            else if not missing(_obs_date) and not missing(_rfx_date) and _obs_date = _rfx_date then do;
                /* If both have time, compare times */
                if _has_time and _rfx_has_time and not missing(_obs_dtm) and not missing(_rfx_dtm) then do;
                    if timepart(_obs_dtm) < timepart(_rfx_dtm) then _before_exp=1;
                end;
                /* If RFXSTDTC has no time and base_before_exp=Y, all same-day obs are before exposure */
                %if &base_before_exp.=Y %then %do;
                    else if not _rfx_has_time then _before_exp=1;
                %end;
            end;
        end;
        
        format _obs_dtm _rfx_dtm datetime20. _obs_date _rfx_date date9.;
    run;

    /* Check for valid observations */
    %let nvalid = 0;
    %let nbefore = 0;
    proc sql noprint;
        select count(*) into :nvalid trimmed from _temp2 where _valid=1;
        select count(*) into :nbefore trimmed from _temp2 where _before_exp=1;
    quit;

    %if &nvalid.=0 %then %do;
        %put WARNING: derive_lobxfl - No valid observations found. &lobxfl. will be blank for all records.;
        data &outds.; 
            set _temp2(drop=_obs_dtm _rfx_dtm _obs_date _rfx_date _has_time 
                            _rfx_has_time _valid _before_exp 
                            %if &ref_exists.=0 %then &ref_var.;);
            length &lobxfl. $1; 
            &lobxfl.='';
        run;
        proc datasets library=work nolist; delete _temp1 _temp2; quit;
        %return;
    %end;
    
    %if &nbefore.=0 %then %do;
        %put WARNING: derive_lobxfl - No observations before exposure found. &lobxfl. will be blank for all records.;
        data &outds.; 
            set _temp2(drop=_obs_dtm _rfx_dtm _obs_date _rfx_date _has_time 
                            _rfx_has_time _valid _before_exp 
                            %if &ref_exists.=0 %then &ref_var.;);
            length &lobxfl. $1; 
            &lobxfl.='';
        run;
        proc datasets library=work nolist; delete _temp1 _temp2; quit;
        %return;
    %end;

    /* Sort by subject, test, datetime */
    proc sort data=_temp2; 
        by &subjid. &testcd. 
           %if %length(&key_vars.)>0 %then &key_vars.;
           %if %length(&sort_by.)>0 %then &sort_by.; 
           _obs_dtm &dtc.; 
    run;

    /* Find maximum datetime for each subject-test combination */
    proc sql;
        create table _last as
        select &subjid., &testcd.
               %if %length(&key_vars.)>0 %then %do;, &key_vars. %end;,
               max(_obs_dtm) as _max_dtm format=datetime20.,
               max(&dtc.) as _max_dtc length=50
               %if %length(&sort_by.)>0 %then %do;
                   %let i=1; %let var=%scan(&sort_by.,&i.,%str( ));
                   %do %while(%length(&var.)>0);
                       , max(&var.) as _max_&var.
                       %let i=%eval(&i.+1); %let var=%scan(&sort_by.,&i.,%str( ));
                   %end;
               %end;
        from _temp2 where _before_exp=1
        group by &subjid., &testcd. 
                 %if %length(&key_vars.)>0 %then %do;, &key_vars. %end;;
    quit;

    /* Merge and assign LOBXFL flag */
    proc sql;
        create table _final as
        select a.*,
               case when b.&subjid. is not null 
                    and not missing(a._obs_dtm) 
                    and not missing(b._max_dtm)
                    and a._obs_dtm=b._max_dtm 
                    and upcase(strip(a.&dtc.))=upcase(strip(b._max_dtc))
                    %if %length(&sort_by.)>0 %then %do;
                        %let i=1; %let var=%scan(&sort_by.,&i.,%str( ));
                        %do %while(%length(&var.)>0);
                            and coalesce(a.&var.,.)=coalesce(b._max_&var.,.)
                            %let i=%eval(&i.+1); %let var=%scan(&sort_by.,&i.,%str( ));
                        %end;
                    %end;
                    then 'Y' else '' end as &lobxfl. length=1
        from _temp2 as a
        left join _last as b 
        on upcase(strip(a.&subjid.))=upcase(strip(b.&subjid.)) 
           and upcase(strip(a.&testcd.))=upcase(strip(b.&testcd.))
           %if %length(&key_vars.)>0 %then %do; 
               %let i=1; %let var=%scan(&key_vars.,&i.,%str( ));
               %do %while(%length(&var.)>0);
                   and upcase(strip(a.&var.))=upcase(strip(b.&var.))
                   %let i=%eval(&i.+1); %let var=%scan(&key_vars.,&i.,%str( ));
               %end;
           %end;
        order by a.&subjid., a.&testcd., a._obs_dtm;
    quit;

    /* Create final output without temporary variables */
    data &outds.;
        set _final(drop=_obs_dtm _rfx_dtm _obs_date _rfx_date _has_time 
                        _rfx_has_time _valid _before_exp 
                        %if &ref_exists.=0 %then &ref_var.;);
    run;

    /* Report results and cleanup */
    proc sql noprint;
        select count(*) into :nflagged trimmed from &outds. where &lobxfl.='Y';
    quit;
    %put NOTE: derive_lobxfl completed - &nflagged. records flagged.;
    
    proc datasets library=work nolist; delete _temp1 _temp2 _last _final; quit;

%mend derive_lobxfl;

/*******************************************************************************
 USAGE EXAMPLES
*******************************************************************************/

/* Example 1: Basic VS domain */
/* %derive_lobxfl(domain=VS, inds=work.vs0, outds=work.vs, testcd=VSTESTCD, */
/*                dtc=VSDTC, orres=VSSTRESC, stat=VSSTAT, sort_by=VISITNUM); */

/* Example 2: LB domain with different base_before_exp setting */
/* %derive_lobxfl(domain=LB, inds=work.lb0, outds=work.lb, testcd=LBTESTCD, */
/*                dtc=LBDTC, orres=LBORRES, stat=LBSTAT, base_before_exp=N, */
/*                sort_by=VISITNUM LBSEQ); */

/* Example 3: LB domain with LBSPEC - flags last obs per TESTCD+SPEC combination */
/* %derive_lobxfl(domain=LB, inds=work.lb0, outds=work.lb, testcd=LBTESTCD, */
/*                dtc=LBDTC, orres=LBORRES, stat=LBSTAT, key_vars=LBSPEC, */
/*                sort_by=VISITNUM LBSEQ); */

/* Example 4: QS domain with multiple sort variables */
/* %derive_lobxfl(domain=QS, inds=work.qs0, outds=work.qs, testcd=QSTESTCD, */
/*                dtc=QSDTC, orres=QSORRES, stat=QSSTAT, sort_by=VISITNUM QSSEQ); */

/* Example 5: EG domain without status variable */
/* %derive_lobxfl(domain=EG, inds=work.eg0, outds=work.eg, testcd=EGTESTCD, */
/*                dtc=EGDTC, orres=EGORRES, sort_by=VISITNUM); */
