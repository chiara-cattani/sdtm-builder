/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : /SAS/NUTRI-IRON/Files/15 SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : RELREC.sas
 PURPOSE       : Create SDTM data for Related Records
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-03 - Kirill Novichkov - Initial version
 2025-12-12 - Kirill Novichkov - v1.1: LB+AE/MH and PR+FA relationships added
******************************************************************************/

%nutricia_config;

%let domain = %unquote(&_progname);

/*****************************************************************************
  - Use macro %copy import to copy the raw dataset to the work folder,
    with name work.&inset_1
  - Creates USUBJID and removes formats and informats
  - by = can be used to sort, default is USUBJID
*****************************************************************************/
%copy_import (inset = cm);
%copy_import (inset = eos);
%copy_import (inset = pr);
%copy_import (inset = sae);
%copy_import (inset = lb);
%copy_import (inset = lbs_simg);

/******************************************************************************
  The macro code 'relations' is used to create a temporary dataset per relation.
  e.g. for CM and AE or for CM and MH.
  
  1. PROC SQL is used to create macro variables from the CMAE# CMMH# CMPR# variables
     It is unknown how many related adverse events, medical histories, procedures there are per subject
     By using PROC SQL the code can be made more data-driven
     When using &linkvar. the CMAE# etc. in the source are selected
     For example &linkvar. for CM-AE can contain CMAE1 CMAE2 CMAE3
  
  2. DATA step is used to create temporary dataset
     The array rel_var stores all elements in the array
     i.e. all the CMAE#, CMMH# or CMPR# present in &linkvar.
     For example rel_var(3) is the 3rd specified in &linkvar, so CMAE3
     
     The do loop does at least 1 interation to the number of elements / CMAE# that rel_var stored 
     The next step executes if rel_var(1) is not missing. i is 1 to number of elements
     
     RDOMAIN, IDVAR, IDVARVAL, RELID and RELTYPE are derived
     For RELID for the related domain, the first number is taken from the value in CMAE# etc.
     
  3. It is ensured there are no duplicates for the main domain
  
  4. When calling the macro, the input dataset (inds), rdomain1 and rdomain2 are specified
     
  Finally, all the temporary datasets will be combined vertically  

******************************************************************************/

/* Macro details: 
   inds          - source dataset
   rdomain1      - main domain
   rdomain2      - related domain
   linkvarprefix - linking variable, e.g. CMAE, CMPR
   linkvarsuffix - set to ID to exclude from source
   idvar1        - main domain ID, e.g. CMSPID
   idvar2        - related domain ID, e.g. CMSPID
   idvarval1     - value of domain ID
   relid         - value of idvarval for (idvar1 || idvarval)  */
%macro relations(inds = ,rdomain1 = ,rdomain2 = ,linkvarprefix = , linkvarsuffix =id, idvar1 = , idvar2=, idvarval1 =, relid = , origidvarval=N);
  proc sql noprint;
    select distinct name into: linkvar separated by " "     /* Creates macro variables (linkvar) from what is selected */
    from dictionary.columns
    where upcase(libname) = "WORK"                          /* Select from work library */
          and upcase(memname) = upcase("&inds.")            /* Where member name is the source dataset */
          and upcase(name) ? upcase("&linkvarprefix.")      /* Column name contains links, e.g. CMAE# */
          and upcase(name) ~? upcase("&linkvarsuffix.")     /* And Column name does not contain ID */
          ;
  run;
  %put ###NOTE: LINKVAR= &linkvar.;
  %put ###NOTE: Create relations for [%upcase(&rdomain1.)] and [%upcase(&rdomain2.)];
  %put ###NOTE: Input dataset   : [%upcase(&inds.)];
  %put ###NOTE: Input variables : [%upcase(&linkvar.)];
  
  data _tmp_&rdomain1._&rdomain2.;
    length studyid rdomain usubjid idvar idvarval reltype relid subjid $200.;
    set &inds. /*(keep = usubjid subjectid &linkvar.: &rdomain1.spid)*/;
    by usubjid;
    
    /* Assigned variables */
    studyid = "&studyid.";
  
    /* Rename variables that are being copied without modification */
    subjid = strip(subjectid);
      
    /* Create the relations */
    array rel_var (*) &linkvar.; /* All from &linkvar. are stored in rel_var */
    
    do i = 1 to dim(rel_var); /* 1 to number of elements stored in rel_var */
      if ~missing(rel_var(i)) then do; 
        
        *MAIN DOMAIN;
        rdomain  = "%upcase(&rdomain1.)"; 
        idvar    = "%upcase(&idvar1.)"; 
        idvarval =  &idvarval1.; 
        relid    = "%upcase(&rdomain1.&rdomain2.)" || put(i, best.-l);
        reltype  = "";
        output;
        
        *RELATED DOMAIN;
        rdomain  = "%upcase(&rdomain2.)";
        if "&idvar2."^="" then idvar =  "%upcase(&idvar2.)";
        else idvar = "%upcase(&rdomain2.)SPID";
        if "&origidvarval" = "Y" then idvarval = rel_var(i);
        else idvarval = strip(scan(rel_var(i), 1, '-'));
        output;
      end;
    end;
            
    keep studyid usubjid rdomain subjid idvar idvarval relid reltype;
  run;
  
  /* Ensure that there's no duplicates especially for the main domain */  
  proc sort nodupkey;
    by _all_;
  run;
  
  %let dsid = %sysfunc(open(_tmp_&rdomain1._&rdomain2.));
  %let nobs = %sysfunc(attrn(&dsid.,nobs));
  %let rc   = %sysfunc(close(&dsid.));
  
  %put ###NOTE: [&nobs.] records were created for [%upcase(&rdomain1.)] and [%upcase(&rdomain2.)];
%mend relations;

/* Calls the macro 'relations' with values for macro variables specified here */

/*****************************************************************************
  Process relations between CM with AE, MH and PR
*****************************************************************************/
%relations( inds = cm_1 , rdomain1 = cm , rdomain2 = ae
          , linkvarprefix= cmae
          , idvar1 = cmspid
          , idvarval1 = strip(put(cmspid,best.))
          , relid = strip(put(cmspid,best.))
          );

%relations(inds = cm_1, rdomain1 = cm, rdomain2 = mh
          , linkvarprefix= cmmh
          , idvar1 = cmspid
          , idvarval1 = strip(put(cmspid,best.))
          , relid = strip(put(cmspid,best.))
          );
          
%relations(inds = cm_1, rdomain1 = cm, rdomain2 = pr
          , linkvarprefix= cmpr
          , idvar1 = cmspid
          , idvarval1 = strip(put(cmspid,best.))
          , relid = strip(put(cmspid,best.))
          );

/****************************************************************************
  Process relations between DS with AE
****************************************************************************/
%relations( inds = eos_1
          , rdomain1 = ds
          , rdomain2 = ae
          , linkvarprefix= etae
          , idvar1 = dsterm
          , idvarval1 = "ADVERSE EVENT"
          , relid = strip(scan(rel_var(i), 1, '-'))
          );

/* ***************************************************************************
  Process relations between PR with AE and MH
*************************************************************************** */
%relations(inds = pr_1, rdomain1 = pr, rdomain2 = ae
          , linkvarprefix= prae
          , idvar1 = prspid
          , idvarval1 = strip(put(prspid,best.))
          , relid = strip(put(prspid,best.))
          );
          
%relations(inds = pr_1, rdomain1 = pr, rdomain2 = mh
          , linkvarprefix= prmh
          , idvar1 = prspid
          , idvarval1 = strip(put(prspid,best.))
          , relid = strip(put(prspid,best.))
          );

/* ****************************************************************************
   Process relations between XS with AE and CM
**************************************************************************** */
%relations(inds = sae_1, rdomain1 = xs, rdomain2 = ae
          , linkvarprefix= aeno
          , idvar1 = xsspid
          , idvarval1 = strip(put(saespid,best.))
          , relid = strip(put(saespid,best.))
          ); 
          
%relations(inds = sae_1, rdomain1 = xs, rdomain2 = cm
          , linkvarprefix= aescm
          , idvar1 = xsspid
          , idvarval1 = strip(put(saespid,best.))
          , relid = strip(put(saespid,best.))
          );   
          
/* **************************************************************************** 
    Process relations between LB with AE and MH 
**************************************************************************** */
/* The dependency will be established as LB.LBLNKID (= catx('-', paramcd, visitnum)) = AE.AESPID / MH.MHSPID */
data lb_1;
  set lb_1;
  %derive_visit_vars();
  
  if cmiss(of hgb_lbaeno11 hgb_lbmhno11 mcv_lbaeno11 mcv_lbmhno11) < 4 then 
    put "ER" "ROR:: data for relations between LB and AE/MH was collected, please verify the correctness of RELREC derivations";
run;

%relations(inds = lb_1, rdomain1 = lb, rdomain2 = ae
          , linkvarprefix= hgb_lbaeno
          , idvar1 = lblnkid
          , idvarval1 = "HGB-" !! put(visitnum, best.-l)
          , relid = strip(scan(rel_var(i), 1, '-'))
          ); 
          
%relations(inds = lb_1, rdomain1 = lb, rdomain2 = mh
          , linkvarprefix= hgb_lbmhno
          , idvar1 = lblnkid
          , idvarval1 = "HGB-" !! put(visitnum, best.-l)
          , relid = strip(scan(rel_var(i), 1, '-'))
          );  

%relations(inds = lb_1, rdomain1 = lb, rdomain2 = ae
          , linkvarprefix= mcv_lbaeno
          , idvar1 = lblnkid
          , idvarval1 = "MCV-" !! put(visitnum, best.-l)
          , relid = strip(scan(rel_var(i), 1, '-'))
          ); 
          
%relations(inds = lb_1, rdomain1 = lb, rdomain2 = mh
          , linkvarprefix= mcv_lbmhno
          , idvar1 = lblnkid
          , idvarval1 = "MCV-" !! put(visitnum, best.-l)
          , relid = strip(scan(rel_var(i), 1, '-'))
          );  
             
/*****************************************************************************
  Process relations between PR with FA
*****************************************************************************/
/* The dependency will be established as: */
/*  PR.PRLNKID (= catx('-', PRTRT, PRDTC)) = FA.FALNKID (= catx('-', FADTC, FASPID)) */
data lbs_simg_1;
  length lbs_nximg $200;
  set lbs_simg_1;
  
  if ^missing(lbs_nximg) then put "ER" "ROR:: data for relations between PR and FA was collected, please verify the correctness of RELREC derivations";
  
  lbs_nximg = compress(lbs_nximg,"'",'a');
  if ^missing(lbs_nximg) then lbs_nximg = catx('-', lbs_dimg, lbs_nximg);
run;

%relations(inds = lbs_simg_1, rdomain1 = pr, rdomain2 = fa
          , linkvarprefix= lbs_nximg
          , idvar1 = prlnkid
          , idvar2 = falnkid
          , idvarval1 = "STOOL SAMPLE IMAGE-" !! lbs_dimg
          , relid = strip(scan(rel_var(i), 1, '-'))
          , origidvarval = Y
          );  

/*****************************************************************************
  Combine the temporary relations datasets
*****************************************************************************/

data work.&domain.;
  set _tmp_:;
run;

/* Design is applied and checks are made. */
%sdtm_create_domain_v2 ( domain         = &domain.
                       , inset          = &domain.
                       , keys           = studyid rdomain usubjid idvar idvarval relid 
                       , deldupkeys     = N
                       , metavarsset    = metadata.sdtm_study_variables
                       , metadomainsset = metadata.sdtm_study_domains);