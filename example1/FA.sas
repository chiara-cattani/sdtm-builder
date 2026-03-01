/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : /SAS/NUTRI-IRON/Files/15 SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : FA.sas
 PURPOSE       : Create SDTM data for Findings
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-17 - Kirill Novichkov - Initial version
 2025-12-22 - Kirill Novichkov - v1.1: Update after QC findings + aCRF updates
 2025-12-22 - Kirill Novichkov - v1.2: FATPT/FATPTNUM added for records from raw.GIQ
 2026-01-08 - Kirill Novichkov - v1.3: FALNKID populated for GIQ records instead of STOOL records
 2026-01-29 - Kirill Novichkov - v1.4: FADTC updated for FATESTCD=AGERES; 
                                  FAORRESU and FASTRESU updated for SQ-FFQ parameters; 
                                  FAORRES and FASTRESC remapped for FATESTCD=BITSS 
******************************************************************************/

%nutricia_config;

%let sdtm_domain = %unquote(&_progname);

* Prepare RAW data ;
%copy_import (inset=asq3);
%copy_import (inset=lbs);
%copy_import (inset=sqffq);
%copy_import (inset=stool);
%copy_import (inset=giq);
%copy_import (inset=ml);


proc sql noprint;
  * Create a list of FAORRES source variables from SQFFQ ;
  select name into : sqf_orres_list separated by ' '
  from dictionary.columns
  where libname=upcase('WORK') and memname=upcase('SQFFQ_1') and 
    substr(name,1,3)="SQF" and substr(name, lengthn(strip(name)), 1)="R";
    
  * Create a list of FAORRESU source variables from SQFFQ ;
  select name into : sqf_orresu_list separated by ' '
  from dictionary.columns
  where libname=upcase('WORK') and memname=upcase('SQFFQ_1') and 
    substr(name,1,3)="SQF" and substr(name, lengthn(strip(name)), 1)="U";
quit;

* Create formats for further FA variables derivations ;
proc format;
  value $sqftestcd
  "SQFFQER"   = "DCAL"
  "SQFFQPR"   = "DPROT"
  "SQFFQDR"   = "DFIBER"
  "SQFFQTFR"  = "DFATT"
  "SQFFQCHR"  = "DCARBT"
  "SQFFQFR"   = "DFATP"
  "SQFFQCR"   = "DCA"
  "SQFFQIR"   = "DFE"
  "SQFFQVAR"  = "DVITA"
  "SQFFQVCR"  = "DVITC"
  "SQFFQVDR"  = "DVITD"
  "SQFFQZR"   = "DZN"
  "SQFFQVIR"  = "DIODINE"
  "SQFFQVTFR" = "DFOLIC"
  "SQFFQVBR"  = "DVITB12"
  "SQFFQVMR"  = "DMG"
  other = " ";
  
  value $fatest  
  'BITSS' = 'Brussels Infant and Toddler Stool Scale'
  'BSFS' = 'Bristol Stool Form Scale'
  'DCAL' = 'Dietary Calories'
  'DPROT' = 'Dietary Protein'
  'FAALL' = 'All Findings About'
  'OCCUR' = 'Occurrence Indicator'
  'DIODINE' = 'Dietary Iodine'
  'DVITB12' = 'Dietary Vitamin B12'
  'DMG' = 'Dietary Magnesium'
  'DFOLIC' = 'Dietary Folic Acid'
  'DVITA' = 'Dietary Vitamin A'
  'DVITC' = 'Dietary Vitamin C'
  'DZN' = 'Dietary Zinc'
  'DFATT' = 'Dietary Fat, Total'
  'DCARBT' = 'Dietary Total Carbohydrate'
  'DFIBER' = 'Dietary Fiber'
  'DFATP' = 'Dietary Fat, Polyunsaturated'
  'DVITD' = 'Dietary Vitamin D'
  'AGERES' = 'Age at ASQ-3'
  'AGETRT1' = 'Age At First Treatment'
  'DFE' = 'Dietary Iron'
  'DCA' = 'Dietary Calcium'
  other = " ";
  
  /* FACAT $ FAORIG $ FAEVAL */
  value $facat
  'ASQ3' = 'ASQ-3'
  'LBS' = 'SAMPLE COLLECTION'
  'SQFFQ' = 'SQ-FFQ'
  'STOOL', 'GIQ' = 'GASTROINTESTINAL SYMPTOMS'
  'ML' = 'NON-STUDY PRODUCT FEEDING'
  other = ' ';
  
  value $bitss_res_remap
  'TYPE 1' = 'HARD'
  'TYPE 2' = 'FORMED'
  'TYPE 3' = 'LOOSE'
  'TYPE 4' = 'WATERY';
run;

* Append all raw data before merging with SDTM.DM and derive FATESTCD + FAOBJ + FASCAT + FAORRES + FAORRESU + FADAT ;
data &sdtm_domain.0; 
  length subjectid eventid eventdate eventname activityid fatestcd faobj fascat faorres faorresu fadat faorig faeval faevintx $200;
  set asq3_1  (keep=subjectid eventid eventdate activityid eventname asq3d asq3a asq3au asq31)
      lbs_1   (keep=subjectid eventid eventdate activityid eventname stool_ice wb_ao)
      sqffq_1 (keep=subjectid eventid eventdate activityid eventname ffq: sqf:)
      stool_1 (keep=subjectid eventid eventdate activityid eventname st:)
      giq_1   (keep=subjectid eventid eventdate activityid eventname giq1 gidst:)
      ml_1    (keep=subjectid eventid eventdate activityid eventname mlfcid ml_iron_: mlsfwage)
  indsname=_inds;
  
  sourceds = scan(scan(_inds,2,'.'),1,'_');
  
  domain    = "&sdtm_domain";
  sourceid  = "CRF: " !! put(sourceds, $form.); 
  faorig    = "CRF";
  faeval    = "INVESTIGATOR";
  facat     = put(sourceds, $facat.);
  
  if sourceds="ASQ3" and ^missing(asq3a) then do;
    fatestcd  = "AGERES";
    faobj     = "ASQ-3";
    fastresn  = asq3a;
    faorres   = put(asq3a, best.-l);
    faorresu  = asq3au;
    fadat     = asq3d;
    fascat    = catx(" ", asq31, "QUESTIONNAIRE");
    output;
  end;
  
  if sourceds="LBS" then do;
    if ^missing(stool_ice) then do;
      fatestcd  = "OCCUR";
      faobj     = "SUFFICIENT DRY ICE REMAINING IN TRANSPORT BOX";
      faorres   = stool_ice;
      output;
    end;
    if ^missing(wb_ao) then do;
      fatestcd  = "OCCUR";
      faobj     = "ANALGESIC OINTMENT USED";
      faorres   = wb_ao;
      output;
    end;
  end;
  
  if sourceds="SQFFQ" then do;
    faobj = "DIET";
    if ^missing(ffqndnd) then do;
      fatestcd  = "FAALL";
      fastat    = ffqndnd;
      fareasnd  = ffqndr;
      output;
    end;
    else do;
      fadat = ffqd;
      array sqforres  [*] &sqf_orres_list.;
      array sqforresu [*] &sqf_orresu_list.;
      do i=1 to dim(sqforres);
        if ^missing(sqforres[i]) then do;
          fatestcd  = put(vname(sqforres[i]), $sqftestcd.);
          fastresn  = sqforres[i];
          faorres   = put(sqforres[i], best.-l);
          faorresu  = tranwrd(sqforresu[i], "µg/day", "ug/day");
          output;
        end;
      end;
    end;
  end;
  
  if sourceds="STOOL" then do;
    fascat    = 'DEFECATION';
    faevintx  = 'LAST 24 HOURS';
    fadat     = stool4;
    %macro bitss_bsfs();
    %do i=1 %to 8;
      faspid    = put(&i., best.-l);
      if ^missing(st&i.) then do;
        faobj     = st&i.;
        if faobj = "DIAPER" then do;
          fatestcd  = 'BITSS';
          faorres   = put(stcd&i., $bitss_res_remap.);
          output;
        end;
        else if faobj = "TOILET" then do;
          fatestcd  = 'BSFS';
          faorres   = stct&i.;
          output;
        end;
        else put "WARN" "ING:QC Unknonwn ST&i. value " subjectid= eventid=;
        
        if (faobj="DIAPER" and ^missing(stct&i.)) then put "WARN" "ING:QC Stool form is DIAPER, but collected Consistency is for TOILET " subjectid= eventid= faspid=;
        if (faobj="TOILET" and ^missing(stcd&i.)) then put "WARN" "ING:QC Stool form is TOILET, but collected Consistency is for DIAPER " subjectid= eventid= faspid=;
      end;
    %end;
    %mend bitss_bsfs;
    %bitss_bsfs();
  end;
  
  if sourceds="GIQ" then do;
    fascat    = 'DEFECATION';
    faevintx  = 'TODAY';
    fadat     = giq1;
    faeval    = 'PARENT';
    faorig    = 'pPRO';
    
    %macro bitss_bsfs2();
    %do i=1 %to 8;
      faspid    = put(&i., best.-l);
      if ^missing(gidstdt&i.) then do;
        faobj     = gidstdt&i.;
        if faobj = "DIAPER" then do;
          fatestcd  = 'BITSS';
          faorres   = put(gidstd&i., $bitss_res_remap.);
          output;
        end;
        else if faobj = "TOILET" then do;
          fatestcd  = 'BSFS';
          faorres   = gidstt&i.;
          output;
        end;
        else put "WARN" "ING:QC Unknonwn GIDSTDT&i. value " subjectid= eventid=;
        
        if (faobj="DIAPER" and ^missing(gidstt&i.)) then put "WARN" "ING:QC Stool form is DIAPER, but collected Consistency is for TOILET " subjectid= eventid= faspid=;
        if (faobj="TOILET" and ^missing(gidstd&i.)) then put "WARN" "ING:QC Stool form is TOILET, but collected Consistency is for DIAPER " subjectid= eventid= faspid=;
      end;
    %end;
    %mend bitss_bsfs2;
    %bitss_bsfs2();
  end;
  
  if sourceds="ML" then do;
    facat     = "NON-STUDY PRODUCT FEEDING";
    faspid    = put(mlfcid, best.-l);
    
    if eventid='SCR'  then faevintx = "DURING 3 WEEKS PRIOR TO VISIT 0";
    if eventid='ML'   then faevintx = "SINCE LAST VISIT";
    
    if ^missing(mlsfwage) then do;
      fatestcd  = "AGETRT1";
      faorres   = put(mlsfwage, best.-l);
      fastresn  = mlsfwage;
      faorresu  = "MONTHS";
      faobj     = "SOLID WEANING FOODS";
      output;
    end;
    else do;
      array orres [10] $ ml_iron_a ml_iron_b ml_iron_c ml_iron_d ml_iron_e ml_iron_f ml_iron_g ml_iron_h ml_iron_i ml_iron_j;
      array brand [10] $ mlycb1    mlycb2    mlmpb1    mlmpb2    mlffb1    mlffb2    mllmb1    mllmb2    mlcmb1    mlcmb2;
      array other [10] $ mlycsp1   mlycsp2   mlmpsp1   mlmpsp2   mlffsp1   mlffsp2   mllmsp1   mllmsp2   mlcmsp1   mlcmsp2;
      
      do i=1 to 10;
        if ^missing(orres[i]) then do;
          faorres   = orres[i];
          faorresu  = "mg/100 mL";
          if cmiss(brand[i], other[i])=0 then do;
            length qc_msg $200;
            qc_msg = "Both " !! strip(vname(brand[i])) !! " and " !! strip(vname(other[i])) !! " were collected at the same time";
            put "WARN" "ING:QC " qc_msg " " subjectid= eventid=;
          end;
          else faobj = coalescec(brand[i], other[i]);
          output;
        end;
      end;
    end;
  end;
run;

proc sort data=&sdtm_domain.0;
  by subjectid;
run;

data &sdtm_domain.; 
  merge &sdtm_domain.0 (rename=(subjectid=subjid) in=a) 
        sdtm.dm (keep=subjid studyid usubjid rfstdtc);
  by subjid;
  if a;
  
  fatest    = put(fatestcd, $fatest.);
  fastresc  = faorres;
  fastresu  = faorresu;
  
  %derive_tpt_vars();
  %derive_visit_vars();
  %sdtm_dt2dtc(dt=  fadat, dtc=fadtc); 
  %sdtm_dtc2dy(dtc= fadtc); 
  
  length falnkid $200;
  
  if sourceds="GIQ" then falnkid = catx('-',fadtc,faspid);
run;

* Get the list of key variables from the metadata ;
proc sql noprint;  
  select tranwrd(compress(keys), ',', ' ') into: keyvars from metadata.sdtm_study_domains where domain="&sdtm_domain";
quit;

* Apply metadata and store into SDTM folder ;
%sdtm_create_domain_v2 ( domain           = &sdtm_domain.
                        , inset           = &sdtm_domain.
                        , keys            = &keyvars. fadtc facat faspid fatptnum 
                        , deldupkeys      = N
                        , metavarsset     = metadata.sdtm_study_variables
                        , metadomainsset  = metadata.sdtm_study_domains);