/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : LB.sas
 PURPOSE       : Create SDTM dataset for Laboratory Test Results
 ------------------------------------------------------------------------------
 NOTES : The original unit is not known yet, so we assume that results will be
 		 collected in the units stated in the protocol.
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-16 - Chiara Cattani - Initial program
 2025-12-22 - Chiara Cattani - Updated LBSPEC, LBSTRESU, LBLOBXFL, LBDTC.
 							   Added warnings for data issues.
 2025-12-23 - Chiara Cattani - Updated LBLOBXFL. 	
 2026-02-02 - Chiara Cattani - Added LBNAM, LBREFID, LBTPT, LBTPTNU, LBTPTREF.
						 	   Use macro to derive LBLOBXFL.
						 	   Populate LBLNKID only when there is a link.
******************************************************************************/

/* Configuration */
%nutricia_config;
%let sdtm_domain = %unquote(&_progname);

/* Import source data */
%copy_import (inset = lb);
%copy_import (inset = lbs);

/* LB */
data work._wb_src;
  set work.lbs_1(keep=usubjid eventid wb_dat);
  length wb_dtc $200;
  %sdtm_dt2dtc(dt = wb_dat, dtc = wb_dtc);
run;

proc sql;
  create table work._lb_join as
  select a.*, b.wb_dtc
  from work.lb_1 as a
  left join work._wb_src as b
    on a.usubjid = b.usubjid
   and a.eventid = b.eventid;
quit;

data work.lb1;
  	length studyid domain subjid sourceid lbdtc lbcat lbspec lbtest lbtestcd lborres lborresu lbstat lbreasnd lbornrlo lbornrhi lbclsig lblnkid lbstresc lbstresu $200
  		   lbstresn lbstnrlo lbstnrhi 8.;
  	set work._lb_join;
  	
  	studyid  = "&studyid";
    domain   = "&sdtm_domain";
    subjid   = strip(subjectid);
    sourceid = catx(' ', "CRF:", put("LB", $form.)); 
    %sdtm_dt2dtc(dt = wb_dtc, dtc = lbdtc);
    %derive_visit_vars;
	lbcat  = "HEMATOLOGY";
	lbspec = "VENOUS BLOOD";
	
	if not missing(hgb) then do;
		lbtest   = "Hemoglobin";
		lbtestcd = "HGB";
		lborres  = strip(put(hgb, 8.2));
		lborresu = strip(hgb_u);
		if not missing(hgb_ndnd) then lbstat = "NOT DONE";
		else call missing(lbstat);
		lbreasnd = strip(hgb_ndr);
		lbornrlo = strip(HGB_LBORNR_Lower);
		lbornrhi = strip(HGB_LBORNR_Upper);
		lbclsig  = substr(hgb_clsig, 1, 1);
		if not missing(hgb_lbaeno11) or not missing(hgb_lbmhno11) then lblnkid  = catx('-', lbtestcd, visitnum);
		if not missing(lborresu) then do;
			lbstresn = hgb;
			lbstresc = lborres;
			lbstresu = "g/dL";
			lbstnrlo = input(lbornrlo, best.);
			lbstnrhi = input(lbornrhi, best.);
		end;
		else do;
			call missing(lbstresn, lbstresc, lbstresu, lbstnrlo, lbstnrhi);
		end;
		if not missing(lborresu) and lborresu ne "g/dL" then put "WARNING: Conversion may be needed: Check original and standard results and units for " usubjid= lbtestcd= lborres=;
		if not missing(lborres) and missing(lborresu) then put "WARNING: Check data: result is present but unit is missing for " usubjid= lbtestcd= lborres= " Standard results and units will not be derived.";
		output;
	end;
	
	if not missing(mcv) then do;
		lbtest   = "Ery. Mean Corpuscular Volume";
		lbtestcd = "MCV";
		lborres  = strip(put(mcv, 8.2));
		lborresu = strip(mcv_u);
		if not missing(mcv_ndnd) then lbstat = "NOT DONE";
		else call missing(lbstat);
		lbreasnd = strip(mcv_ndr);
		lbornrlo = strip(mcv_LBORNR_Lower);
		lbornrhi = strip(mcv_LBORNR_Upper);
	    lbclsig  = substr(mcv_clsig, 1, 1);
	    if not missing(mcv_lbaeno11) or not missing(mcv_lbmhno11) then lblnkid  = catx('-', lbtestcd, visitnum);
	    if not missing(lborresu) then do;
			lbstresn = mcv;
			lbstresc = lborres;
			lbstresu = "fL";
			lbstnrlo = input(lbornrlo, best.);
			lbstnrhi = input(lbornrhi, best.);
		end;
		else do;
			call missing(lbstresn, lbstresc, lbstresu, lbstnrlo, lbstnrhi);
		end;
	    if not missing(lborresu) and lborresu ne "fL" then put "WARNING: Conversion may be needed: Check original and standard results and units for " usubjid= lbtestcd= lborres=;
		if not missing(lborres) and missing(lborresu) then put "WARNING: Check data: result is present but unit is missing for " usubjid= lbtestcd= lborres= " Standard results and units will not be derived.";
		output;
	end;
  
  keep studyid domain subjid sourceid lbdtc lbcat lbspec lbtest lbtestcd lborres lborresu lbstat lbreasnd lbornrlo lbornrhi lbclsig lblnkid lbstresc lbstresu lbstresn lbstnrlo lbstnrhi usubjid visit visitnum visitdy;
run;

/* LBS */
data work.lbs1;
  	length studyid domain subjid sourceid lbdtc lbcat lbspec lbtest lbtestcd lborres lborresu lbstat lbreasnd lbornrlo lbornrhi lbclsig lblnkid lbstresc lbstresu $200
  		   lbstresn lbstnrlo lbstnrhi 8.;
  	set work.lbs_1;
  	
  	studyid  = "&studyid";
    domain   = "&sdtm_domain";
    subjid   = strip(subjectid);
    sourceid = catx(' ', "CRF:", put("LBS", $form.)); 
    %sdtm_dt2dtc(dt = hc_dat, dtc = lbdtc);
    %derive_visit_vars;
	lbcat  = "HEMATOLOGY";
	lbspec = "CAPILLARY BLOOD";
	
	if not missing(hcv) then do;
		lbtest   = "Hemoglobin";
		lbtestcd = "HGB"; 
		lborres  = strip(put(hcv, 8.2));
		lborresu = strip(hcu);
		lbstat   = "";
		lbreasnd = "";
		lbornrlo = "";
		lbornrhi = "";
		lbclsig  = "";
		lblnkid  = "";
		if not missing(lborresu) then do;
			lbstresn = hcv;
			lbstresc = lborres;
			lbstresu = "g/dL";
			lbstnrlo = .;
			lbstnrhi = .;
		end;
		else do;
			call missing(lbstresn, lbstresc, lbstresu, lbstnrlo, lbstnrhi);
		end;
		if not missing(lborresu) and lborresu ne "g/dL" then put "WARNING: Conversion may be needed: Check original and standard results and units for " usubjid= lbtestcd= lborres=;
		if not missing(lborres) and missing(lborresu) then put "WARNING: Check data: result is present but unit is missing for " usubjid= lbtestcd= lborres= " Standard results and units will not be derived.";
		output;
	end;
	
  keep  studyid domain subjid sourceid lbdtc lbcat lbspec lbtest lbtestcd lborres lborresu lbstat lbreasnd lbornrlo lbornrhi lbclsig lblnkid lbstresc lbstresu lbstresn lbstnrlo lbstnrhi usubjid visit visitnum visitdy;
run;

/* Merge */
data lb_final;
	set lb1 lbs1;
	if missing(lbdtc) then put "WARNING: LBDTC is missing for " usubjid= visit= lbtestcd=; 
run;

proc sql;
  create table work.lb_final1 as
    select a.* , b.rfstdtc, b.rfxstdtc
      from 
        work.lb_final as a
      left join 
        sdtm.dm         as b 
      on a.usubjid = b.usubjid;
quit;

/* LBLOBXFL */
%derive_lobxfl(
    domain=LB,
	inds=lb_final1,
    outds=lb_final2,
    subjid=USUBJID,
    testcd=LBTESTCD,
    dtc=LBDTC,
    orres=LBORRES,
    stat=LBSTAT,
    dm_ds=sdtm.dm,
    base_before_exp=Y,
    key_vars=LBSPEC
);

/* LBDY */
data work.&sdtm_domain;
  set work.lb_final2;
  length lbdy lbtptnum 8. lbnrind lbnam lbrefid lbtpt lbtptref $200;
  %sdtm_dtc2dy(dtc = lbdtc);
  call missing(lbnrind, LBNAM, LBREFID, LBTPT, LBTPTNUM, LBTPTREF);		
run;

/* Finalize */
%sdtm_create_domain_v2 ( domain     = &sdtm_domain
                       , inset      = &sdtm_domain
                       , keys       = STUDYID USUBJID LBCAT LBTESTCD LBSPEC VISITNUM LBDTC
                       , metavarsset    = metadata.sdtm_study_variables
                       , metadomainsset = metadata.sdtm_study_domains
                       , deldupkeys = N);