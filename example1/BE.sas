/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : BE.sas
 PURPOSE       : Create SDTM data for Biospecimen Events
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-15 - cattanch - Initial program
 2025-12-19 - cattanch - Minor update to BETERM.
 2026-02-04 - cattanch - Updated BEREASOC.
 2026-02-11 - cattanch - Minor: added VISITDY.
******************************************************************************/

/* Configuration */
%nutricia_config;
%let sdtm_domain = %unquote(&_progname);

%copy_import (inset=lbs);

/* Stool */
data stool;
	length studyid domain subjid sourceid becat bedecod beterm bespec bepresp beoccur begeo bereasoc beenrtpt beentpt bedtc bestdtc $200;
	set lbs_1;
	
	studyid  = "&studyid";
    domain   = "&sdtm_domain";
    subjid   = strip(subjectid);
    sourceid = catx(' ', "CRF:", put("LBS", $form.));
	%derive_visit_vars();

    /* Collecting */
	if not missing(stool_site) or not missing(stool_home) then do;
		becat   = "SAMPLE COLLECTION";
		bedecod = "COLLECTING";
		beterm  = "COLLECTED";
		bespec  = "STOOL";
		bepresp = "Y";
	
		if stool_site = "Y" then do;
			beoccur = "Y";
			begeo = "INVESTIGATIONAL SITE";
		end;
		else if stool_home = "Y" then do;
			beoccur = "Y";
			begeo = "HOUSEHOLD ENVIRONMENT";
		end;
		else if stool_site = "N" and stool_home = "N" then do;
			beoccur = "N";
			begeo = "";
			bereasoc = strip(stool_reasnd);
		end;
		else put "WARNING - Check data for stool_site and stool_home";
	
		if lbs_spiyn = "Y" then do;
			beenrtpt = "BEFORE";
			beentpt  = "FIRST STUDY PRODUCT INTAKE";
		end;
		else if lbs_spiyn = "N" then do;
			beenrtpt = "AFTER";
			beentpt  = "FIRST STUDY PRODUCT INTAKE";
		end;
		else call missing(beenrtpt, beentpt);
	
		%sdtm_dt2dtc(dt=stool_dat, dtc=bedtc);
    	%sdtm_dt2dtc(dt=stool_dat, dtc=bestdtc);
		
		output;
	end;
	
	/* Storing */
	if not missing(stoolins) then do;
		becat   = "SAMPLE COLLECTION";
		bedecod = "STORING";
		beterm  = "HANDLED BY PARENTS ACCORDING TO THE INSTRUCTIONS";
		bespec  = "STOOL";
		bepresp = "Y";
		beoccur = substr(strip(stoolins), 1);
		%sdtm_dt2dtc(dt=stool_dat, dtc=bedtc);
		bereasoc = strip(stosa_nd);
		call missing(begeo, beenrtpt, beentpt, bestdtc);
		output;
	end;
	
    /* Refrigerating */
	if not missing(stdatcou) then do;
		becat   = "SAMPLE COLLECTION";
		bedecod = "REFRIGERATING";
		beterm  = "PLACEMENT ON DRY ICE";
		bespec  = "STOOL";
		%sdtm_dt2dtc(dt=stool_dat, dtc=bedtc);
		%sdtm_dt2dtc(dt=stdatcou, dtc=bestdtc);
		call missing(bepresp, beoccur, begeo, bereasoc, beenrtpt, beentpt);
		output;
	end;
	
	/* Freezing */
	if not missing(stool_os) then do;
		becat   = "SAMPLE COLLECTION";
		bedecod = "FREEZING";
		beterm  = "PLACEMENT IN ON-SITE FREEZER";
		bespec  = "STOOL";
		%sdtm_dt2dtc(dt=stool_dat, dtc=bedtc);
		%sdtm_dt2dtc(dt=stool_os, dtc=bestdtc);
		call missing(bepresp, beoccur, begeo, bereasoc, beenrtpt, beentpt);
		output;
	end;
	
	keep usubjid studyid domain subjid sourceid becat bedecod beterm bespec bepresp beoccur begeo bereasoc beenrtpt beentpt bedtc bestdtc visit visitnum visitdy;
run;

/* Venous blood */
data blood;
	length studyid domain subjid sourceid becat bedecod beterm bespec bepresp beoccur bereasoc bedtc bestdtc $200;
	set lbs_1;
	
	studyid  = "&studyid";
    domain   = "&sdtm_domain";
    subjid   = strip(subjectid);
    sourceid = catx(' ', "CRF:", put("LBS", $form.));
	%derive_visit_vars();

	if not missing(wb_perf) then do;
		becat   = "SAMPLE COLLECTION";
		bedecod = "COLLECTING";
		beterm  = "COLLECTED";
		bespec  = "VENOUS BLOOD";
		bepresp = "Y";
		
		if wb_perf = "Y" then beoccur = "Y";
		else if wb_perf = "N" then beoccur = "N";
		bereasoc = strip(wb_reasnd);

		%sdtm_dt2dtc(dt=wb_dat, dtc=bedtc);
    	%sdtm_dt2dtc(dt=wb_dat, dtc=bestdtc);
		
		output;
	end;
		
	keep usubjid studyid domain subjid sourceid becat bedecod beterm bespec bepresp beoccur bereasoc bedtc bestdtc visit visitnum visitdy;
run;

/* Hemocue sample */
data hemocue;
	length studyid domain subjid sourceid becat bedecod beterm bespec bepresp beoccur bereasoc bedtc bestdtc beclmeth $200;
	set lbs_1;
	
	studyid  = "&studyid";
    domain   = "&sdtm_domain";
    subjid   = strip(subjectid);
    sourceid = catx(' ', "CRF:", put("LBS", $form.));
	%derive_visit_vars();

	if not missing(hc_perf) then do;
		becat   = "SAMPLE COLLECTION";
		bedecod = "COLLECTING";
		beterm  = "COLLECTED";
		bespec  = "CAPILLARY BLOOD";
		bepresp = "Y";
		beclmeth = "FINGERSTICK";
		
		if hc_perf = "Y" then beoccur = "Y";
		else if hc_perf = "N" then beoccur = "N";
		bereasoc = strip(hc_reasnd);

		%sdtm_dt2dtc(dt=hc_dat, dtc=bedtc);
    	%sdtm_dt2dtc(dt=hc_dat, dtc=bestdtc);
		
		output;
	end;
		
	keep usubjid studyid domain subjid sourceid becat bedecod beterm bespec bepresp beoccur bereasoc bedtc bestdtc visit visitnum visitdy beclmeth;
run;

/* BE */
data be1;
	set stool blood hemocue;
run;

proc sql;
  create table work.be2 as
    select a.*, b.rfstdtc
    from work.be1 as a
    left join sdtm.dm as b
    on a.usubjid = b.usubjid;
quit;

data &sdtm_domain.;
  length bedy bestdy beendy 8. berefid beendtc $200;
  set work.be2;

  %sdtm_dtc2dy(dtc=bedtc, dy=bedy);
  %sdtm_dtc2dy(dtc=bestdtc, dy=bestdy);
 
  /* Unmapped variables */
  call missing(berefid, beendtc, beendy) ;
  
  label beentpt = "End Reference Time Point";
  label beenrtpt = "End Relative to Reference Time Point";
run;

/* Finalize */
%sdtm_create_domain_v2 (
    domain       = &sdtm_domain,
    inset        = &sdtm_domain,
    keys         = studyid usubjid bespec beterm visit bestdtc,
    addsuppvars  = BEENTPT BEENRTPT,
    metavarsset    = metadata.sdtm_study_variables,
    metadomainsset = metadata.sdtm_study_domains,
    deldupkeys   = N);
