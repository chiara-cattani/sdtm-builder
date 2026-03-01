/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : CE.sas
 PURPOSE       : Create SDTM data for Clinical Events
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-15 - cattanch - Initial program
 2026-02-09 - cattanch - Add CELNKID.
 2026-02-11 - cattanch - Minor: added VISITDY.
******************************************************************************/

/* Configuration */
%nutricia_config;
%let sdtm_domain = %unquote(&_progname);

%copy_import (inset=stool);
%copy_import (inset=giq);

/* Stool */
data stool;
	length studyid domain subjid sourceid ceeval ceorig ceterm cestdtc cepresp ceoccur cecat cescat ceevintx $200;
	set stool_1;
	
	if not missing(stool4);

	studyid  = "&studyid";
    domain   = "&sdtm_domain";
    subjid   = strip(subjectid);
    sourceid = catx(' ', "CRF:", put("STOOL", $form.));
	%derive_visit_vars();

	ceeval = "INVESTIGATOR";
	ceorig = "CRF";
	ceterm = "STOOL";
	%sdtm_dt2dtc(dt=stool4, dtc=cestdtc);
	cepresp = "Y";
	if missing(stool71) then ceoccur = "Y";
	else ceoccur = "N";
	cecat    = "GASTROINTESTINAL SYMPTOMS";
	cescat   = "DEFECATION";
	ceevintx = "LAST 24 HOURS";
		
	keep usubjid studyid domain subjid sourceid ceeval ceorig ceterm cestdtc cepresp ceoccur cecat cescat ceevintx visit visitnum visitdy;
run;

/* GIQ */
data giq;
	length studyid domain subjid sourceid ceeval ceorig ceterm cestdtc cepresp ceoccur cecat cescat ceevintx $200;
	set giq_1;
	if not missing(giq1);

	studyid  = "&studyid";
    domain   = "&sdtm_domain";
    subjid   = strip(subjectid);
    sourceid = catx(' ', "CRF:", put("GIQ", $form.));
	%derive_tpt_vars();

	ceeval = "PARENT";
	ceorig = "pPRO";
	ceterm = "STOOL";
	%sdtm_dt2dtc(dt=giq1, dtc=cestdtc);
	cepresp = "Y";
	if missing(giq41) then ceoccur = "Y";
	else ceoccur = "N";
	cecat    = "GASTROINTESTINAL SYMPTOMS";
	cescat   = "DEFECATION";
	ceevintx = "TODAY";
		
	keep usubjid studyid domain subjid sourceid ceeval ceorig ceterm cestdtc cepresp ceoccur cecat cescat ceevintx cetpt cetptref ceeltm cetptnum;
run;

/* CE */
data ce1;
	set stool giq;
run;

proc sql;
  create table work.ce2 as
    select a.*, b.rfstdtc
    from work.ce1 as a
    left join sdtm.dm as b
    on a.usubjid = b.usubjid;
quit;

data &sdtm_domain.;
  length cestdy 8. celnkid $200;
  set work.ce2;
  %sdtm_dtc2dy(dtc=cestdtc, dy=cestdy); 
  if not missing(cetptnum) then celnkid = strip(cestdtc) || "-" || strip(put(cetptnum, best.));
  else celnkid = strip(cestdtc);
  label visitdy = "Planned Study Day of Visit";
run;

/* Finalize */
%sdtm_create_domain_v2 (
    domain       = &sdtm_domain,
    inset        = &sdtm_domain,
    keys         = studyid usubjid ceterm cestdtc ceeval visit cetpt cetptref,
    addsuppvars  = VISITDY,
    metavarsset    = metadata.sdtm_study_variables,
    metadomainsset = metadata.sdtm_study_domains,
    deldupkeys   = N
);
