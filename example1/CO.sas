/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : CO.sas
 PURPOSE       : Create SDTM data for Comments
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-04 - cattanch - Initial program
 2025-12-22 - cattanch - Minor: strip COSPID.
 2026-01-27 - cattanch - Updated COREF.
 2026-02-04 - cattanch - Updated records from LBS and COREF.
 2026-02-05 - cattanch - Updated records from CO to follow mapping table for visits.
******************************************************************************/

/* Configuration */
%nutricia_config;
%let sdtm_domain = %unquote(&_progname);

/* Import source data */
%copy_import(inset = chk);
%copy_import(inset = co);
%copy_import(inset = lbs);
%copy_import(inset = vstat);

proc format;
  value $visitfmt
    "VISIT 0" = "Visit 0: Screening"
    "VISIT 1" = "Visit 1"
    "WK1 PHONE CALL" = "Phone Call 1"
    "WK5 PHONE CALL" = "Phone Call 2"
    "VISIT 2" = "Visit 2"
    "WK15 PHONE CALL" = "Phone Call 3"
    "WK20 PHONE CALL" = "Phone Call 4"
    "VISIT 3" = "Visit 3"
    "FOLLOW-UP CALL" = "Phone Call 5: Follow-up";
run;

/* CHK */
data chk1;
	length studyid domain subjid sourceid coval coref $200;
	set chk_1;
	if not missing(chkdrnd);
    %derive_visit_vars;
    coval    = strip(chkdrnd);
	coref    = strip(visit) || " - RETURN OF DIARY";
	studyid  = "&studyid";
    domain   = "&sdtm_domain";
    subjid   = strip(subjectid);
    sourceid = catx(" ", "CRF:", put("CHK", $form.));
	keep studyid domain usubjid subjid sourceid coval coref;
run;

/* CO */
data co1;
	length studyid domain subjid sourceid cospid coval coref $200;
	set co_1(rename=(cospid=_cospid coref=_coref));
	if not missing(_cospid);
	cospid   = strip(put(_cospid, best.));
    coval    = strip(coval);
	coref    = strip(put(upcase(covisit), visitfmt.)) || " - " || strip(_coref);
	studyid  = "&studyid";
    domain   = "&sdtm_domain";
    subjid   = strip(subjectid);
    sourceid = catx(" ", "CRF:", put("CO", $form.));
	keep studyid domain usubjid subjid sourceid cospid coval coref;
run;

/* LBS */
data lbs1;
	length studyid domain subjid sourceid coval coref $200;
	set lbs_1;
	%derive_visit_vars;
	studyid  = "&studyid";
    domain   = "&sdtm_domain";
    subjid   = strip(subjectid);
    sourceid = catx(" ", "CRF:", put("LBS", $form.));
	if not missing(lbs_spir) then do;
    	coval    = strip(lbs_spir);
		coref    = strip(visit) || " - STOOL SAMPLE COLLECTION BEFORE FIRST STUDY PRODUCT INTAKE";
		output;
	end;
	keep studyid domain usubjid subjid sourceid coval coref;
run;

/* VSTAT */
data vstat1;
	length studyid domain subjid sourceid coval coref $200;
	set vstat_1;
	if not missing(fupynr);
    %derive_visit_vars;
    coval    = strip(fupynr);
	coref    = strip(visit) || " - REASON PHONE CALL NOT SUCCESSFUL";
	studyid  = "&studyid";
    domain   = "&sdtm_domain";
    subjid   = strip(subjectid);
    sourceid = catx(" ", "CRF:", put("VSTAT", $form.));
	keep studyid domain usubjid subjid sourceid coval coref;
run;

/* Merge */
data work.&sdtm_domain;
  set work.chk1 co1 lbs1 vstat1;
  if length(coref) > 4999 then put "###WARNING: [COREF] exceeds length > 4999 at record " _n_ ", Code needs adjustment.";
run;

/* Finalize */
%sdtm_create_domain_v2(
  domain     = &sdtm_domain,
  inset      = &sdtm_domain,
  keys       = studyid usubjid cospid coref,
  metavarsset    = metadata.sdtm_study_variables,
  metadomainsset = metadata.sdtm_study_domains,
  deldupkeys = N
);
