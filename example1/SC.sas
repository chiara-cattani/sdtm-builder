/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : SC.sas
 PURPOSE       : Create SDTM data for Subject Characteristics
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-11 - Chiara Cattani - Initial program
 2025-12-22 - Chiara Cattani - Updated SCORRES to be as collected.
 2026-01-08 - Chiara Cattani - Updated SCORRES to follow codelist.
 2026-02-06 - Chiara Cattani - Added VISITDY.
******************************************************************************/

/* Configuration */
%nutricia_config;
%let sdtm_domain = %unquote(&_progname);

/* Import source data */
%copy_import (inset=lbs);
%copy_import (inset=sc);

/* LBS */
data work.lbs1;
	length studyid domain subjid sourceid scorres scstresc sctestcd sctest $200;
	set work.lbs_1;
	studyid  = "&studyid.";
    domain   = "&sdtm_domain.";
    subjid   = strip(subjectid);
    sourceid = catx(' ', "CRF:", put("LBS", $form.));
    %derive_visit_vars();
	if not missing(hcst1);
	sctest   = "Health Status";
	sctestcd = "HLTHSTAT";
	scorres  = strip(hcst1);
	scstresc = strip(hcst1);
	keep studyid domain subjid usubjid scorres scstresc sctestcd sctest visit visitdy  visitnum sourceid;
run;

/* SC */
data work.sc1;
	length studyid domain subjid sourceid scorres scorresu scstresc scstresu sctestcd sctest $200
	       scstresn 8.;
	set work.sc_1;
	
	studyid  = "&studyid.";
    domain   = "&sdtm_domain.";
    subjid   = strip(subjectid);
    sourceid = catx(' ', "CRF:", put("SC", $form.));
    %derive_visit_vars();
	
	if not missing(gestagew) then do;
		sctest   = "Gestational Age at Birth";
		sctestcd = "GSTABRTH";
		scorres  = strip(put(gestagew, best.)) || "W;" || strip(put(gestaged, best.)) ||"D";
	    scorresu = "";
		scstresn = gestagew * 7 + gestaged;
		scstresc = strip(put(scstresn, best.));
		scstresu = "DAYS";
		output;
	end;
	
	if not missing(btype) then do;
		sctest   = "Mode of Delivery";
		sctestcd = "DLVRMODE";
		scorres = strip(btype);
		if strip(btype) = "VAGINAL" then scorres = "VAGINAL DELIVERY";
		else if strip(btype) = "CAESAREAN SECTION" then scorres = "CESAREAN SECTION";
		scstresc = scorres;
		call missing(scorresu, scstresn, scstresu);
		output;
	end;
	
	keep studyid domain subjid usubjid scorres scorresu scstresc scstresn scstresu sctestcd sctest visit visitdy visitnum sourceid;
run;

/* Merge */
data work.&sdtm_domain.;
	set lbs1 sc1;
run;

/* Finalize */
%sdtm_create_domain_v2 ( domain      = &sdtm_domain.
                    , inset          = &sdtm_domain.
                    , keys           = studyid usubjid sctestcd
                    , deldupkeys     = N
                    , metavarsset    = metadata.sdtm_study_variables
                    , metadomainsset = metadata.sdtm_study_domains);