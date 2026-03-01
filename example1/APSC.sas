/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : APSC.sas
 PURPOSE       : Create SDTM data for Associated Persons Subject Characteristics
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-04 - Chiara Cattani - Initial program
 2025-12-11 - Chiara Cattani - Updated APID.
 2025-12-22 - Chiara Cattani - Updated SCORRES and SCSTRESC.
 2026-01-08 - Chiara Cattani - Updated SCORRES to follow CT.
 2026-02-02 - Chiara Cattani - Updated SCTEST and SREL.
 2026-02-11 - Chiara Cattani - Minor: added VISITDY.
******************************************************************************/

/* Configuration */
%nutricia_config;
%let sdtm_domain = %unquote(&_progname);

%copy_import (inset=sc);

/* APSC */
data work.apsc1;
	length studyid domain subjid rsubjid apid sourceid srel scorres scstresc sctestcd sctest $200;
	set work.sc_1;
	
	studyid  = "&studyid.";
    domain   = "&sdtm_domain.";
    subjid   = strip(subjectid);
    rsubjid  = strip(usubjid);
    sourceid = catx(' ', "CRF:", put("SC", $form.));
    %derive_visit_vars();

	if not missing(sc_r1) then do;
		srel = strip(sc_r1);
		apid = strip(subjid) || "-" || strip(srel);
		if not missing(hedl1) then do;
			sctest   = "Level of Education Attained";
			sctestcd = "EDULEVEL";
			scorres  = compress(strip(hedl1), '-');
			scstresc = compress(strip(hedl1), '-');
			output;
		end;
		if not missing(prostat1) then do;
			sctest   = "Employment Status";
			sctestcd = "EMPSTAT";
			if strip(prostat1) = "NOT EMPLOYED" then scorres = strip(prostat1);
			else if strip(prostat1) = "WORKING FULL-TIME" then scorres = "FULL-TIME";
			else if strip(prostat1) = "WORKING PART-TIME" then scorres = "PART-TIME";
			else scorres = strip(prostat1);
			scstresc = scorres;
			output;
		end;
	end;
	
	if not missing(sc_r2) then do;
		srel = strip(sc_r2);
        apid = strip(subjid) || "-" || strip(srel);
		if not missing(hedl2) then do;
			sctest   = "Level of Education Attained";
			sctestcd = "EDULEVEL";
			scorres  = compress(strip(hedl2), '-');
			scstresc = compress(strip(hedl2), '-');
			output;
		end;
		if not missing(prostat2) then do;
			sctest   = "Employment Status";
			sctestcd = "EMPSTAT";
			if strip(prostat2) = "NOT EMPLOYED" then scorres = strip(prostat2);
			else if strip(prostat2) = "WORKING FULL-TIME" then scorres = "FULL-TIME";
			else if strip(prostat2) = "WORKING PART-TIME" then scorres = "PART-TIME";
			else scorres = strip(prostat2);
			scstresc = strip(scorres);
			output;
		end;
	end;
	keep studyid domain subjid rsubjid apid srel scorres scstresc sctestcd sctest visit visitnum visitdy sourceid;
run;

proc sort data=work.apsc1;
    by apid sctestcd;
run;

proc sql;
    create table _apid_dups as
    select apid, sctestcd, count(*) as n
    from work.apsc1
    group by apid, sctestcd;
quit;

proc sort data=_apid_dups;
    by apid sctestcd;
run;

data work.&sdtm_domain.;
    merge work.apsc1(in=a)
          _apid_dups;
    by apid sctestcd;
    retain seq;
    if first.sctestcd then seq = 0;
    seq + 1;
    if n > 1 then apid = cats(apid, seq);
    drop n seq;
    if srel = 'NON-BIOLOGICAL FATHER' then srel = "FATHER, NON-BIOLOGICAL";
    else if  srel = 'NON-BIOLOGICAL MOTHER' then srel = "MOTHER, NON-BIOLOGICAL";
    else if  srel = 'BIOLOGICAL FATHER' then srel = "FATHER, BIOLOGICAL";
    else if  srel = 'BIOLOGICAL MOTHER' then srel = "MOTHER, BIOLOGICAL";
    else if  index(srel, "LEGAL") then srel = "LEGALLY AUTHORIZED REPRESENTATIVE";
    label visitdy = "Planned Study Day of Visit";
run;

/* Finalize */
%sdtm_create_domain_v2 ( domain      = &sdtm_domain.
                    , inset          = &sdtm_domain.
                    , keys           = studyid apid sctestcd
                    , deldupkeys     = N
                    , addsuppvars    = VISITDY
                    , metavarsset    = metadata.sdtm_study_variables
                    , metadomainsset = metadata.sdtm_study_domains);