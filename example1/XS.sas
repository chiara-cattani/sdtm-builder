/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : XS.sas
 PURPOSE       : Create SDTM data for Serious Adverse Events
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-04 - Chiara Cattani - Initial program (copied from SONA and adjusted)
******************************************************************************/

/* Configuration */
%nutricia_config;
%let sdtm_domain = %unquote(&_progname);

/* Import source data */
%copy_import(inset = sae);

/* Step 1: Initial SAE Mapping */
data work.sae1;
  length studyid domain subjid xsspid xsageu xsstdtc xsenrtpt xsentpt xsendtc xssex $200 xsage 8;
  set work.sae_1(
    keep = usubjid subjectid saespid saeterm aesstdat aesongo aesendat
           aesage aesageu aesdth aeslife aeshosp aesdisab aescong aesmie
           aesaddat aesdidat aesdtdat aesdtres aesautop aesdthct
           saeacnp aesfpi aesdose aesrddat aesrdosf aesrddot aessex aescb
           aesindat aesindof aesindot aesitdat: aesrsdat: aerein: aesabate aesrecur
  );
  where ^missing(saeterm);

  studyid  = "&studyid.";
  domain   = "&sdtm_domain";
  subjid   = subjectid;

  xsterm   = strip(saeterm);
  xsspid   = strip(put(saespid, best.));

  if lowcase(aesongo) = "y" then xsenrtpt = "ONGOING";
  else if lowcase(aesongo) = "n" then xsenrtpt = "BEFORE";
  else call missing(xsenrtpt);
  if not missing(aesongo) then xsentpt  = "END OF STUDY";

  %sdtm_dt2dtc(dt = aesstdat, dtc = xsstdtc);
  %sdtm_dt2dtc(dt = aesendat, dtc = xsendtc);

  xssex = aessex;
  xsage = aesage;
  if ^missing(xsage) then xsageu = strip(aesageu);

  drop saeterm aesage aesage aesageu aesongo aesstdat aesendat;
run;

/* Step 2: Mapping */
data work.sae2;
  length xssdth xsslife xsshosp xssdisab xsscong xssmie xsadmdtc xsdisdtc xsdthdtc xsautop xsdthct
  xsacn xsfpidtc xsrddtc xsindtc xsit1dtc xsrs1dtc xsrit1 xsit2dtc xsrs2dtc xsrit2 xsit3dtc xsrs3dtc
  xseabate xsereapp xsrddosf xsrddost xsindosf xsindost xstrtdos xsbroken $200;
  set work.sae1;

  xssdth   = strip(aesdth);
  xsslife  = strip(aeslife);
  xsshosp  = strip(aeshosp);
  xssdisab = strip(aesdisab);
  xsscong  = strip(aescong);
  xssmie   = strip(aesmie);

  %sdtm_dt2dtc(dt = aesaddat, dtc = xsadmdtc);
  %sdtm_dt2dtc(dt = aesdidat, dtc = xsdisdtc);
  %sdtm_dt2dtc(dt = aesdtdat, dtc = xsdthdtc);

  xsdthrsn = strip(aesdtres);
  xsautop  = strip(aesautop);
  xsdthct  = strip(aesdthct);

  if lowcase(saeacnp) = "na" then xsacn = "NOT APPLICABLE";
  else xsacn = strip(saeacnp);

  %sdtm_dt2dtc(dt = aesfpi, dtc = xsfpidtc);
  xstrtdos = strip(aesdose);
  xsbroken = strip(aescb);

  %sdtm_dt2dtc(dt = aesrddat, dtc = xsrddtc);
  xsrddosf = strip(put(aesrdosf, best.));
  xsrddost = strip(put(aesrddot, best.));

  %sdtm_dt2dtc(dt = aesindat, dtc = xsindtc);
  xsindosf = strip(put(aesindof, best.));
  xsindost = strip(put(aesindot, best.));

  %sdtm_dt2dtc(dt = aesitdat, dtc = xsit1dtc);
  %sdtm_dt2dtc(dt = aesrsdat, dtc = xsrs1dtc);
  xsrit1 = strip(aerein);

  %sdtm_dt2dtc(dt = aesitdat2, dtc = xsit2dtc);
  %sdtm_dt2dtc(dt = aesrsdat2, dtc = xsrs2dtc);
  xsrit2 = strip(aerein2);

  %sdtm_dt2dtc(dt = aesitdat3, dtc = xsit3dtc);
  %sdtm_dt2dtc(dt = aesrsdat3, dtc = xsrs3dtc);

  xseabate = strip(aesabate);
  xsereapp = strip(aesrecur);
run;

/* Step 6: Add RFSTDTC and derive --DY variables */
proc sql;
  create table work.sae3 as
  select a.*, b.rfstdtc
  from work.sae2 as a
  left join sdtm.dm as b
  on a.usubjid = b.usubjid;
quit;

data &sdtm_domain;
  length sourceid $200 xsstdy xsendy 8;
  set work.sae3;

  %sdtm_dtc2dy(dtc = xsstdtc, dy = xsstdy);
  %sdtm_dtc2dy(dtc = xsendtc, dy = xsendy);

  sourceid = catx(' ', "CRF:", put("SAE", $form.));
run;

/* Finalize */
%sdtm_create_domain_v2(
  domain     = &sdtm_domain,
  inset      = &sdtm_domain,
  keys       = studyid usubjid xsterm xsstdtc xsendtc xsspid,
  deldupkeys = N,
  metavarsset    = metadata.sdtm_study_variables,
  metadomainsset = metadata.sdtm_study_domains,

  delperm    = N
);
