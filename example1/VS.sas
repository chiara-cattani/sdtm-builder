/*/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : VS.sas
 PURPOSE       : Create SDTM data for Vital Signs
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-03 - Chiara Cattani - Initial program (copied from SONA and adjusted)
 2025-12-12 - Chiara Cattani - Populated VSDTC and VSDY for assessments at birth.
 							   Added label to VSEVAL.
 2025-12-18 - Chiara Cattani - Removed update from VSDTC and VSDY at birth.
 2025-12-22 - Chiara Cattani - Updated record for LENHEI_ND = NOT DONE. 
 2026-01-28 - Chiara Cattani - Updated decimal precision for VSORRES.
 2026-02-04 - Chiara Cattani - Derive VSLOBXFL with macro.
******************************************************************************/

/* Configuration */
%nutricia_config;

/* Parameters */
%let sdtm_domain = VS;

/* Import source data */
%copy_import(inset = an);
%copy_import(inset = sc);
%copy_import(inset = review_status);

/* Keep variables required */
data work.an1;
  merge work.an_1 (in=a) sdtm.dm(keep = usubjid age);
  by usubjid;
  if a;
  keep usubjid age subjectid eventid an_ndnd anreasnd andat aninit
       weight weight_u weight_ndnd weight_ndr
       an_lh lenhei_ndnd lenhei_ndr
       height height_u
       length length_u
       hdcirc hdcirc_u hdcirc_ndnd hdcirc_ndr;
run;

data work.sc1;
  set work.sc_1;
  keep usubjid subjectid eventid
       bweight bweightu bweigndnd
       blength blengthu blengndnd;
run;

/* Derive VS records from AN */
data work.an2;
  length vscat vsstat vsdtc vsreasnd vstestcd vseval vsevalid vsorres vsorresu vspos
  		 studyid domain usubjid vstest vsstresc vsstresu visit subjid sourceid $200
         vsstresn visitnum 8;
  set work.an1;

  /* Assigned variables */
  studyid  = "&studyid.";
  domain   = "&sdtm_domain.";
  vscat    = "ANTHROPOMETRICS";
  subjid   = strip(subjectid);
  sourceid = catx(" ", "CRF:", put("AN", $form.));
  %derive_visit_vars();
  %sdtm_dt2dtc(dt = andat, dtc = vsdtc);
  vseval   = "INVESTIGATOR";
  vsevalid = aninit;
    
  /* Measurements done */
  if missing(an_ndnd) then do;

    /* Weight */
    vstest   = "Weight";
    vstestcd = "WEIGHT";
    if upcase(weight_ndnd) ^= "NOT DONE" then do;
      if not missing(weight) then do;
        if not missing(weight_u) and strip(lowcase(weight_u)) = 'kg' then vsorres = strip(put(weight, 8.1));
        else if not missing(weight_u) and strip(lowcase(weight_u)) = 'g' then vsorres = strip(put(weight, best.));
        vsstresc  = vsorres;
        vsstresn  = input(vsstresc, best.);
        vsorresu  = strip(weight_u);
        vsstresu  = vsorresu;
        vsstat    = "";
        vsreasnd  = "";
        vspos     = "";
      end;
      else call missing(vsorres, vsstresc, vsstresn, vsorresu, vsstresu, vsstat, vsreasnd, vspos);
    end;
    else do;
      vsstat   = "NOT DONE";
      vsreasnd = weight_ndr;
      call missing(vsorres, vsstresc, vsstresn, vsorresu, vsstresu, vspos);
    end;
    output;

    /* Height */
    vstest   = "Height";
    vstestcd = "HEIGHT";
    if upcase(lenhei_ndnd) ^= "NOT DONE" then do;
      if not missing(height) then do;
        vspos     = an_lh;
        if upcase(vspos) = "LYING" then vspos = "SUPINE";
        vsorres   = strip(put(height, 8.1));
        vsstresc  = vsorres;
        vsstresn  = input(vsstresc, best.);
        vsorresu  = strip(height_u);
        vsstresu  = vsorresu;
        vsstat    = "";
        vsreasnd  = "";
        output;
      end;
    end;
    else if age > 24 then do;
      vsstat = "NOT DONE";
      vsreasnd = lenhei_ndr;
      call missing(vspos, vsorres, vsstresc, vsstresn, vsorresu, vsstresu);
      output;
    end;
    
    /* Length */
    vstest   = "Body Length";
    vstestcd = "BODLNGTH";
    if upcase(lenhei_ndnd) ^= "NOT DONE" then do;
      if not missing(length) then do;
        vspos     = an_lh;
        if upcase(vspos) = "LYING" then vspos = "SUPINE";
        vsorres   = strip(put(length, 8.1));
        vsstresc  = vsorres;
        vsstresn  = input(vsstresc, best.);
        vsorresu  = strip(length_u);
        vsstresu  = vsorresu;
        vsstat    = "";
        vsreasnd  = "";
        output;
      end;
    end;
    else if age <= 24 then do;
      vsstat = "NOT DONE";
      vsreasnd = lenhei_ndr;
      call missing(vspos, vsorres, vsstresc, vsstresn, vsorresu, vsstresu);
      output;
    end;

    /* Head Circumference */
    vstest   = "Head Circumference";
    vstestcd = "HDCIRC";
    if upcase(hdcirc_ndnd) ^= "NOT DONE" then do;
      if not missing(hdcirc) then do;
        vsorres   = strip(put(hdcirc, 8.1));
        vsstresc  = vsorres;
        vsstresn  = input(vsstresc, best.);
        vsorresu  = strip(hdcirc_u);
        vsstresu  = vsorresu;
        vsstat    = "";
        vsreasnd  = "";
        vspos     = "";
      end;
      else call missing(vsorres, vsstresc, vsstresn, vsorresu, vsstresu, vsstat, vsreasnd, vspos);
    end;
    else do;
      vsstat   = "NOT DONE";
      vsreasnd = hdcirc_ndr;
      call missing(vsorres, vsstresc, vsstresn, vsorresu, vsstresu, vspos);
    end;
    output;
    
  end;

  /* Measurements not done */
  else do;
    vstest   = "All Vital Signs Tests";
    vstestcd = "VSALL";
    vsstat   = "NOT DONE";
    vsreasnd = strip(anreasnd);
    output;
  end;
run;

/* Add RFSTDTC and RFXSTDTC to derive VSDY */
proc sql;
  create table work.an4 as
  select a.*, b.rfstdtc, b.rfxstdtc
  from work.an2 as a
  left join sdtm.dm as b
  on a.usubjid = b.usubjid;
quit;

data work.an5;
  length vsdy 8.;
  set work.an4;
  %sdtm_dtc2dy(dtc = vsdtc, dy = vsdy, rfdtc = rfstdtc, subjid = usubjid);
run;

/* Derive VS records from SC */
data work.sc2;
  length vscat vsstat vstestcd vsorres vsorresu vsevintx
  		 studyid domain usubjid vstest vsstresc vsstresu visit subjid sourceid $200
         vsstresn visitnum 8;
  set work.sc1;

  /* Assigned variables */
  studyid  = "&studyid.";
  domain   = "&sdtm_domain.";
  vscat    = "ANTHROPOMETRICS";
  subjid   = strip(subjectid);
  sourceid = catx(" ", "CRF:", put("SC", $form.));
  %derive_visit_vars();
  vsevintx = "BIRTH";
    
    /* Birth weight */
    vstest   = "Weight";
    vstestcd = "WEIGHT";
    if upcase(bweigndnd) ^= "NOT DONE" then do;
      if not missing(bweight) then do;
        if not missing(bweightu) and strip(lowcase(bweightu)) = 'kg' then vsorres = strip(put(bweight, 8.1));
        else if not missing(bweightu) and strip(lowcase(bweightu)) = 'g' then vsorres = strip(put(bweight, best.));
        vsstresc  = strip(put((bweight / 1000), best.));
        vsstresn  = input(vsstresc, best.);
        vsorresu  = strip(bweightu);
        vsstresu  = "kg";
        vsstat    = "";
      end;
      else call missing(vsorres, vsstresc, vsstresn, vsorresu, vsstresu, vsstat);
    end;
    else do;
      vsstat   = "NOT DONE";
      call missing(vsorres, vsstresc, vsstresn, vsorresu, vsstresu);
    end;
    output;

    /* Birth length */
    vstest   = "Body Length";
    vstestcd = "BODLNGTH";
    if upcase(blengndnd) ^= "NOT DONE" then do;
      if not missing(blength) then do;
        vsorres   = strip(put(blength, 8.1));
        vsstresc  = vsorres;
        vsstresn  = input(vsstresc, best.);
        vsorresu  = strip(blengthu);
        vsstresu  = vsorresu;
        vsstat    = "";
      end;
      else call missing(vsorres, vsstresc, vsstresn, vsorresu, vsstresu, vsstat);
    end;
    else do;
      vsstat   = "NOT DONE";
      call missing(vsorres, vsstresc, vsstresn, vsorresu, vsstresu);
    end;
    output;
run;

/* Add RFSTDTC and RFXSTDTC to derive VSDY */
proc sql;
  create table work.sc3 as
  select a.*, b.rfstdtc, b.rfxstdtc, "" as VSDTC
  from work.sc2 as a
  left join sdtm.dm as b
  on a.usubjid = b.usubjid;
quit;

data work.sc4;
  length vsdy 8.;
  set work.sc3;
  %sdtm_dtc2dy(dtc = vsdtc, dy = vsdy, rfdtc = rfstdtc, subjid = usubjid);
run;

data vs_final;
  set an5 sc4;
  label VSEVAL = "Evaluator";
run;

/* Derive VSLOBXFL */
%derive_lobxfl(
    domain=VS,
	inds=vs_final,
    outds=work.&sdtm_domain.,
    subjid=USUBJID,
    testcd=VSTESTCD,
    dtc=VSDTC,
    orres=VSORRES,
    stat=VSSTAT,
    dm_ds=sdtm.dm,
    base_before_exp=Y
);

/* Finalize */
%sdtm_create_domain_v2(
  domain     = &sdtm_domain,
  inset      = &sdtm_domain,
  keys       = studyid usubjid vstestcd visitnum vsdtc,
  addsuppvars    = VSEVAL,
  metavarsset    = metadata.sdtm_study_variables,
  metadomainsset = metadata.sdtm_study_domains,
  deldupkeys = N  
);