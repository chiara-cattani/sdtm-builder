/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : /SAS/NUTRI-IRON/Files/15 SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : AE.sas
 PURPOSE       : Create SDTM data for Adverse Events
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-01 - Kirill Novichkov - Initial version
 2025-12-08 - Kirill Novichkov - v1.1: Log was cleared
 2025-12-10 - Kirill Novichkov - v1.2: AESOCLST added, AESTPER dropped
 2026-01-06 - Kirill Novichkov - v1.3: AEENRTPT = "BEFORE" derivation fixed
 2026-01-09 - Kirill Novichkov - v1.4: AESDTH, AESLIFE, AESHOSP, AESDISAB, AESCONG and AESMIE added
 2026-01-29 - Kirill Novichkov - v1.5: AEACNOTH, AEREL and AESTRTPT derivation updated; 
                                       audit variables (AEINITD, AELTEDID and AECODED) added 
 2026-01-30 - Chiara Cattani   - Minor update to AESTRTPT.
 2026-02-02 - Chiara Cattani   - Minor update to audit variables format.
******************************************************************************/

%nutricia_config;

%let domain = %unquote(&_progname);

%copy_import (inset=ae, by=subjectid aespid);
%copy_import (inset=sae, by=subjectid saespid);
%copy_import (inset=ae_meddra, by=subjectid aespid);

* Merge all raw AE related datasets into a single dataset ;
data ae_all;
  merge ae_meddra_1 sae_1 (keep=subjectid sae: aesdth aeslife aeshosp aesdisab aescong aesmie rename=saespid=aespid);
  by subjectid aespid; 
run;

%get_meddra_version(indata = ae_all);

data &domain. (drop=aestper); 
  length aeacn aerel aerelp aedict aesoclst aeacnoth $200; * to add ' RELATED' later to be consistent with codelist ;
  label aesoclst = "Primary and Secondary SOC Abbreviations";
  merge ae_all (rename=(subjectid=subjid aespid=_aespid aeacns0=_aeacns0 aeacnssp=_aeacnssp) drop=usubjid in=a) 
        sdtm.dm (keep=subjid studyid usubjid rfstdtc);
  by subjid;
  if a;
  
  call missing(aeseq);
  
  domain    = "&domain";
  aespid    = put(_aespid, best.-l);
  sourceid  = "CRF: " !! put(domain, $form.);  
  aellt     = llt_name;
  aelltcd   = input(llt_code, best.);
  aedecod   = pt_name;
  aedict    = ifc(^missing(aedecod), "MedDRA &meddra_version_data.", " ");
  aeptcd    = input(pt_code, best.);
  aehlt     = hlt_name;
  aehltcd   = input(hlt_code, best.);
  aehlgt    = hlgt_name;
  aehlgtcd  = input(hlgt_code, best.);
  aebodsys  = soc_name;
  aebdsycd  = input(soc_code, best.);
  aesoc     = pt_soc_name;
  aesoccd   = input(pt_soc_code, best.);
  aesoclst  = soc_list; 
  aeacn     = tranwrd(aeacnp, "NA", "NOT APPLICABLE");
  aeacnoth  = ifc(^missing(_aeacns0), _aeacns0, catx("; ", of aeacns:));
  aeacnotx  = _aeacnssp;
  aeacnx    = aeacnpsp;
  aerelx    = aerel1co;
  aerelp    = tranwrd(aerel2, "NA", "NOT APPLICABLE");
  aerelpx   = aerel2co;
  aeoutx    = aesequel1;
  aedesc    = aecoval;
  aefuneed  = aefu;
  aeout     = tranwrd(aeout, " / ", "/");
  
  aerel     = tranwrd(aerel1, "NA", "NOT APPLICABLE");
  if aerel ^in (" ", "NOT RELATED", "NOT APPLICABLE") then aerel = catt(aerel, " RELATED");
  
  %sdtm_dt2dtc (dt=aestdat, dtc=aestdtc);
  %sdtm_dt2dtc (dt=aeendat, dtc=aeendtc);
  
  %sdtm_dtc2dy(dtc= aestdtc);
  %sdtm_dtc2dy(dtc= aeendtc);
  
  * AESTRTPT + AESTTPT ;
  if ^missing(aestper) then do;
  	if index(upcase(aestper), "BEFORE") then aestrtpt = "BEFORE";
  	else if index(upcase(aestper), "AFTER") then aestrtpt = "AFTER";
  	aesttpt = "FIRST PRODUCT INTAKE";
  end;
  
  * AEENRTPT + AEENTPT ;
  if ^missing(aeongo) then do;
    if aeongo = "Y" then aeenrtpt = "ONGOING";
    else if aeongo = "N" then aeenrtpt = "BEFORE";
    aeentpt = "END OF STUDY";
  end;
  
  /* Audit variables */
  %sdtm_dt2dtc(dt = CodedOnDate,dtc = aecoded);
  %sdtm_dt2dtc(dt = InitiatedDate,dtc = aeinitd);
  %sdtm_dt2dtc(dt = LastEditedDate,dtc = aeltedid);
run;

* Get the list of key variables from the metadata ;
proc sql noprint;  
  select tranwrd(compress(keys), ',', ' ') into: keyvars from metadata.sdtm_study_domains where domain="&domain";
quit;

* Apply metadata and store into SDTM folder ;
%sdtm_create_domain_v2 ( domain           = &domain.
                        , inset           = &domain.
                        , keys            = &keyvars.
                        , deldupkeys      = N
                        , metavarsset     = metadata.sdtm_study_variables
                        , metadomainsset  = metadata.sdtm_study_domains
                        , addsuppvars     = aesoclst );