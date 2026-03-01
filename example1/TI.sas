/******************************************************************************
 Danone Nutricia Research
 ******************************************************************************
 STUDY/PROJECT : NUTRI-IRON / 24REX0077885
 PROGRAM PATH  : SAS/NUTRI-IRON/Files/SDTM Conversion/40 SDTM Programs
 PROGRAM NAME  : TI.sas
 PURPOSE       : Create SDTM data for Trial Inclusion/Exclusion Criteria
 ------------------------------------------------------------------------------
 NOTES : 
 ------------------------------------------------------------------------------
 PROGRAM HISTORY :
 2025-12-04 - Chiara Cattani - Initial program (copied from SONA and adjusted)
 2026-02-02 - Chiara Cattani - Added TIVERS.
 2026-02-05 - Chiara Cattani - Removed special characters.
******************************************************************************/

/* Configuration */
%nutricia_config;
%let sdtm_domain = %unquote(&_progname);

/*Get number of inclusion and exclusion criteria*/
proc sql noprint;
  select distinct max(input(compress(name, , 'a'),best.)) into: max_in     /* Inclusion */
  from dictionary.columns
  where libname = 'IMPORT' and memname = 'IE' and name like '%IEIN%' and name not like '%IEINTESTCD2%';
  
  select distinct max(input(compress(name, , 'a'),best.)) into: max_ex     /* Exclusion */
  from dictionary.columns
  where libname = 'IMPORT' and memname = 'IE' and name like '%IEEX%';
  
  select distinct count(input(compress(name, , 'a'),best.)) into: max_con  /* Continuation */
  from dictionary.columns
  where libname = 'IMPORT' and memname = 'IE' and name like '%IEINTESTCD2%' and name ne 'IEINTESTCD2';
quit;

data work.ti_1;
  length studyid
         domain
         ietestcd
         iecat 
         tivers $200.;

  /* General variables */
  studyid = "&studyid";
  domain  = "&sdtm_domain";
  tivers = "1.1";

  /* Create IETESTCD for inclusion criteria */
  do i = 1 to &max_in.;
    iecat    = "INCLUSION";
    ietestcd = "INCL"||strip(put(i,z2.));
    output;
  end;
  
  /* Create IETESTCD for exclusion criteria */
  do j = 1 to &max_ex.;
    iecat    = "EXCLUSION";
    ietestcd = "EXCL"||strip(put(j,z2.));
    output;
  end;
  
  drop i j;
  
run;

options noquotelenmax;

/* Derive the IETEST values */
proc format;
  value $ietest
  "INCL01" = "Children aged 1 to 3 years at Visit 0 (V0)."
  "INCL02" = "Healthy children (as per investigator clinical judgement and non-anaemic as confirmed by Hemocue result) or meeting the following criteria for mild anaemia based on Hb levels measured using Hemocue: Children aged 12-23 months: 9.5-10.4 g/dL; Children aged 24-36 months:10.0-10.9 g/dL"
  "INCL03" = "Children familiar with drinking >=440 ml/day of cow's milk and/or (fortified) milk products (in combination with breastfeeding or not) for at least 3 weeks prior to V0."
  "INCL04" = "Children who are expected to take 2-3 servings (of 220 ml each) of study product per day, or to take a total amount of 440-660 ml of study product per day."
  "INCL05" = "Written informed consent provided by parent(s) / legally acceptable representative(s) aged >=18 years at V0."
  
  "EXCL01" = "Low Hb meeting the following criteria for moderate or severe anaemia: Children aged 12-23 months: Hb <= 9.4 g/dL using Hemocue at V0; Children aged 24-36 months: Hb <= 9.9 g/dL using Hemocue at V0."
  "EXCL02" = "Use of iron and/or fibre (e.g. prebiotic) supplementation within 3 months prior to V0."
  "EXCL03" = "Any infection within 2 weeks prior to V0."
  "EXCL04" = "Any other medical condition (including but not limited to autoimmune, inflammatory conditions, tissue injury/trauma, chronic disease (e.g. IBD), severe allergic reaction, post-surgical inflammation) with known high CRP (>5 mg/L) / high AGP (>1 g/L) value within 2 weeks prior to V0."
  "EXCL05" = "The use of medication that is likely to interfere with iron metabolism (such as but not limited to antibiotics [antiviral, antibacterial, antifungal, antiparasitic], anti-regurgitation, anti-reflux or laxative medication) within 2 weeks prior to V0."
  "EXCL06" = "The use of medication to treat iron deficiency or anaemia within 3 months prior to V0."
  "EXCL07" = "Any developmental delays that could impact feeding behavior or growth, or any other condition - reported by parents or diagnosed by the Investigator - that is likely to influence nutritional status or growth, including a weight-for-age and height-for-age WHO z-score outside the range of +/-2 SD at V0."
  "EXCL08" = "Any relevant congenital abnormality, chromosomal disorder or severe disease (such as but not limited to tracheoesophageal fistula, tracheomalacia, major congenital heart disease, Down's syndrome, HIV, cancer)."
  "EXCL09" = "Disorders requiring a special diet (such as food intolerance or food allergy or complaints such as reflux, constipation and cramps for which special toddler formula is required)."
  "EXCL10" = "Any other iron metabolism and related disorders (such as hemoglobinopathies, thalassaemia, hemochromatosis or sideroblastic anaemia)."
  "EXCL11" = "Blood transfusion received within the last 6 months or expected to receive one during the study."
  "EXCL12" = "Vaccination with a live or live-attenuated vaccine received during the last two weeks prior to V0."
  "EXCL13" = "Children with known or suspected medical conditions requiring a special diet (e.g. a fibre-free diet) or special formulae, allergy to foods or product ingredients (e.g. allergy against cow's milk, fish and products thereof, soy, corn, and/or sulphur dioxide and sulfites), or food intolerances (i.e. lactose, galactosemia)."
  "EXCL14" = "Sibling of a child already participating in the study."
  "EXCL15" = "Incapability of child's parents to comply with study protocol as per the judgement of the Investigator or Investigator's uncertainty about the willingness or ability of the parents to comply with the protocol requirements."
  "EXCL16" = "Previous, current, or intended participation in any other study involving investigational or marketed products or nutritional intervention programs."
  "EXCL17" = "Children of employees and/or family members or relatives of employees of Danone, the participating sites, or any other nutrition company that develops infant, follow-on or young child formulae."
;
run;

data work.&sdtm_domain;
  length ietest $1100;
  set work.ti_1;
  
  ietest = strip(put(ietestcd, $ietest.));
  
  /* Apostrophe check */
  if index(ietest, "’") > 0 then put "###WAR" "NING: ietest=" ietest "has a curved apostrohpe.";
  
run;

options quotelenmax;

/* Design is applied and checks are made. */
%sdtm_create_domain_v2 (domain   = &sdtm_domain
                    , inset      = &sdtm_domain
                    , keys       = studyid ietestcd
                    , metavarsset    = metadata.sdtm_study_variables
                    , metadomainsset = metadata.sdtm_study_domains
                    , deldupkeys = N);