# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_TI.R 
# PURPOSE       : QC SDTM data for Trial Inclusion/Exclusion Criteria
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-01 - cattanch - Initial program
# ******************************************************************************

# Configuration ----
library(nutriciaconfig)
nutricia_config()

library(dplyr)
library(haven)
library(purrr)
library(readxl)
library(rlang)
library(sdtm.oak)
library(stringr)
library(tibble)
library(tidyr)

# Set working directory ----
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  # Running in RStudio
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # Running in LSAF
  setwd(progdir)
}

# Set parameters
study       <- "SONA"
sdtm_domain <- "TI"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
ie1 <- copy_import(inset = "ie")

# Criteria ----
incl_text <- c(
  "Male or female subject aged >= 60 years (bound included).",
  "BMI between 18.5-29.9 kg/m2 (bounds included), based on the calculations of the investigator.",
  "In good health considering the subject's age (e.g., potentially with stable hypertension or hypercholesterolemia or prediabetes), to the discretion of the investigator.",
  "Self-reported regular bowel movements (>= 3 days/week and <= 3 movements/day).",
  "Subject able and willing to adhere to protocol, i.e., willing and able to not use prebiotic, probiotic or synbiotic supplements and to maintain physical activity and dietary habits during the study (i.e., days 1-42), to handle and consume the study product according to instructions for 4 weeks, to complete the study questionnaires/diaries and to collect their stool samples with the collection kits provided by the investigator, store the kits in their freezer and bring these to the site at four time points (V1-V4).",
  "Subject able to speak, read and understand the local language to communicate with the site staff and to comply with the study instructions.",
  "Signed Informed Consent Form (ICF)."
)

excl_text <- c(
  "Subject that is not in good health, defined as having ongoing chronic metabolic diseases (e.g., diabetes), inflammatory diseases (e.g., kidney or auto-immune disease, benign or malignant tumors or other cancers, hepatitis, chronic obstructive pulmonary disease (COPD) II or III), asthma or allergy requiring chronic systemic treatment (daily or on a regular schedule), GI diseases, eating or psychotic disorders, other psychiatric disorders under medication-based treatment (e.g., autism spectrum disorder (ASD), mood, anxiety, personality disorders), neurological disorders (e.g., Alzheimer's or Parkinson's disease), diagnosed by a physician.",
  "Self-reported (acute) diarrhea within 2 weeks before screening and enrolment and/or V1.",
  "Any antecedents of digestive surgery, or (plan for) such a surgery during the study (i.e., days 1-42) or within 4 weeks after the end of intervention.",
  "Subject that plans to have any surgery or intervention requiring a general anaesthesia during the study (i.e., days 1-42).",
  "Any medical condition or disturbances for which probiotic use is not recommended (e.g., immunocompromised individuals, presence of blood in stool or other signs/symptoms indicating severe GI disturbances, central venous catheter, open wounds following major surgery).",
  "Any medical condition or disturbances for which high protein intake is not recommended (e.g., renal insufficiency).",
  "Any medical conditions or disturbances that require fibre-free diet.",
  "Galactosaemia or known allergies or intolerances to the ingredients of the study product or allergens in the Product and Reference Safety Information (e.g., fish allergy, soy allergy, fibre allergy, cow's milk protein allergy or lactose intolerance).",
  "Medication that affects the GI function or stomach (e.g., opioids, antibiotics, intestinal antiseptics, systemic steroids or corticosteroids, systemic beta sympathomimetics, metformin, antidepressants (tricyclic or SSRIs), laxatives, anti-diarrhoea medication, prokinetics/dopamine antagonists, proton pump inhibitors (PPIs), antiacids, antihistamine2 blockers, antihistamines) within 8 weeks before screening and enrolment or (plan to) use of any of these during the study (i.e., days 1-42).",
  "Non-incidental use or use of > 4 paracetamol tablets [500 mg], NSAIDs or aspirin within 2 weeks before screening and enrolment or plan to use > 4 paracetamol tablets [500 mg], NSAIDs or aspirin during a 2-week period during the study (i.e., > 4 tablets/pills during days (-14)-0, > 4 tablets/pills during days 1-14, > 4 tablets/pills during days 15-28, > 4 tablets/pills during days 29-42).",
  "Other medications (e.g., ACE inhibitors, statins) that are not on stable regime, e.g., started or stopped their use within 8 weeks before screening and enrolment or (plan to) start or stop such medication during the study (i.e., days 1-42).",
  "Use of prebiotic, probiotic or synbiotic supplement or other fibre supplements (e.g., fibre gummies) within 2 weeks before screening and enrolment or (plan for) such use during the study (i.e., days 1-42).",
  "Subjects with self-reported high daily calcium and vitamin D intake from vitamin supplements (> 500 mg calcium and > 40 ug vitamin D), or (plan for) high daily doses during the study (i.e., days 1-42).",
  "Self-reported special diets (e.g., vegan/vegetarian, gluten-free, ketogenic, low-FODMAP, intermitted fasting) or other prescribed/recommended special diet regime or (plan for) such a diet during the study (i.e., days 1-42).",
  "Self-reported significant change in subject's dietary and/or physical activity habits (e.g., new diet, increased/decreased calorie intake, change in vitamin or mineral supplements, new sports) within 2 weeks before screening and enrolment or (plan for) such change during the study (i.e., days 1-42).",
  "Alcohol or drug abuse (i.e., > 14 weekly alcohol units for women, > 21 weekly alcohol units for men, or at least monthly drug use), based on investigator's judgement.",
  "Subject involved in another clinical study with investigational or marketed products within 4 weeks before screening and enrolment or (plan for) involvement in such a study during this study (i.e., days 1-42).",
  "Employees and/or children/family members or relatives of employees of Danone or the participating sites, medical, pharmacy and nursing students, subordinate laboratory personnel, persons kept in detention."
)

# Codes ----
pad2 <- function(i) sprintf("%02d", i)
incl_codes <- paste0("INCL", pad2(seq_along(incl_text)))
excl_codes <- paste0("EXCL", pad2(seq_along(excl_text)))

# TI ----
ti1 <- bind_rows(
  tibble(studyid = study,
         domain = sdtm_domain,
         iecat = "EXCLUSION", 
         ietestcd = excl_codes,
         ietest = excl_text),
  tibble(studyid = study,
         domain = sdtm_domain,
         iecat = "INCLUSION",
         ietestcd = incl_codes,
         ietest = incl_text)
)

# Check: does IE structure match these counts?
nm  <- names(ie1)
num <- function(x) suppressWarnings(as.integer(gsub("\\D", "", x)))
max_in_ie <- {
  m <- grepl("iein", nm, ignore.case = TRUE) & !grepl("ieintestcd2", nm, ignore.case = TRUE)
  x <- num(nm[m]); ifelse(all(is.na(x)), 0L, max(x, na.rm = TRUE))
}
max_ex_ie <- {
  m <- grepl("ieex", nm, ignore.case = TRUE)
  x <- num(nm[m]); ifelse(all(is.na(x)), 0L, max(x, na.rm = TRUE))
}

if (length(incl_codes) != max_in_ie)
  warning("TI: inclusion count mismatch (TI=", length(incl_codes),
          " vs IE=", max_in_ie, ").")
if (length(excl_codes) != max_ex_ie)
  warning("TI: exclusion count mismatch (TI=", length(excl_codes),
          " vs IE=", max_ex_ie, ").")

# Finalize ----
ti <- sdtm_create_domain(ti1, keys = "STUDYID, IETESTCD")
