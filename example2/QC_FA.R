# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_FA.R
# PURPOSE       : QC SDTM data for Findings About (FA)
# ------------------------------------------------------------------------------
# NOTES :
#   - Builds FA from:
#       1) GIQ (BSFS form): stool records (ST7P1-8) and sample flags (SAMPYN1-8)
#       2) MyFood24: dietary records exploded into multiple nutrient FA records
# ******************************************************************************
# PROGRAM HISTORY :
# 2025-10-13 - Vikas Shokeen - Initial version
# 2025-10-17 - cattanch      - Updated FALNKID to include FASPID
# 2025-12-17 - Vikas Shokeen - Added MyFood24 FA records 
# 2026-01-14 - Vikas Shokeen - Added FALNKID, FALNKGR2 and FALNKGRP 
# ******************************************************************************

# --- Configuration -------------------------------------------------------------

library(nutriciaconfig)
nutricia_config()

library(readxl)
library(haven)
library(sdtm.oak)
library(dplyr)
library(tidyr)
library(stringr)
library(rlang)

# Set working directory ----
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  # Running in RStudio
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  # Running in LSAF
  setwd(progdir)
}

# --- Parameters ---------------------------------------------------------------
study       <- "SONA"
sdtm_domain <- "FA"

# Metadata (if/when needed)
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# ------ Sources ---------------------------------------------
rs1 <- copy_import(inset = "review_status")

sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# --- Input  ------------------------------------------
dm <- read_sas("../41 SDTM Data/dm.sas7bdat")
names(dm) <- toupper(names(dm))
dm_ref <- dm %>% select(USUBJID, RFSTDTC)

giq <- copy_import(inset = "giq")
names(giq) <- tolower(names(giq))

# LBS is required for FALNKID validation
lbs <- copy_import(inset = "lbs")
names(lbs) <- tolower(names(lbs))

myfood24 <- copy_import_myfood24(inset = "myfood24")
names(myfood24) <- tolower(names(myfood24))

# =============================================================================
# PART 1: DATA FROM GIQ (BSFS)
# =============================================================================

giq <- giq %>%
  select(
    usubjid, subjectid, activityid, eventid, gidat,
    matches("^st7p\\d+$"), matches("^sampyn\\d+$")
  )

# keep just IDs + the suffixed fields to avoid name collisions
fa_long <- giq %>%
  select(
    usubjid, subjectid, activityid, eventid, gidat,
    matches("^(st7p|sampyn)\\d+$")
  ) %>%
  # one pivot to handle BOTH st7p* and sampyn* at once
  pivot_longer(
    cols = matches("^(st7p|sampyn)\\d+$"),
    names_to = c(".value", "k"),
    names_pattern = "^(st7p|sampyn)(\\d+)$"
  ) %>%
  transmute(
    USUBJID     = as.character(usubjid),
    SUBJECTID   = as.character(subjectid),
    ACTIVITYID  = as.character(activityid),
    EVENTID     = as.character(eventid),
    GIDAT       = gidat,
    FASPID      = as.character(k),   # SAS: strip(put(i,best.)) => no leading blanks
    FAORRES     = st7p,              # BSFS answer (st7p*)
    SAMPYN_VAL  = sampyn             # sample yes/no (sampyn*)
  ) %>%
  # keep rows where at least one side was present
  filter(!(is.na(FAORRES) & is.na(SAMPYN_VAL)))

fa1 <- fa_long %>%
  mutate(
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    SUBJID   = SUBJECTID,
    FADTC    = sub("T00:00(:00)?$", "", as.character(GIDAT)),
    FACAT    = "GASTROINTESTINAL SYMPTOMS",
    FASCAT   = "DEFECATION",
    FAOBJ    = "STOOL",
    FAEVAL   = "STUDY SUBJECT",
    FAORIG   = "pPRO",
    FATESTCD = "BSFS",
    FATEST   = "Bristol Stool Form Scale",
    FAEVINTX = "TODAY",
    SOURCEID = paste("CRF:", sources$formname[match("GIQ", sources$formid)]),
    FASTRESC = as.character(FAORRES)
  )

#------------ Derive Timepoint Variables ----------------------------
fa2 <- derive_tpt_vars(fa1, eventid_var = "ACTIVITYID", sdtm = "FA")


#------------ FADY using DM ---------------------------------------------------
fa3 <- sdtm.oak::derive_study_day(
  sdtm_in       = fa2,
  dm_domain     = dm_ref,
  tgdt          = "FADTC",
  refdt         = "RFSTDTC",
  study_day_var = "FADY",
  merge_key     = "USUBJID"
)

# --- VISIT / VISITNUM mapping from EVENTID (BL/A1–A4) ------------
fa4 <- fa3 %>%
  mutate(
    .ev = toupper(trimws(as.character(EVENTID))),
    VISIT = case_when(
      .ev == "BL" ~ "Visit 1",
      .ev == "A1" ~ "Visit 2",
      .ev == "A2" ~ "Visit 3",
      .ev == "A3" ~ "Visit 3",
      .ev == "A4" ~ "Visit 4",
      TRUE        ~ ""
    ),
    VISITNUM = case_when(
      .ev == "BL" ~ 1,
      .ev == "A1" ~ 2,
      .ev == "A2" ~ 3,
      .ev == "A3" ~ 3,
      .ev == "A4" ~ 4,
      TRUE        ~ NA_real_
    )
  ) %>%
  select(-.ev)

# --- FALNKGR2 = FATPT for stool ---------------------------------
fa4 <- fa4 %>%
  mutate(
    FALNKGR2 = if_else(
      toupper(trimws(as.character(FAOBJ))) == "STOOL" & !is.na(FATPT),
      as.character(FATPT),
      NA_character_
    )
  )

# --- LBS lookup (collected stools per USUBJID + DATE) -------------
# SAS uses stool_perf='y' and stool_dat to build bedate; match on date(FADTC)
lbs_sel_date <- lbs %>%
  transmute(
    USUBJID     = as.character(usubjid),
    stool_perf  = tolower(trimws(as.character(stool_perf))),
    stool_dat   = as.character(stool_dat)
  ) %>%
  filter(stool_perf == "y") %>%
  mutate(
    BEDATE = as.Date(substr(stool_dat, 1, 10))  # assumes ISO-like date first 10 chars
  ) %>%
  filter(!is.na(BEDATE)) %>%
  distinct(USUBJID, BEDATE)

lbs_sel_date2 <- lbs_sel_date %>%
  mutate(BEDATE_join=BEDATE) 

# --- FALNKID only when selected AND LBS collected stool exists ----
fa5 <- fa4 %>%
  mutate(
    FADATE = as.Date(substr(as.character(FADTC), 1, 10))
  ) %>%
  left_join(lbs_sel_date2, by = c("USUBJID" = "USUBJID", "FADATE" = "BEDATE")) %>%
  mutate(
    .sel     = tolower(trimws(as.character(SAMPYN_VAL))) == "y",
    .has_lbs = !is.na(BEDATE_join),
    FALNKID  = case_when(
      .sel & .has_lbs ~ paste(trimws(as.character(VISIT)), trimws(as.character(FADTC)), sep = " - "),
      TRUE            ~ NA_character_
    ),
    
    #VISIT and VISITNUM are not populated in FA
    VISIT = NA_character_ ,
    VISITNUM = NA_real_
  ) %>%
  select(-.sel, -.has_lbs, -BEDATE_join)

# =============================================================================
# PART 2: DATA FROM MyFood24 
# =============================================================================

# --- Step MF1/MF2: base records + date parsing to FADTC + FALNKGRP ------------
mf2 <- myfood24 %>%
  transmute(
    USUBJID  = as.character(usubjid),
    SUBJID   = as.character(subjid),
    SOURCEID = "participant-breakdown.csv",
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    FACAT    = "MYFOOD24",
    FAEVAL   = "STUDY SUBJECT",
    FAORIG   = "ePRO",
    FAOBJ    = str_trim(as.character(food_name)),
    raw1     = str_replace_all(str_trim(as.character(diary_date)), "-", "/"),
    
    # Parse item_added_at: "30/09/25 09:50:43" -> "2025-09-30"
    FAINITD  = {
      x <- str_trim(as.character(item_added_at))
      x <- str_replace_all(x, "-", "/")
      d <- as.Date(x, format = "%d/%m/%y %H:%M:%S")
      format(d, "%Y-%m-%d")
    },
    
    # nutrient vars (lowercase)
    prot, fat, cho, kcals, kj, aoacfib, monofac,
    na, k, ca, mg, p, fe, cu, zn, cl, mn, se, i,
    retequ, vite, vitk1, thia, ribo, niac, tryp60, niacequ,
    vitb6, vitb12, folt, panto, biot, vitc, altret, vitd3,
    fod20_5cn3, fod22_6cn3
  ) %>%
  mutate(
    raw1     = str_replace_all(raw1, "\\s+", ""),
    date_num = case_when(
      nchar(raw1) == 8  ~ suppressWarnings(as.Date(raw1, format = "%d/%m/%y")),
      nchar(raw1) == 10 ~ suppressWarnings(as.Date(raw1, format = "%d/%m/%Y")),
      TRUE              ~ as.Date(NA)
    ),
    FADTC    = if_else(!is.na(date_num), format(date_num, "%Y-%m-%d"), NA_character_)
  ) %>%
  select(-raw1, -date_num) %>%
  arrange(USUBJID, FAOBJ, FADTC) %>%
  group_by(USUBJID) %>%
  mutate(FALNKGRP = as.character(row_number())) %>%
  ungroup()

# Nutrient map 
nutr_map <- tibble::tribble(
  ~var,          ~FATESTCD,    ~FATEST,                                   ~FAORRESU, ~FASTRESU, ~conv,
  "prot",        "DPROT",      "Dietary Protein",                         "g",       "g",       1,
  "fat",         "DFATT",      "Dietary Fat, Total",                      "g",       "g",       1,
  "cho",         "DCARBT",     "Dietary Total Carbohydrate",              "g",       "g",       1,
  "kcals",       "DCAL",       "Dietary Calories",                        "kcal",    "kcal",    1,
  "kj",          "DCAL",       "Dietary Calories",                        "kJ",      "kcal",    0.2390057361,
  "aoacfib",     "DFIBER",     "Dietary Fiber",                           "g",       "g",       1,
  "monofac",     "DFATM",      "Dietary Fat, Monounsaturated",            "g",       "g",       1,
  "na",          "DNA",        "Dietary Sodium",                          "mg",      "mg",      1,
  "k",           "DK",         "Dietary Potassium",                       "mg",      "mg",      1,
  "ca",          "DCA",        "Dietary Calcium",                         "mg",      "mg",      1,
  "mg",          "DMG",        "Dietary Magnesium",                       "mg",      "mg",      1,
  "p",           "DP",         "Dietary Phosphorus",                      "mg",      "mg",      1,
  "fe",          "DFE",        "Dietary Iron",                            "mg",      "mg",      1,
  "cu",          "DCU",        "Dietary Copper",                          "mg",      "mg",      1,
  "zn",          "DZN",        "Dietary Zinc",                            "mg",      "mg",      1,
  "cl",          "DCL",        "Dietary Chloride",                        "mg",      "mg",      1,
  "mn",          "DMN",        "Dietary Manganese",                       "mg",      "mg",      1,
  "se",          "DSE",        "Dietary Selenium",                        "ug",      "ug",      1,
  "i",           "DIODINE",    "Dietary Iodine",                          "ug",      "ug",      1,
  "retequ",      "DVITARE",    "Dietary Retinol Equivalents",             "ug",      "ug",      1,
  "vite",        "DVITE",      "Dietary Vitamin E",                       "mg",      "mg",      1,
  "vitk1",       "DVITK1",     "Dietary Vitamin K1",                      "ug",      "ug",      1,
  "thia",        "DTHIAMIN",   "Dietary Thiamine",                        "mg",      "mg",      1,
  "ribo",        "DRIBFLVN",   "Dietary Riboflavin",                      "mg",      "mg",      1,
  "niac",        "DNIACIN",    "Dietary Niacin",                          "mg",      "mg",      1,
  "tryp60",      "DTRP60",     "Dietary Tryptophan/60",                   "mg",      "mg",      1,
  "niacequ",     "DVITB3EQ",   "Dietary Niacin Equivalents",              "mg",      "mg",      1,
  "vitb6",       "DVITB6",     "Dietary Vitamin B6",                      "mg",      "mg",      1,
  "vitb12",      "DVITB12",    "Dietary Vitamin B12",                     "ug",      "ug",      1,
  "folt",        "DFOLIC",     "Dietary Folic Acid",                      "ug",      "ug",      1,
  "panto",       "DVITB5",     "Dietary Vitamin B5",                      "mg",      "mg",      1,
  "biot",        "DVITB7",     "Dietary Vitamin B7",                      "ug",      "ug",      1,
  "vitc",        "DVITC",      "Dietary Vitamin C",                       "mg",      "mg",      1,
  "altret",      "DVITAAT",    "Dietary All-trans-Retinol",               "ug",      "ug",      1,
  "vitd3",       "DVITD3",     "Dietary Vitamin D3",                      "ug",      "ug",      1,
  "fod20_5cn3",  "DECPT",      "Dietary Eicosapentaenoic Acid",           "g",       "g",       1,
  "fod22_6cn3",  "DDCHX",      "Dietary Docosahexaenoic Acid",            "g",       "g",       1
)

nutr_cols <- nutr_map$var

# --- Step MF3: explode nutrients to FA records 
mf3 <- mf2 %>%
  pivot_longer(
    cols      = any_of(nutr_cols),
    names_to  = "var",
    values_to = "val"
  ) %>%
  filter(!is.na(val) & trimws(as.character(val)) != "") %>%
  left_join(nutr_map, by = "var") %>%
  mutate(
    FAORRES  = as.character(val),
    FASTRESN = as.numeric(val) * conv,
    FASTRESC = substr(trimws(as.character(signif(as.numeric(val) * conv, 12))), 1, 12)
  ) %>%
  transmute(
    STUDYID, DOMAIN, USUBJID, SUBJID, SOURCEID,
    FADTC, FACAT, FAOBJ,
    FATESTCD, FATEST,
    FAORRES, FAORRESU,
    FASTRESC, FASTRESU, FASTRESN,
    FALNKGRP, FAEVAL, FAORIG, FAINITD
  )

# --- Step MF4: derive FADY from DM (same as GIQ) ------------------------------
mf4 <- sdtm.oak::derive_study_day(
  sdtm_in       = mf3,
  dm_domain     = dm_ref,
  tgdt          = "FADTC",
  refdt         = "RFSTDTC",
  study_day_var = "FADY",
  merge_key     = "USUBJID"
)

# =============================================================================
# Combine sources + sequence + finalize
# =============================================================================

fa_all <- bind_rows(fa5, mf4)

add_if_missing <- function(df, name, prototype) {
  if (!name %in% names(df)) df[[name]] <- prototype
  df
}

# Ensure consistent column set across sources (GIQ vs MyFood24)
fa_all <- fa_all %>%
  { add_if_missing(., "FASPID",    as.character(NA)) } %>%
  { add_if_missing(., "FALNKGRP",  as.character(NA)) } %>%
  { add_if_missing(., "FALNKGR2",  as.character(NA)) } %>%
  { add_if_missing(., "FAINITD",   as.character(NA)) } %>%
  { add_if_missing(., "VISIT",     as.character(NA)) } %>%
  { add_if_missing(., "VISITNUM",  as.numeric(NA)) } %>%
  { add_if_missing(., "FATPTREF",  as.character(NA)) } %>%
  { add_if_missing(., "FATPTNUM",  as.numeric(NA)) }

# Derive sequence 
fa6 <- sdtm.oak::derive_seq(
  tgt_dat  = fa_all,
  tgt_var  = "FASEQ",
  rec_vars = c("STUDYID","USUBJID","FATESTCD","FAOBJ","FADTC","FASPID","FALNKGRP","FAORRESU")
)

attr(fa6$FALNKGR2, "label") <- "Link Group ID 2"
attr(fa6$FAINITD, "label") <- "Initiated Date"

FA <- fa6 %>%
  rename_with(toupper) %>%
  sdtm_create_domain(delperm = FALSE, addsuppvars=c("FALNKGR2", "FAINITD"))