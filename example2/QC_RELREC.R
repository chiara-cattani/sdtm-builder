# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_RELREC.R
# PURPOSE       : QC SDTM data for Related Records (RELREC)
# ------------------------------------------------------------------------------
# NOTES :
#   - CM/PR ↔ AE/MH/PR via link columns 
#   - DS ↔ AE via ETAE* (EOS)
#   - XS ↔ AE via AENO* (SAE)
#   - XS ↔ CM via AESCM* (SAE)
#   - Domain-level relations:
#       BE ↔ FA  (RELID=BEFA; IDVAR=BELNKID/FALNKID; RELTYPE=ONE/ONE)
#       EC ↔ EX  (RELID=ECEX; IDVAR=ECLNKGRP/EXLNKID; RELTYPE=MANY/ONE)
#       FA ↔ ML  (RELID=MLFA; IDVAR=FALNKGRP/MLLNKID; RELTYPE=MANY/ONE)
#       FA ↔ CE  (RELID=CEFA; IDVAR=FALNKGR2/CELNKID; RELTYPE=MANY/ONE)
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
#   2025-10-08 - Vikas Shokeen - Initial version
#   2025-10-10 - Vikas Shokeen - Updated working directory (LSAF/RStudio)
#   2025-10-28 - Vikas Shokeen - Updated the RELID and FALNKID
#   2025-10-28 - cattanch      - Updated to set studyid = study
#   2025-12-17 - Vikas Shokeen - Added EC–EX and FA–ML blocks
#   2025-12-18 - Vikas Shokeen - Updated MLLNKID for FA-ML Block
#   2026-01-16 - Vikas Shokeen - Added (domain-level BEFA/ECEX/MLFA/CEFA)
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
library(purrr)


# --- Working directory ---------------------------------------------------------
# If running in RStudio, use the active document directory; otherwise use LSAF
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  setwd(progdir)
}


# --- Metadata imports ----------------------------------------------------------
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")


# --- Parameters ---------------------------------------------------------------
study       <- "SONA"
sdtm_domain <- "RELREC"


# --- Input data ---------------------------------------------------------------
cm_who_drug_1 <- copy_import(inset = "cm_whodrug")
eos_1         <- copy_import(inset = "eos")
pr_meddra_1   <- copy_import(inset = "pr_meddra")
sae_1         <- copy_import(inset = "sae")
giq_1         <- copy_import(inset = "giq")
myfood24_1    <- copy_import_myfood24(inset = "myfood24")


# --- Normalize headers --------------------------------------------------------
names(cm_who_drug_1) <- tolower(names(cm_who_drug_1))
names(eos_1)         <- tolower(names(eos_1))
names(pr_meddra_1)   <- tolower(names(pr_meddra_1))
names(sae_1)         <- tolower(names(sae_1))
names(giq_1)         <- tolower(names(giq_1))
names(myfood24_1)    <- tolower(names(myfood24_1))


# --- Small helper -------------------------------------------------------------
# Return the substring before the first '-' (used to extract SPID-like keys)
first_token <- function(x) sub("^([^\\-]+).*", "\\1", x)


# ============================================================================
# 1) Generic relation builder for CM/AE/PR/MH 
# ----------------------------------------------------------------------------
# Builds bidirectional RELREC rows between rdomain1 and rdomain2.
# - Expects link columns like cmae1, cmmh2, prmh1, etc., excluding *id fields
# - Uses <RDOMAIN1>SPID for main-side IDVAR and the token of link value for
#   related-side IDVARVAL.
# - RELID = <RDOMAIN1><RDOMAIN2><SPID of main> 
# ----------------------------------------------------------------------------
make_relations <- function(df, rdomain1, rdomain2, studyid) {
  df <- df %>% rename_with(tolower)
  r1 <- tolower(rdomain1)
  r2 <- tolower(rdomain2)
  
  link_cols <- names(df)[
    str_detect(names(df), regex(paste0(r1, r2), ignore_case = TRUE)) &
      !str_detect(names(df), regex("id", ignore_case = TRUE))
  ]
  
  spid_col <- paste0(r1, "spid")
  stopifnot(spid_col %in% names(df))
  
  base_keep <- c("usubjid", "subjectid", spid_col, link_cols)
  base_keep <- intersect(base_keep, names(df))
  
  empty_out <- tibble(
    studyid   = character(),
    usubjid   = character(),
    rdomain   = character(),
    subjid    = character(),
    idvar     = character(),
    idvarval  = character(),
    relid     = character(),
    reltype   = character()
  )
  
  if (length(link_cols) == 0) return(empty_out)
  
  long <- df %>%
    select(all_of(base_keep)) %>%
    pivot_longer(
      cols      = all_of(link_cols),
      names_to  = "linkvar",
      values_to = "relval"
    ) %>%
    filter(!is.na(relval) & relval != "")
  
  if (nrow(long) == 0) return(empty_out)
  
  long <- long %>%
    mutate(
      subjid  = as.character(subjectid),
      relid   = paste0(toupper(rdomain1), toupper(rdomain2), as.character(.data[[spid_col]])),
      reltype = ""
    )
  
  main_rows <- long %>%
    transmute(
      studyid  = studyid,
      usubjid  = as.character(usubjid),
      rdomain  = toupper(rdomain1),
      subjid,
      idvar    = paste0(toupper(rdomain1), "SPID"),
      idvarval = as.character(.data[[spid_col]]),
      relid,
      reltype
    )
  
  related_rows <- long %>%
    transmute(
      studyid  = studyid,
      usubjid  = as.character(usubjid),
      rdomain  = toupper(rdomain2),
      subjid,
      idvar    = paste0(toupper(rdomain2), "SPID"),
      idvarval = first_token(relval),
      relid,
      reltype
    )
  
  bind_rows(main_rows, related_rows) %>% distinct()
}

# Build the relation sets 
tmp_cm_ae <- make_relations(cm_who_drug_1, "CM", "AE", study)
tmp_cm_mh <- make_relations(cm_who_drug_1, "CM", "MH", study)
tmp_cm_pr <- make_relations(cm_who_drug_1, "CM", "PR", study)
tmp_pr_ae <- make_relations(pr_meddra_1 , "PR", "AE", study)
tmp_pr_mh <- make_relations(pr_meddra_1 , "PR", "MH", study)


# ============================================================================
# 2) DS–AE relations (from eos_1 ETAE* columns) 
# ----------------------------------------------------------------------------
tmp_ds_ae <- {
  df <- eos_1 %>% rename_with(tolower)
  
  etae_cols <- names(df)[
    str_detect(names(df), regex("etae", TRUE)) &
      !str_detect(names(df), regex("id", TRUE))
  ]
  
  empty_out <- tibble(
    studyid   = character(),
    usubjid   = character(),
    rdomain   = character(),
    subjid    = character(),
    idvar     = character(),
    idvarval  = character(),
    relid     = character(),
    reltype   = character()
  )
  
  if (length(etae_cols) == 0) {
    empty_out
  } else {
    long <- df %>%
      select(usubjid, subjectid, all_of(etae_cols)) %>%
      pivot_longer(all_of(etae_cols), names_to = "var", values_to = "val") %>%
      filter(!is.na(val) & val != "") %>%
      mutate(
        subjid  = as.character(subjectid),
        reltype = "",
        ae_key  = first_token(val),
        relid   = paste0("DSAE", ae_key)
      )
    
    ds_rows <- long %>%
      transmute(
        studyid  = study,
        usubjid  = as.character(usubjid),
        rdomain  = "DS",
        subjid,
        idvar    = "DSTERM",
        idvarval = "ADVERSE EVENT",
        relid,
        reltype
      )
    
    ae_rows <- long %>%
      transmute(
        studyid  = study,
        usubjid  = as.character(usubjid),
        rdomain  = "AE",
        subjid,
        idvar    = "AESPID",
        idvarval = ae_key,
        relid,
        reltype
      )
    
    bind_rows(ds_rows, ae_rows) %>% distinct()
  }
}


# ============================================================================
# 3) FA–BE relations 
# ----------------------------------------------------------------------------
tmp_fa_be <- tibble(
  studyid  = study,
  usubjid  = "",
  rdomain  = c("BE", "FA"),
  subjid   = "",
  idvar    = c("BELNKID", "FALNKID"),
  idvarval = c("", ""),
  relid    = c("BEFA", "BEFA"),
  reltype  = c("ONE", "ONE")
) %>% distinct()


# ============================================================================
# 4) XS–AE (AENO*) and XS–CM (AESCM*) relations (from sae_1) 
# ----------------------------------------------------------------------------
make_xs_rel <- function(df, prefix, target_domain, rel_prefix, studyid) {
  df <- df %>% rename_with(tolower)
  
  cols <- names(df)[
    str_detect(names(df), paste0("^", tolower(prefix))) &
      !str_detect(names(df), "id")
  ]
  
  empty_out <- tibble(
    studyid   = character(),
    usubjid   = character(),
    rdomain   = character(),
    subjid    = character(),
    idvar     = character(),
    idvarval  = character(),
    relid     = character(),
    reltype   = character()
  )
  
  if (!all(c("usubjid", "subjectid", "saespid") %in% names(df)) || length(cols) == 0) {
    return(empty_out)
  }
  
  long <- df %>%
    select(usubjid, subjectid, saespid, all_of(cols)) %>%
    pivot_longer(all_of(cols), names_to = "var", values_to = "val") %>%
    filter(!is.na(val) & val != "" & !is.na(saespid)) %>%
    mutate(
      subjid  = as.character(subjectid),
      reltype = "",
      key     = first_token(val),
      relid   = paste0(rel_prefix, key),
      xsspid  = as.character(saespid)
    )
  
  xs_rows <- long %>%
    transmute(
      studyid  = studyid,
      usubjid  = as.character(usubjid),
      rdomain  = "XS",
      subjid,
      idvar    = "XSSPID",
      idvarval = xsspid,
      relid,
      reltype
    )
  
  tgt_rows <- long %>%
    transmute(
      studyid  = studyid,
      usubjid  = as.character(usubjid),
      rdomain  = toupper(target_domain),
      subjid,
      idvar    = paste0(toupper(target_domain), "SPID"),
      idvarval = key,
      relid,
      reltype
    )
  
  bind_rows(xs_rows, tgt_rows) %>% distinct()
}

tmp_xs_ae <- make_xs_rel(sae_1, "AENO",  "AE", "XSAE", study)
tmp_xs_cm <- make_xs_rel(sae_1, "AESCM", "CM", "XSCM", study)


# ============================================================================
# 5) EC–EX relations 
# ----------------------------------------------------------------------------
tmp_ec_ex <- tibble(
  studyid  = study,
  usubjid  = "",
  rdomain  = c("EC", "EX"),
  subjid   = "",
  idvar    = c("ECLNKGRP", "EXLNKID"),
  idvarval = c("", ""),
  relid    = c("ECEX", "ECEX"),
  reltype  = c("MANY", "ONE")
) %>% distinct()


# ============================================================================
# 6) FA–ML relations 
# ----------------------------------------------------------------------------
tmp_fa_ml <- tibble(
  studyid  = study,
  usubjid  = "",
  rdomain  = c("FA", "ML"),
  subjid   = "",
  idvar    = c("FALNKGRP", "MLLNKID"),
  idvarval = c("", ""),
  relid    = c("MLFA", "MLFA"),
  reltype  = c("MANY", "ONE")
) %>% distinct()


# ============================================================================
# 7) FA–CE relations 
# ----------------------------------------------------------------------------
tmp_fa_ce <- tibble(
  studyid  = study,
  usubjid  = "",
  rdomain  = c("FA", "CE"),
  subjid   = "",
  idvar    = c("FALNKGR2", "CELNKID"),
  idvarval = c("", ""),
  relid    = c("CEFA", "CEFA"),
  reltype  = c("MANY", "ONE")
) %>% distinct()


# ============================================================================
# 8) Final combined output (stack all temporary relation sets)
# ----------------------------------------------------------------------------
relrec <- bind_rows(
  tmp_cm_ae, tmp_cm_mh, tmp_cm_pr, tmp_pr_ae, tmp_pr_mh,
  tmp_ds_ae,
  tmp_fa_be,
  tmp_xs_ae, tmp_xs_cm,
  tmp_ec_ex,
  tmp_fa_ml,
  tmp_fa_ce
) %>%
  distinct()

relrec <- relrec %>%
  mutate(
    usubjid  = as.character(usubjid),
    subjid   = as.character(subjid),
    idvarval = as.character(idvarval)
  ) %>%
  arrange(studyid, rdomain, usubjid, idvar, idvarval, relid)

# --- Finalize -----------------------------------------------------------------
RELREC <- sdtm_create_domain(
  relrec,
  keys    = "studyid, rdomain, usubjid, idvar, idvarval, relid",
  delperm = FALSE
)