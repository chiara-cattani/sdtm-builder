# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA / 23REX0061265
# PROGRAM PATH  : SAS/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : QC_QS.R 
# PURPOSE       : QC SDTM data for Questionnaires
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-08 - cattanch - Initial program
# 2025-12-08 - cattanch - Use CT for QSORRES and QSSTRESC.
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
sdtm_domain <- "QS"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
qs1 <- copy_import(inset = "qs")
dm1 <- read_sas("../41 SDTM Data/dm.sas7bdat")
rs1 <- copy_import(inset = "review_status")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# Well-being ----
qs_wb1 <-
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs01",
    tgt_var = "QSTESTCD",
    tgt_val = "WBQ0101",
    ct_spec = sdtm_ct,
    ct_clst = "D0028"
  ) %>%
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs01",
    tgt_var = "QSTEST",
    tgt_val = "WBQ01-Have you experienced a change in your energy levels after taking the study product for 4 weeks?",
    ct_spec = sdtm_ct,
    ct_clst = "D0029",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = qs1,
    raw_var = "qs01",
    tgt_var = "QSORRES",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = qs1,
    raw_var = "qs01",
    tgt_var = "QSSTRESC",
    id_vars = oak_id_vars()
  )

qs_wb2 <-
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs02",
    tgt_var = "QSTESTCD",
    tgt_val = "WBQ0102",
    ct_spec = sdtm_ct,
    ct_clst = "D0028"
  ) %>%
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs02",
    tgt_var = "QSTEST",
    tgt_val = "WBQ01-How would you describe your energy levels after taking the study product for 4 weeks?",
    ct_spec = sdtm_ct,
    ct_clst = "D0029",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = qs1,
    raw_var = "qs02",
    tgt_var = "QSORRES",
    ct_spec = sdtm_ct,
    ct_clst = "D0027",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = qs1,
    raw_var = "qs02",
    tgt_var = "QSSTRESC",
    ct_spec = sdtm_ct,
    ct_clst = "D0027",
    id_vars = oak_id_vars()
  )

qs_wb3 <-
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs03",
    tgt_var = "QSTESTCD",
    tgt_val = "WBQ0103",
    ct_spec = sdtm_ct,
    ct_clst = "D0028"
  ) %>%
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs03",
    tgt_var = "QSTEST",
    tgt_val = "WBQ01-What other changes in your day-to-day life have you experienced after taking the study product for 4 weeks?",
    ct_spec = sdtm_ct,
    ct_clst = "D0029",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = qs1,
    raw_var = "qs03",
    tgt_var = "QSORRES",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = qs1,
    raw_var = "qs03",
    tgt_var = "QSSTRESC",
    id_vars = oak_id_vars()
  )

qs_wb <- 
  bind_rows(
    qs_wb1,
    qs_wb2,
    qs_wb3
  ) %>%
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "usubjid",
    tgt_var = "QSCAT",
    tgt_val = "WELL-BEING QUESTIONNAIRE",
    ct_spec = sdtm_ct,
    ct_clst = "C100129",
    id_vars = oak_id_vars()
  )

# Product evaluation ----
qs_pe1 <-
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs04",
    tgt_var = "QSTESTCD",
    tgt_val = "PEQ0101",
    ct_spec = sdtm_ct,
    ct_clst = "D0014"
  ) %>%
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs04",
    tgt_var = "QSTEST",
    tgt_val = "PEQ01-The product is easy to consume/drink",
    ct_spec = sdtm_ct,
    ct_clst = "D0015",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = qs1,
    raw_var = "qs04",
    tgt_var = "QSORRES",
    ct_spec = sdtm_ct,
    ct_clst = "D0013",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = qs1,
    raw_var = "qs04",
    tgt_var = "QSSTRESC",
    ct_spec = sdtm_ct,
    ct_clst = "D0013",
    id_vars = oak_id_vars()
  )

qs_pe2 <-
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs05",
    tgt_var = "QSTESTCD",
    tgt_val = "PEQ0102",
    ct_spec = sdtm_ct,
    ct_clst = "D0014"
  ) %>%
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs05",
    tgt_var = "QSTEST",
    tgt_val = "PEQ01-It is easy to consume/drink the full serving",
    ct_spec = sdtm_ct,
    ct_clst = "D0015",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = qs1,
    raw_var = "qs05",
    tgt_var = "QSORRES",
    ct_spec = sdtm_ct,
    ct_clst = "D0013",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = qs1,
    raw_var = "qs05",
    tgt_var = "QSSTRESC",
    ct_spec = sdtm_ct,
    ct_clst = "D0013",
    id_vars = oak_id_vars()
  )


qs_pe3 <-
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs06",
    tgt_var = "QSTESTCD",
    tgt_val = "PEQ0103",
    ct_spec = sdtm_ct,
    ct_clst = "D0014"
  ) %>%
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs06",
    tgt_var = "QSTEST",
    tgt_val = "PEQ01-It will be easy to continue consuming 2 servings per day",
    ct_spec = sdtm_ct,
    ct_clst = "D0015",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = qs1,
    raw_var = "qs06",
    tgt_var = "QSORRES",
    ct_spec = sdtm_ct,
    ct_clst = "D0013",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = qs1,
    raw_var = "qs06",
    tgt_var = "QSSTRESC",
    ct_spec = sdtm_ct,
    ct_clst = "D0013",
    id_vars = oak_id_vars()
  )

qs_pe4 <-
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs07",
    tgt_var = "QSTESTCD",
    tgt_val = "PEQ0104",
    ct_spec = sdtm_ct,
    ct_clst = "D0014"
  ) %>%
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "qs07",
    tgt_var = "QSTEST",
    tgt_val = "PEQ01-The product is easy to prepare",
    ct_spec = sdtm_ct,
    ct_clst = "D0015",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = qs1,
    raw_var = "qs07",
    tgt_var = "QSORRES",
    ct_spec = sdtm_ct,
    ct_clst = "D0013",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = qs1,
    raw_var = "qs07",
    tgt_var = "QSSTRESC",
    ct_spec = sdtm_ct,
    ct_clst = "D0013",
    id_vars = oak_id_vars()
  )

qs_pe <- 
  bind_rows(
    qs_pe1,
    qs_pe2,
    qs_pe3,
    qs_pe4
  ) %>%
  hardcode_ct(
    raw_dat = qs1,
    raw_var = "usubjid",
    tgt_var = "QSCAT",
    tgt_val = "PRODUCT EVALUATION QUESTIONNAIRE",
    ct_spec = sdtm_ct,
    ct_clst = "C100129",
    id_vars = oak_id_vars()
  )

# QS ----
qs2 <- 
  bind_rows(
    qs_wb,
    qs_pe
  ) %>%
  assign_no_ct(
    raw_dat = qs1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = qs1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = qs1,   
    raw_var = "usubjid",
    tgt_var = "QSEVAL",
    ct_spec = sdtm_ct,
    ct_clst = "C78735",
    tgt_val = "STUDY SUBJECT",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = qs1,   
    raw_var = "usubjid",
    tgt_var = "QSORIG",
    ct_spec = sdtm_ct,
    ct_clst = "D0023",
    tgt_val = "pPRO",
    id_vars = oak_id_vars()
  ) %>%
  mutate(
    STUDYID  = study,
    DOMAIN   = sdtm_domain,
    SOURCEID = paste("CRF:", sources$formname[match(sdtm_domain, sources$formid)])
  ) %>%
  left_join(
    qs1 %>% select(all_of(c(oak_id_vars(), "eventid"))),
    by = oak_id_vars()
  ) %>%
  {
    # Always run assign_datetime, but protect against empty input or missing column
    if (nrow(qs1) == 0L || !"qsdat" %in% names(qs1)) {
      mutate(., QSDTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = qs1 %>% mutate(qsdat = as.character(qsdat)),
        raw_var = "qsdat",
        tgt_var = "QSDTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  {
    if (nrow(.) == 0L || !"QSDTC" %in% names(.) || !"USUBJID" %in% names(.) || all(is.na(.$QSDTC))) {
      mutate(., QSDY = NA_integer_)
    } else {
      mutate(., USUBJID = as.character(USUBJID)) %>%
        mutate(QSDTC_TMP = QSDTC) %>%
        derive_study_day(
          sdtm_in       = .,
          dm_domain     = dm1 %>% mutate(USUBJID = as.character(USUBJID)),
          tgdt          = "QSDTC",
          refdt         = "RFSTDTC",
          study_day_var = "QSDY",
          merge_key     = "USUBJID"
        ) %>%
        mutate(QSDTC = QSDTC_TMP)
    }
  } %>%
  filter(!is.na(.data$QSTESTCD)) %>%
  derive_lobxfl(., dm1, sdtm_domain) %>%
  derive_visit_vars(., eventid_var = "eventid") %>%
  derive_seq(
    tgt_var = "QSSEQ", 
    rec_vars = c("STUDYID", "USUBJID", "QSCAT", "QSTESTCD", "VISITNUM")
  )

# Finalize ----
qs <- sdtm_create_domain(qs2, keys = "STUDYID, USUBJID, QSCAT, QSTESTCD, VISITNUM", delperm = FALSE)
