# ******************************************************************************
# Danone Nutricia Research
# ******************************************************************************
# STUDY/PROJECT : SONA
# PROGRAM PATH  : R/SONA/Files/SDTM Conversion/40 SDTM Programs
# PROGRAM NAME  : XS.R 
# PURPOSE       : Create SDTM data for Serious Adverse Events
# ------------------------------------------------------------------------------
# NOTES : 
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2025-10-13 - cattanch - Initial program
# 2025-10-29 - Vikas Shokeen - Extended Checks
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
sdtm_domain <- "XS"

# Import metadata
sdtm_ct  <- read_excel("../31 Metadata Data/SONA_CT.xlsx", sheet = "CT")
sdtm_var <- read_sas("../31 Metadata Data/sdtm_3_4_variables.sas7bdat")
sdtm_dom <- read_sas("../31 Metadata Data/sdtm_3_4_domains.sas7bdat")

# Import data ----
dm1  <- read_sas("../41 SDTM Data/dm.sas7bdat")
sae1 <- copy_import(inset = "sae")
rs1 <- copy_import(inset = "review_status")

# Sources ----
sources <- rs1 %>%
  filter(trimws(formname) != "") %>%
  distinct(formid, formname)

# XS ----
xs1 <- 
  assign_no_ct(
    raw_dat = sae1,
    raw_var = "saespid",
    tgt_var = "XSSPID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = sae1,
    raw_var = "saeterm",
    tgt_var = "XSTERM",
    id_vars = oak_id_vars()
  ) %>%
  {
    if (nrow(sae1) == 0L || !"aesstdat" %in% names(sae1)) {
      mutate(., XSSTDTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesstdat = as.character(aesstdat)),
        raw_var = "aesstdat",
        tgt_var = "XSSTDTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  {
    if (nrow(sae1) == 0L || !"aesendat" %in% names(sae1)) {
      mutate(., XSENDTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesendat = as.character(aesendat)),
        raw_var = "aesendat",
        tgt_var = "XSENDTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  hardcode_ct(
    raw_dat = condition_add(sae1, aesongo == "Y"),
    raw_var = "aesongo",
    tgt_var = "XSENRTPT",
    ct_spec = sdtm_ct,
    ct_clst = "C66728",
    tgt_val = "ONGOING",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_ct(
    raw_dat = condition_add(sae1, aesongo == "N"),
    raw_var = "aesongo",
    tgt_var = "XSENRTPT",
    ct_spec = sdtm_ct,
    ct_clst = "C66728",
    tgt_val = "BEFORE",
    id_vars = oak_id_vars()
  ) %>%
  hardcode_no_ct(
    raw_dat = condition_add(sae1, aesongo != ""),
    raw_var = "aesongo",
    tgt_var = "XSENTPT",
    tgt_val = "END OF STUDY",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = sae1,
    raw_var = "aesage",
    tgt_var = "XSAGE",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = condition_add(sae1, !is.na(aesage)),
    raw_var = "aesageu",
    tgt_var = "XSAGEU",
    ct_spec = sdtm_ct,
    ct_clst = "C66781",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = sae1,
    raw_var = "usubjid",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_no_ct(
    raw_dat = sae1,
    raw_var = "subjectid",
    tgt_var = "SUBJID",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = sae1,
    raw_var = "aesdth",
    tgt_var = "XSSDTH",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = sae1,
    raw_var = "aeslife",
    tgt_var = "XSSLIFE",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = sae1,
    raw_var = "aeshosp",
    tgt_var = "XSSHOSP",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = sae1,
    raw_var = "aesdisab",
    tgt_var = "XSSDISAB",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = sae1,
    raw_var = "aescong",
    tgt_var = "XSSCONG",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = sae1,
    raw_var = "aesmie",
    tgt_var = "XSSMIE",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  {
    if (nrow(sae1) == 0L || !"aesaddat" %in% names(sae1)) {
      mutate(., XSADMDTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesaddat = as.character(aesaddat)),
        raw_var = "aesaddat",
        tgt_var = "XSADMDTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  {
    if (nrow(sae1) == 0L || !"aesdidat" %in% names(sae1)) {
      mutate(., XSDISDTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesdidat = as.character(aesdidat)),
        raw_var = "aesdidat",
        tgt_var = "XSDISDTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  {
    if (nrow(sae1) == 0L || !"aesdtdat" %in% names(sae1)) {
      mutate(., XSDTHDTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesdtdat = as.character(aesdtdat)),
        raw_var = "aesdtdat",
        tgt_var = "XSDTHDTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  assign_no_ct(
    raw_dat = sae1,
    raw_var = "aesdtres",
    tgt_var = "XSDTHRSN",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = sae1,
    raw_var = "aesautop",
    tgt_var = "XSAUTOP",
    ct_spec = sdtm_ct,
    ct_clst = "D0012",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = sae1,
    raw_var = "aesdthct",
    tgt_var = "XSDTHCT",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct_with_na(
    raw_dat = sae1,
    raw_var = "saeacnp",
    tgt_var = "XSACN",
    ct_spec = sdtm_ct,
    ct_clst = "D0001",
    id_vars = oak_id_vars()
  ) %>%
  {
    if (nrow(sae1) == 0L || !"aesfpi" %in% names(sae1)) {
      mutate(., XSFPIDTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesfpi = as.character(aesfpi)),
        raw_var = "aesfpi",
        tgt_var = "XSFPIDTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  assign_no_ct(
    raw_dat = sae1,
    raw_var = "aesdose",
    tgt_var = "XSTRTDOS",
    id_vars = oak_id_vars()
  ) %>%
  {
    if (nrow(sae1) == 0L || !"aesrddat" %in% names(sae1)) {
      mutate(., XSRDDTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesrddat = as.character(aesrddat)),
        raw_var = "aesrddat",
        tgt_var = "XSRDDTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  assign_no_ct(
    raw_dat = sae1,
    raw_var = "aesrdosf",
    tgt_var = "XSRDDOSF",
    id_vars = oak_id_vars()
  ) %>% 
  assign_no_ct(
    raw_dat = sae1,
    raw_var = "aesrddot",
    tgt_var = "XSRDDOST",
    id_vars = oak_id_vars()
  ) %>%
  {
    if (nrow(sae1) == 0L || !"aesindat" %in% names(sae1)) {
      mutate(., XSINDTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesindat = as.character(aesindat)),
        raw_var = "aesindat",
        tgt_var = "XSINDTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  assign_no_ct(
    raw_dat = sae1,
    raw_var = "aesindof",
    tgt_var = "XSINDOSF",
    id_vars = oak_id_vars()
  ) %>% 
  assign_no_ct(
    raw_dat = sae1,
    raw_var = "aesindot",
    tgt_var = "XSINDOST",
    id_vars = oak_id_vars()
  ) %>%
  {
    if (nrow(sae1) == 0L || !"aesitdat" %in% names(sae1)) {
      mutate(., XSIT1DTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesitdat = as.character(aesitdat)),
        raw_var = "aesitdat",
        tgt_var = "XSIT1DTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  {
    if (nrow(sae1) == 0L || !"aesrsdat" %in% names(sae1)) {
      mutate(., XSRS1DTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesrsdat = as.character(aesrsdat)),
        raw_var = "aesrsdat",
        tgt_var = "XSRS1DTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  assign_ct(
    raw_dat = sae1,
    raw_var = "aerein",
    tgt_var = "XSRIT1",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  {
    if (nrow(sae1) == 0L || !"aesitdat2" %in% names(sae1)) {
      mutate(., XSIT2DTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesitdat2 = as.character(aesitdat2)),
        raw_var = "aesitdat2",
        tgt_var = "XSIT2DTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  {
    if (nrow(sae1) == 0L || !"aesrsdat2" %in% names(sae1)) {
      mutate(., XSRS2DTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesrsdat2 = as.character(aesrsdat2)),
        raw_var = "aesrsdat2",
        tgt_var = "XSRS2DTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  assign_ct(
    raw_dat = sae1,
    raw_var = "aerein2",
    tgt_var = "XSRIT2",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  {
    if (nrow(sae1) == 0L || !"aesitdat3" %in% names(sae1)) {
      mutate(., XSIT3DTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesitdat3 = as.character(aesitdat3)),
        raw_var = "aesitdat3",
        tgt_var = "XSIT3DTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  {
    if (nrow(sae1) == 0L || !"aesrsdat3" %in% names(sae1)) {
      mutate(., XSRS3DTC = NA_character_)
    } else {
      assign_datetime(
        .,
        raw_dat = sae1 %>% mutate(aesrsdat3 = as.character(aesrsdat3)),
        raw_var = "aesrsdat3",
        tgt_var = "XSRS3DTC",
        raw_fmt = c("y-m-d"),
        raw_unk = c("NK", "UNK", "UN"),
        id_vars = oak_id_vars()
      )
    }
  } %>%
  assign_ct(
    raw_dat = sae1,
    raw_var = "aesabate",
    tgt_var = "XSEABATE",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  assign_ct(
    raw_dat = sae1,
    raw_var = "aesrecur",
    tgt_var = "XSEREAPP",
    ct_spec = sdtm_ct,
    ct_clst = "C66742",
    id_vars = oak_id_vars()
  ) %>%
  # XSSTDTC study day ----
{
  if (nrow(.) == 0L || !"XSSTDTC" %in% names(.) || !"USUBJID" %in% names(.) || all(is.na(.$XSSTDTC))) {
    mutate(., XSSTDY = NA_integer_)
  } else {
    mutate(., USUBJID = as.character(USUBJID)) %>%
      mutate(XSSTDTC_TMP = XSSTDTC) %>%
      derive_study_day(
        sdtm_in       = .,
        dm_domain     = dm1 %>% mutate(USUBJID = as.character(USUBJID)),
        tgdt          = "XSSTDTC",
        refdt         = "RFSTDTC",
        study_day_var = "XSSTDY",
        merge_key     = "USUBJID"
      ) %>%
      mutate(XSSTDTC = XSSTDTC_TMP)
  }
} %>%
  
  # XSENDTC study day ----
{
  if (nrow(.) == 0L || !"XSENDTC" %in% names(.) || !"USUBJID" %in% names(.) || all(is.na(.$XSENDTC))) {
    mutate(., XSENDY = NA_integer_)
  } else {
    mutate(., USUBJID = as.character(USUBJID)) %>%
      mutate(XSENDTC_TMP = XSENDTC) %>%
      derive_study_day(
        sdtm_in       = .,
        dm_domain     = dm1 %>% mutate(USUBJID = as.character(USUBJID)),
        tgdt          = "XSENDTC",
        refdt         = "RFSTDTC",
        study_day_var = "XSENDY",
        merge_key     = "USUBJID"
      ) %>%
      mutate(XSENDTC = XSENDTC_TMP)
  }
} %>%
  filter(!is.na(XSTERM)) %>%
  mutate(
    STUDYID = study,
    DOMAIN  = sdtm_domain,
    SOURCEID = paste("CRF:", sources$formname[match("SAE", sources$formid)])
  ) %>%
  derive_seq(
    tgt_var  = "XSSEQ",
    rec_vars = c("STUDYID", "USUBJID", "XSTERM", "XSSTDTC")
  )

attr(xs1$XSTRTDOS, "label") <- "Product dose"

# Finalize ----
xs <- sdtm_create_domain(xs1, delperm = FALSE, addsuppvars = c("XSTRTDOS"))
