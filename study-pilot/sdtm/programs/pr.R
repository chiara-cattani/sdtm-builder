# ******************************************************************************
# STUDY-PILOT
# ******************************************************************************
# PROGRAM NAME  : PR.R
# PURPOSE       : SDTM PR Domain - Procedures
# ------------------------------------------------------------------------------
# NOTES :
#   Raw datasets : pr, pr_meddra
#   Dependencies : sdtmbuilder package
# ------------------------------------------------------------------------------
# PROGRAM HISTORY :
# 2026-02-15 - sdtmbuilder - Auto-generated program
# ******************************************************************************

# Configuration ----
library(sdtmbuilder)
library(dplyr)

study       <- "STUDY-PILOT"
sdtm_domain <- "PR"

# Set working directory to study root ----
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  prog_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(file.path(prog_dir, "..", ".."))
} else if (exists("progdir")) {
  setwd(file.path(progdir, "..", ".."))
}

# Import metadata ----
study_meta <- read_study_metadata_excel("metadata/Study_Metadata.xlsx")
ct_spec    <- read_study_ct_excel("metadata/Study_CT.xlsx")

target_meta      <- study_meta$target_meta
domain_meta      <- study_meta$domain_meta
value_level_meta <- study_meta$value_level_meta

# Import data ----
all_raw <- load_raw_datasets("raw")
for (nm in names(all_raw)) {
  all_raw[[nm]] <- all_raw[[nm]] %>% standardize_names() %>% convert_blanks_to_na()
}

pr <- all_raw[["pr"]]
pr_meddra <- all_raw[["pr_meddra"]]
dm1 <- all_raw[["dm"]]

# Sources lookup (for SOURCEID) ----
review_status <- all_raw[["review_status"]]
sources <- review_status %>%
  dplyr::filter(trimws(formname) != "") %>%
  dplyr::distinct(formid, formname)

# Merge secondary source datasets ----
pr_meddra <- dplyr::rename(pr_meddra, dplyr::any_of(c(prspid = "pr_meddraspid")))
pr_meddra_new <- setdiff(names(pr_meddra), names(pr))
pr_meddra_slim <- pr_meddra[, c("subjectid", "prspid", pr_meddra_new), drop = FALSE]
pr <- pr %>%
  dplyr::left_join(pr_meddra_slim, by = c("subjectid", "prspid"))

# Prepare DM for RFSTDTC ----
# Load DM SDTM dataset (has RFSTDTC); fall back to raw DM
dm_rda <- "sdtm/datasets/RDA/dm.rda"
dm_xpt <- "sdtm/datasets/XPT/dm.xpt"
if (file.exists(dm_rda)) {
  dm_env <- new.env(parent = emptyenv())
  load(dm_rda, envir = dm_env)
  dm_sdtm <- dm_env[[ls(dm_env)[1]]]
  names(dm_sdtm) <- tolower(names(dm_sdtm))
} else if (file.exists(dm_xpt)) {
  dm_sdtm <- haven::read_xpt(dm_xpt)
  names(dm_sdtm) <- tolower(names(dm_sdtm))
} else {
  dm_sdtm <- dm1
}

dm_slim <- dm_sdtm %>%
  dplyr::select(dplyr::any_of(c("subjectid", "subjid", "usubjid", "rfstdtc"))) %>%
  dplyr::distinct()

# Join RFSTDTC: try subjectid=subjid, fall back to usubjid
if ("subjectid" %in% names(pr) && "subjid" %in% names(dm_slim)) {
  pr <- pr %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("subjid", "rfstdtc"))) %>% dplyr::rename(subjectid = subjid), by = "subjectid")
} else if ("usubjid" %in% names(pr) && "usubjid" %in% names(dm_slim)) {
  pr <- pr %>%
    dplyr::left_join(dm_slim %>% dplyr::select(dplyr::any_of(c("usubjid", "rfstdtc"))), by = "usubjid")
}
if ("rfstdtc" %in% names(pr) && !"RFSTDTC" %in% names(pr)) pr$RFSTDTC <- pr$rfstdtc

# PR derivations ----
pr2 <- pr %>%
  # --- Identifiers ---
  mutate(
    STUDYID = study,
    DOMAIN = "PR",
    PRSPID = as.character(prspid)
  ) %>%
  # --- Derived Variables ---
  mutate(PRLNKID = as.character(prlnkid)) %>%
  # --- Topic / Term ---
  mutate(PRTRT = as.character(prtrt)) %>%
  # --- Coded Fields ---
  mutate(PRDECOD = as.character(pt_name)) %>%
  # --- Categories ---
  mutate(PRCAT = as.character(prcat)) %>%
  # --- Derived Variables ---
  mutate(
    PRPRESP = as.character(prpresp),
    PROCCUR = as.character(proccur)
  ) %>%
  # --- Coded Fields ---
  mutate(PRREASOC = as.character(prreasoc)) %>%
  # --- Derived Variables ---
  mutate(PRINDC = as.character(prindc)) %>%
  # --- Visit Variables ---
  mutate(
    VISITNUM = as.numeric(visitnum),
    VISIT = as.character(visit),
    VISITDY = as.numeric(visitdy)
  ) %>%
  # --- Dates ---
  mutate(
    PRSTDTC = format_iso_dtc(prstdat),
    PRENDTC = format_iso_dtc(prendat)
  ) %>%
  # --- Coded Fields ---
  mutate(
    PRENRTPT = NA_character_,  # TODO: case_when derivation
    PRENTPT = dplyr::if_else(!is.na(prongo), "END OF STUDY", NA_character_),
    PRHLGTCD = as.numeric(hlgt_code),
    PRBDSYCD = as.numeric(soc_code),
    PRBODSYS = as.character(soc_name)
  ) %>%
  # --- Dates ---
  mutate(PRCODED = format_iso_dtc(codedondate)) %>%
  # --- Coded Fields ---
  mutate(
    PRHLGT = as.character(hlgt_name),
    PRHLT = as.character(hlt_name),
    PRHLTCD = as.numeric(hlt_code)
  ) %>%
  # --- Derived Variables ---
  mutate(PRINDCX = as.character(prindcx)) %>%
  # --- Dates ---
  mutate(PRINITD = format_iso_dtc(initiateddate)) %>%
  # --- Coded Fields ---
  mutate(
    PRLLT = as.character(llt_name),
    PRLLTCD = as.numeric(llt_code)
  ) %>%
  # --- Dates ---
  mutate(PRLTEDID = format_iso_dtc(lastediteddate)) %>%
  # --- Coded Fields ---
  mutate(
    PRPTCD = as.numeric(pt_code),
    PRSOC = as.character(pt_soc_name),
    PRSOCCD = as.numeric(pt_soc_code),
    PRSOCLST = as.character(soc_list)
  ) %>%
  # --- Derived Variables ---
  mutate(
    PREVAL = as.character(preval),
    PRORIG = as.character(prorig)
  ) %>%
  # --- Identifiers ---
  mutate(
    SOURCEID = paste("CRF:", sources$formname[match("PR", toupper(sources$formid))]),
    SUBJID = as.character(subjectid)
  ) %>%
  derive_usubjid(study, subjid_col = "subjectid") %>%
  # --- Dictionary Version ---
  mutate(PRDICT = dplyr::if_else(!is.na(PRDECOD), paste("MedDRA", get_meddra_version(pr_meddra, dictvar = "DictInstance")), "")) %>%
  # --- Study Day ---
  derive_dy("PRSTDY", "PRSTDTC", "RFSTDTC") %>%
  derive_dy("PRENDY", "PRENDTC", "RFSTDTC") %>%
  # --- Sequence ---
  derive_seq("PRSEQ", by = c("USUBJID"))

# Finalize ----
pr_final <- export_domain(
  data        = pr2,
  domain      = sdtm_domain,
  output_dir  = "sdtm/datasets",
  formats     = c("xpt", "rda"),
  xpt_version = 8L,
  target_meta = target_meta,
  domain_meta = domain_meta,
  keys        = c("STUDYID", "USUBJID", "PRTRT", "PRSTDTC"),
  drop_empty_perm = TRUE
)

cli::cli_alert_success("PR domain created: {nrow(pr2)} rows")
