# ==============================================================================
# Dummy Study Data Generator
# ==============================================================================
# Produces deterministic, SDTM-realistic raw datasets for testing.
# All randomness is seed-controlled.  Generates: dm_raw, ae_raw, cm_raw,
# mh_raw, pr_raw as tibbles, plus loads metadata and config from starter kit.
# ==============================================================================

#' Generate a complete dummy study for testing
#'
#' @description
#' Creates a named list containing:
#' - `raw_data`: named list of tibbles (`dm_raw`, `ae_raw`, `cm_raw`, `mh_raw`,
#'   `pr_raw`)
#' - `target_meta`: tibble loaded from starter kit
#' - `source_meta`: tibble loaded from starter kit
#' - `ct_lib`: tibble loaded from starter kit
#' - `config`: `sdtm_config` object
#'
#' All randomness is controlled by `seed`.  Calling `make_dummy_study(seed=123)`
#' always produces identical output.
#'
#' @param seed Integer. Random seed. Default `123`.
#' @param n_subjects Integer. Number of subjects. Default `30`.
#' @param bad_case Character or `NULL`. Inject a specific data quality issue.
#'   Supported values:
#'   - `"dup_dm_key"`: duplicate one USUBJID in DM
#'   - `"bad_ct_value"`: insert unmapped severity "TERRIBLE" in AE
#'   - `"invalid_date"`: insert "2025-13-01" in AE start date
#'   - `"missing_req_var"`: drop `aeterm` column from ae_raw
#'   - `NULL`: clean data (default)
#' @param starter_kit_dir Character. Path to starter kit files. Default uses
#'   the installed package path.
#'
#' @return Named list (see Description).
#'
#' @section Design choices:
#' - **30 subjects** by default (SUBJ-001 to SUBJ-030)
#' - **Date anchor**: 2025-01-01; each subject offset by 0–60 days
#' - **AE**: 0–5 events per subject (Poisson λ = 2.5); ~75 total
#' - **CM**: 0–3 meds per subject (Poisson λ = 1.5); ~45 total
#' - **MH**: 0–4 history items per subject (Poisson λ = 2.0); ~60 total
#' - **PR**: 0–3 procedures per subject (Poisson λ = 1.5); ~45 total
#'
#' @export
make_dummy_study <- function(seed = 123,
                             n_subjects = 30,
                             bad_case = NULL,
                             starter_kit_dir = NULL) {
  set.seed(seed)

  # ---------------------------------------------------------------------------
  # Resolve starter kit path
  # ---------------------------------------------------------------------------
  if (is.null(starter_kit_dir)) {
    starter_kit_dir <- system.file("extdata", "starter_kit",
                                   package = "sdtmbuilder")
    if (starter_kit_dir == "") {
      # Fallback for development: relative to package root
      starter_kit_dir <- file.path("inst", "extdata", "starter_kit")
    }
  }

  # ---------------------------------------------------------------------------
  # Load metadata
  # ---------------------------------------------------------------------------
  target_meta <- utils::read.csv(
    file.path(starter_kit_dir, "target_meta.csv"),
    stringsAsFactors = FALSE, na.strings = ""
  )
  source_meta <- utils::read.csv(
    file.path(starter_kit_dir, "source_meta.csv"),
    stringsAsFactors = FALSE, na.strings = ""
  )

  ct_lib <- utils::read.csv(
    file.path(starter_kit_dir, "ct_codelist.csv"),
    stringsAsFactors = FALSE, na.strings = ""
  )
  cfg_yaml <- yaml::read_yaml(file.path(starter_kit_dir, "config.yaml"))

  # Convert to tibbles
  target_meta <- tibble::as_tibble(target_meta)
  source_meta <- tibble::as_tibble(source_meta)
  ct_lib      <- tibble::as_tibble(ct_lib)

  # ---------------------------------------------------------------------------
  # Build config object
  # ---------------------------------------------------------------------------
  visit_map_df <- NULL
  if (!is.null(cfg_yaml$visit_map)) {
    visit_map_df <- tibble::tibble(
      VISITNUM  = vapply(cfg_yaml$visit_map, `[[`, numeric(1), "visitnum"),
      VISIT     = vapply(cfg_yaml$visit_map, `[[`, character(1), "visit"),
      START_DAY = vapply(cfg_yaml$visit_map, `[[`, numeric(1), "start_day"),
      END_DAY   = vapply(cfg_yaml$visit_map, `[[`, numeric(1), "end_day")
    )
  }

  config <- new_sdtm_config(
    studyid        = cfg_yaml$studyid,
    timezone       = cfg_yaml$timezone %||% "UTC",
    ref_start_rule = list(
      var    = cfg_yaml$ref_start_rule$column %||% "rfstdtc",
      source = cfg_yaml$ref_start_rule$dataset %||% "dm_raw"
    ),
    visit_map      = visit_map_df,
    ct_paths       = character(),
    log_level      = cfg_yaml$log_level %||% "INFO"
  )

  # Attach epoch_map (list-of-lists from config.yaml) -------------------------
  config$epoch_map <- cfg_yaml$epoch_map

  # ---------------------------------------------------------------------------
  # Helper vectors
  # ---------------------------------------------------------------------------
  subject_ids <- sprintf("SUBJ-%03d", seq_len(n_subjects))
  site_ids    <- sprintf("SITE-%02d", rep(1:5, length.out = n_subjects))
  date_anchor <- as.Date("2025-01-01")

  # Per-subject offsets (days from anchor to RFSTDTC)
  subj_offsets <- sample(0:60, n_subjects, replace = TRUE)
  rfstdtc_dates <- date_anchor + subj_offsets

  # ---------------------------------------------------------------------------
  # DM raw
  # ---------------------------------------------------------------------------
  arms <- c("TREATMENT", "PLACEBO")
  sexes <- c("M", "F")
  races <- c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "OTHER")

  dm_raw <- tibble::tibble(
    usubjid = subject_ids,
    subjid  = sprintf("%03d", seq_len(n_subjects)),
    siteid  = site_ids,
    rfstdtc = format(rfstdtc_dates, "%Y-%m-%d"),
    rfendtc = format(rfstdtc_dates + sample(60:120, n_subjects, replace = TRUE),
                     "%Y-%m-%d"),
    brthdtc = format(date_anchor - (365.25 * sample(25:75, n_subjects,
                                                     replace = TRUE)),
                     "%Y-%m-%d"),
    age     = sample(25:75, n_subjects, replace = TRUE),
    sex     = sample(sexes, n_subjects, replace = TRUE, prob = c(0.5, 0.5)),
    race    = sample(races, n_subjects, replace = TRUE,
                     prob = c(0.6, 0.2, 0.15, 0.05)),
    armcd   = sample(arms, n_subjects, replace = TRUE, prob = c(0.5, 0.5)),
    arm     = NA_character_
  )
  dm_raw$arm <- ifelse(dm_raw$armcd == "TREATMENT",
                       "Active Treatment 10mg",
                       "Placebo")
  dm_raw$ageu_raw <- rep("YEARS", n_subjects)

  # ---------------------------------------------------------------------------
  # AE raw
  # ---------------------------------------------------------------------------
  ae_terms <- c("HEADACHE", "NAUSEA", "DIZZINESS", "FATIGUE", "INSOMNIA",
                "RASH", "DIARRHEA", "VOMITING", "COUGH", "BACK PAIN",
                "ARTHRALGIA", "NASOPHARYNGITIS")
  ae_decods <- ae_terms  # simplified: same as term (no real MedDRA)
  ae_bodsys <- c("NERVOUS SYSTEM DISORDERS", "GASTROINTESTINAL DISORDERS",
                 "NERVOUS SYSTEM DISORDERS", "GENERAL DISORDERS",
                 "PSYCHIATRIC DISORDERS", "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                 "GASTROINTESTINAL DISORDERS", "GASTROINTESTINAL DISORDERS",
                 "RESPIRATORY DISORDERS", "MUSCULOSKELETAL DISORDERS",
                 "MUSCULOSKELETAL DISORDERS", "INFECTIONS AND INFESTATIONS")
  sev_values <- c("mild", "Mild", "MILD", "moderate", "Moderate", "severe")
  rel_values <- c("related", "not related", "possibly related",
                   "NOT RELATED", "RELATED")
  ser_values <- c("y", "n", "N", "Y")

  ae_rows <- list()
  ae_counter <- 0
  for (i in seq_len(n_subjects)) {
    n_ae <- stats::rpois(1, lambda = 2.5)
    if (n_ae == 0) next
    rfst <- rfstdtc_dates[i]
    for (j in seq_len(n_ae)) {
      ae_counter <- ae_counter + 1
      ae_start_offset <- sample(-5:80, 1)
      ae_start <- rfst + ae_start_offset
      ae_dur <- sample(1:30, 1)
      ae_end <- ae_start + ae_dur

      # ~40% missing start time
      has_time <- stats::runif(1) > 0.4
      stim <- if (has_time) sprintf("%02d:%02d", sample(6:22, 1),
                                    sample(0:59, 1)) else NA_character_

      # ~70% missing end time
      has_end_time <- stats::runif(1) > 0.7
      etim <- if (has_end_time) sprintf("%02d:%02d", sample(6:22, 1),
                                        sample(0:59, 1)) else NA_character_

      # ~15% ongoing (no end date)
      ongoing <- stats::runif(1) < 0.15
      end_dat <- if (ongoing) NA_character_ else format(ae_end, "%Y-%m-%d")

      # ~10% partial start date (year-month only)
      partial <- stats::runif(1) < 0.10
      start_dat <- if (partial) format(ae_start, "%Y-%m") else
        format(ae_start, "%Y-%m-%d")

      term_idx <- sample(length(ae_terms), 1)
      ae_sess <- sample(c("VISIT 1", "VISIT 2", "VISIT 3", "UNSCHEDULED"), 1)
      ae_rows[[ae_counter]] <- tibble::tibble(
        usubjid   = subject_ids[i],
        aeid      = sprintf("AE-%04d", ae_counter),
        aeterm    = ae_terms[term_idx],
        aedecod   = ae_decods[term_idx],
        aesev_raw = sample(sev_values, 1),
        aeser_raw = sample(ser_values, 1),
        aerel_raw = sample(rel_values, 1),
        aestdat   = start_dat,
        aestim    = stim,
        aeendat   = end_dat,
        aeentim   = if (!ongoing) etim else NA_character_,
        aebodsys  = ae_bodsys[term_idx],
        aession   = ae_sess,
        aetrtem   = paste0(ae_terms[term_idx], " (local)")
      )
    }
  }
  ae_raw <- dplyr::bind_rows(ae_rows)

  # ---------------------------------------------------------------------------
  # CM raw
  # ---------------------------------------------------------------------------
  cm_names <- c("ASPIRIN", "METFORMIN", "ATORVASTATIN", "LISINOPRIL",
                "OMEPRAZOLE", "AMLODIPINE", "IBUPROFEN", "PARACETAMOL")
  cm_decods <- cm_names
  route_vals <- c("oral", "iv", "topical")
  dosu_vals  <- c("mg", "ml", "g")
  freq_vals  <- c("qd", "bid", "tid", "prn")

  cm_rows <- list()
  cm_counter <- 0
  for (i in seq_len(n_subjects)) {
    n_cm <- stats::rpois(1, lambda = 1.5)
    if (n_cm == 0) next
    rfst <- rfstdtc_dates[i]
    for (j in seq_len(n_cm)) {
      cm_counter <- cm_counter + 1
      cm_start_offset <- sample(-30:60, 1)
      cm_start <- rfst + cm_start_offset
      # ~30% ongoing (no end)
      ongoing <- stats::runif(1) < 0.30
      cm_dur <- sample(7:90, 1)
      cm_end <- cm_start + cm_dur

      med_idx <- sample(length(cm_names), 1)
      cm_rows[[cm_counter]] <- tibble::tibble(
        usubjid      = subject_ids[i],
        cmid         = sprintf("CM-%04d", cm_counter),
        cmtrt        = cm_names[med_idx],
        cmdecod      = cm_decods[med_idx],
        cmroute_raw  = sample(route_vals, 1),
        cmdose       = sample(c(5, 10, 20, 50, 100, 250, 500), 1),
        cmdosu_raw   = sample(dosu_vals, 1),
        cmdosfrq_raw = sample(freq_vals, 1),
        cmstdat      = format(cm_start, "%Y-%m-%d"),
        cmendat      = if (ongoing) NA_character_ else
          format(cm_end, "%Y-%m-%d")
      )
    }
  }
  cm_raw <- dplyr::bind_rows(cm_rows)

  # ---------------------------------------------------------------------------
  # MH raw
  # ---------------------------------------------------------------------------
  mh_terms <- c("HYPERTENSION", "DIABETES MELLITUS", "ASTHMA",
                "SEASONAL ALLERGY", "MIGRAINE", "DEPRESSION",
                "HYPERLIPIDEMIA", "GASTROESOPHAGEAL REFLUX",
                "OSTEOARTHRITIS", "HYPOTHYROIDISM")
  mh_decods <- mh_terms
  mh_cats <- c("GENERAL", "SURGICAL", "FAMILY HISTORY")

  mh_rows <- list()
  mh_counter <- 0
  for (i in seq_len(n_subjects)) {
    n_mh <- stats::rpois(1, lambda = 2.0)
    if (n_mh == 0) next
    for (j in seq_len(n_mh)) {
      mh_counter <- mh_counter + 1
      # MH dates are historical: 1–20 years before anchor
      years_ago <- sample(1:20, 1)
      mh_start <- date_anchor - (years_ago * 365)

      # Partial date probability: ~20% year-only, ~30% year-month, ~50% full
      partial_r <- stats::runif(1)
      mh_stdat <- if (partial_r < 0.20) {
        format(mh_start, "%Y")
      } else if (partial_r < 0.50) {
        format(mh_start, "%Y-%m")
      } else {
        format(mh_start, "%Y-%m-%d")
      }

      # ~50% ongoing
      ongoing <- stats::runif(1) < 0.50
      mh_endat <- if (ongoing) NA_character_ else
        format(mh_start + sample(30:3650, 1), "%Y-%m-%d")

      term_idx <- sample(length(mh_terms), 1)
      mh_rows[[mh_counter]] <- tibble::tibble(
        usubjid  = subject_ids[i],
        mhid     = sprintf("MH-%04d", mh_counter),
        mhterm   = mh_terms[term_idx],
        mhdecod  = mh_decods[term_idx],
        mhcat    = sample(mh_cats, 1, prob = c(0.7, 0.2, 0.1)),
        mhstdat  = mh_stdat,
        mhendat  = mh_endat
      )
    }
  }
  mh_raw <- dplyr::bind_rows(mh_rows)

  # ---------------------------------------------------------------------------
  # PR raw
  # ---------------------------------------------------------------------------
  pr_names <- c("APPENDECTOMY", "COLONOSCOPY", "ECG", "X-RAY",
                "BLOOD DRAW", "MRI", "CT SCAN", "BIOPSY",
                "PHYSICAL THERAPY", "DENTAL CLEANING")
  pr_cats <- c("DIAGNOSTIC", "SURGICAL", "MONITORING", "THERAPEUTIC")

  pr_rows <- list()
  pr_counter <- 0
  for (i in seq_len(n_subjects)) {
    n_pr <- stats::rpois(1, lambda = 1.5)
    if (n_pr == 0) next
    rfst <- rfstdtc_dates[i]
    for (j in seq_len(n_pr)) {
      pr_counter <- pr_counter + 1
      pr_start_offset <- sample(-10:80, 1)
      pr_start <- rfst + pr_start_offset

      # ~60% have start time
      has_time <- stats::runif(1) < 0.60
      prstim <- if (has_time) sprintf("%02d:%02d", sample(7:17, 1),
                                      sample(0:59, 1)) else NA_character_

      # ~70% have end date
      has_end <- stats::runif(1) < 0.70
      prendat <- if (has_end) format(pr_start + sample(0:3, 1),
                                     "%Y-%m-%d") else NA_character_

      pr_idx <- sample(length(pr_names), 1)
      pr_rows[[pr_counter]] <- tibble::tibble(
        usubjid     = subject_ids[i],
        prid        = sprintf("PR-%04d", pr_counter),
        prtrt       = pr_names[pr_idx],
        prcat       = sample(pr_cats, 1),
        prpresp_raw = sample(c("y", "n", "Y", "N"), 1),
        prstdat     = format(pr_start, "%Y-%m-%d"),
        prstim      = prstim,
        prendat     = prendat
      )
    }
  }
  pr_raw <- dplyr::bind_rows(pr_rows)

  # ---------------------------------------------------------------------------
  # EX raw (Exposure)
  # ---------------------------------------------------------------------------
  ex_rows <- list()
  ex_counter <- 0
  for (i in seq_len(n_subjects)) {
    rfst <- rfstdtc_dates[i]
    trt_name <- if (dm_raw$armcd[i] == "TREATMENT") "ACTIVE TREATMENT 10MG"
                else "PLACEBO"
    # Each subject gets ~4-8 dosing records (visits)
    n_ex <- sample(4:8, 1)
    for (j in seq_len(n_ex)) {
      ex_counter <- ex_counter + 1
      ex_start <- rfst + (j - 1) * 7
      ex_end   <- ex_start + 6
      dose_val <- if (dm_raw$armcd[i] == "TREATMENT") 10 else 0
      ex_rows[[ex_counter]] <- tibble::tibble(
        usubjid      = subject_ids[i],
        exid         = sprintf("EX-%04d", ex_counter),
        extrt        = trt_name,
        exdose       = dose_val,
        exdosu_raw   = "mg",
        exdosfrq_raw = "qd",
        exroute_raw  = "oral",
        exstdat      = format(ex_start, "%Y-%m-%d"),
        exendat      = format(ex_end, "%Y-%m-%d")
      )
    }
  }
  ex_raw <- dplyr::bind_rows(ex_rows)

  # ---------------------------------------------------------------------------
  # VS raw (Vital Signs)
  # ---------------------------------------------------------------------------
  vs_tests <- list(
    list(cd = "SYSBP",  nm = "Systolic Blood Pressure",  u = "mmHg",    lo = 90,  hi = 180),
    list(cd = "DIABP",  nm = "Diastolic Blood Pressure", u = "mmHg",    lo = 50,  hi = 110),
    list(cd = "HR",     nm = "Heart Rate",               u = "beats/min", lo = 50,  hi = 120),
    list(cd = "TEMP",   nm = "Temperature",              u = "C",        lo = 360, hi = 390),
    list(cd = "WEIGHT", nm = "Weight",                   u = "kg",       lo = 50,  hi = 120)
  )
  # Visits for VS: use first 5 visits from visit_map
  vs_visit_info <- list(
    list(vnum = 1, vname = "SCREENING"),
    list(vnum = 2, vname = "BASELINE"),
    list(vnum = 3, vname = "WEEK 1"),
    list(vnum = 4, vname = "WEEK 2"),
    list(vnum = 5, vname = "WEEK 4")
  )

  vs_rows <- list()
  vs_counter <- 0
  for (i in seq_len(n_subjects)) {
    rfst <- rfstdtc_dates[i]
    for (vi in seq_along(vs_visit_info)) {
      vinfo <- vs_visit_info[[vi]]
      # Visit day offset relative to rfstdtc
      day_offsets <- c(-7, 0, 5, 12, 29)
      vdate <- rfst + day_offsets[vi]
      is_baseline <- (vinfo$vnum == 2)

      for (ti in seq_along(vs_tests)) {
        tst <- vs_tests[[ti]]
        vs_counter <- vs_counter + 1
        # Generate value with some variation
        val <- if (tst$cd == "TEMP") {
          round(stats::runif(1, tst$lo, tst$hi) / 10, 1)
        } else {
          round(stats::runif(1, tst$lo, tst$hi))
        }

        has_time <- stats::runif(1) < 0.80
        vstim <- if (has_time) sprintf("%02d:%02d", sample(7:17, 1),
                                       sample(0:59, 1)) else NA_character_

        vs_rows[[vs_counter]] <- tibble::tibble(
          usubjid  = subject_ids[i],
          vsid     = sprintf("VS-%05d", vs_counter),
          vstestcd = tst$cd,
          vstest   = tst$nm,
          vsorres  = as.character(val),
          vsorresu = tst$u,
          vsdat    = format(vdate, "%Y-%m-%d"),
          vstim    = vstim,
          visit    = vinfo$vname,
          visitnum = vinfo$vnum,
          vsblfl   = if (is_baseline) "Y" else NA_character_
        )
      }
    }
  }
  vs_raw <- dplyr::bind_rows(vs_rows)

  # ---------------------------------------------------------------------------
  # LB raw (Laboratory)
  # ---------------------------------------------------------------------------
  lb_tests <- list(
    list(cd = "ALT",  nm = "Alanine Aminotransferase", u = "U/L",    lo = 5,   hi = 80),
    list(cd = "AST",  nm = "Aspartate Aminotransferase", u = "U/L",  lo = 5,   hi = 60),
    list(cd = "GLUC", nm = "Glucose",                  u = "mg/dL",  lo = 60,  hi = 200),
    list(cd = "WBC",  nm = "Leukocytes",               u = "10^9/L", lo = 30,  hi = 120),
    list(cd = "HGB",  nm = "Hemoglobin",               u = "g/dL",   lo = 100, hi = 180)
  )
  lb_visit_info <- list(
    list(vnum = 1, vname = "SCREENING"),
    list(vnum = 2, vname = "BASELINE"),
    list(vnum = 3, vname = "WEEK 1"),
    list(vnum = 5, vname = "WEEK 4")
  )

  lb_rows <- list()
  lb_counter <- 0
  for (i in seq_len(n_subjects)) {
    rfst <- rfstdtc_dates[i]
    for (vi in seq_along(lb_visit_info)) {
      vinfo <- lb_visit_info[[vi]]
      day_offsets <- c(-7, 0, 5, 29)
      vdate <- rfst + day_offsets[vi]

      for (ti in seq_along(lb_tests)) {
        tst <- lb_tests[[ti]]
        lb_counter <- lb_counter + 1
        val <- round(stats::runif(1, tst$lo, tst$hi) / 10, 1)

        has_time <- stats::runif(1) < 0.70
        lbtim <- if (has_time) sprintf("%02d:%02d", sample(6:12, 1),
                                       sample(0:59, 1)) else NA_character_

        lb_rows[[lb_counter]] <- tibble::tibble(
          usubjid  = subject_ids[i],
          lbid     = sprintf("LB-%05d", lb_counter),
          lbtestcd = tst$cd,
          lbtest   = tst$nm,
          lborres  = as.character(val),
          lborresu = tst$u,
          lbdat    = format(vdate, "%Y-%m-%d"),
          lbtim    = lbtim,
          visit    = vinfo$vname,
          visitnum = vinfo$vnum
        )
      }
    }
  }
  lb_raw <- dplyr::bind_rows(lb_rows)

  # ---------------------------------------------------------------------------
  # Bad-case injection
  # ---------------------------------------------------------------------------
  if (!is.null(bad_case)) {
    if (bad_case == "dup_dm_key") {
      # Duplicate the first subject's DM row
      dm_raw <- dplyr::bind_rows(dm_raw, dm_raw[1, ])
    } else if (bad_case == "bad_ct_value") {
      # Insert unmapped severity
      if (nrow(ae_raw) > 0) ae_raw$aesev_raw[1] <- "TERRIBLE"
    } else if (bad_case == "invalid_date") {
      # Invalid month
      if (nrow(ae_raw) > 0) ae_raw$aestdat[1] <- "2025-13-01"
    } else if (bad_case == "missing_req_var") {
      # Drop aeterm column
      ae_raw$aeterm <- NULL
    } else {
      rlang::warn(glue::glue("Unknown bad_case: {bad_case}; ignoring."))
    }
  }

  # ---------------------------------------------------------------------------
  # Return
  # ---------------------------------------------------------------------------
  list(
    raw_data = list(
      dm_raw = dm_raw,
      ae_raw = ae_raw,
      cm_raw = cm_raw,
      mh_raw = mh_raw,
      pr_raw = pr_raw,
      ex_raw = ex_raw,
      vs_raw = vs_raw,
      lb_raw = lb_raw
    ),
    target_meta = target_meta,
    source_meta = source_meta,
    ct_lib      = ct_lib,
    config      = config
  )
}
