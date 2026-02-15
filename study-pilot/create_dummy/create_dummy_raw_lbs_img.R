# =============================================================================
# Create Dummy Raw Data: lbs_img.csv (Images and Videos CRF)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/lbs_img.csv
# =============================================================================

set.seed(199)

library(tibble)

n_subjects <- 8
sites      <- c("SITEA-01", "SITEB-02", "SITEC-03")
countries  <- c("XX", "XX", "XX")
date_anchor <- as.Date("2025-06-01")

subjectids <- character(n_subjects)
for (i in seq_len(n_subjects)) {
  country_idx <- ((i - 1) %% length(countries)) + 1
  subjectids[i] <- sprintf("%s-%s-%04d", countries[country_idx], sites[country_idx], i)
}

ic_dates   <- date_anchor + sample(-30:0, n_subjects, replace = TRUE)
rfst_dates <- ic_dates

img_eventids <- c("V1", "V1_IMG", "V3", "V3_IMG")

lbs_img_rows <- list()
img_counter <- 0
for (i in seq_len(n_subjects)) {
  n_visits <- sample(1:2, 1)
  selected_visits <- sample(img_eventids, n_visits)

  for (v in selected_visits) {
    img_counter <- img_counter + 1
    visit_date <- rfst_dates[i] + switch(v,
      "V1" = , "V1_IMG" = 0,
      "V3" = , "V3_IMG" = 141,
      0
    )

    occur_vals <- sample(c("Y", NA_character_), 5, replace = TRUE, prob = c(0.7, 0.3))
    reason_vals <- ifelse(is.na(occur_vals),
                          sample(c("Equipment failure", "Subject refused"), 1),
                          NA_character_)
    date_vals <- ifelse(!is.na(occur_vals), format(visit_date, "%Y-%m-%d"), NA_character_)

    lbs_img_rows[[img_counter]] <- tibble(
      SubjectId      = subjectids[i],
      EventId        = v,
      lbs_imdhndnd   = occur_vals[1],
      lbs_imphndnd   = occur_vals[2],
      lbs_vllndnd    = occur_vals[3],
      lbs_vlelndnd   = occur_vals[4],
      lbs_vtndnd     = occur_vals[5],
      lbs_imdhndr    = reason_vals[1],
      lbs_imphndr    = reason_vals[2],
      lbs_vllndr     = reason_vals[3],
      lbs_vllelndr   = reason_vals[4],
      lbs_vtndr      = reason_vals[5],
      lbs_dhdate     = date_vals[1],
      lbs_phdate     = date_vals[2],
      lbs_vlldate    = date_vals[3],
      lbs_vlyldate   = date_vals[4],
      lbs_vtdate     = date_vals[5],
      InitiatedDate  = format(visit_date, "%Y-%m-%d"),
      LastEditedDate = format(visit_date + sample(1:5, 1), "%Y-%m-%d")
    )
  }
}
lbs_img <- dplyr::bind_rows(lbs_img_rows)

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(lbs_img, "raw/lbs_img.csv", row.names = FALSE, na = "")
cat("Created raw/lbs_img.csv:", nrow(lbs_img), "rows x", ncol(lbs_img), "cols\n")
