# =============================================================================
# Create Dummy Raw Data: be.csv (Biospecimen Events)
# =============================================================================
# Run from study-pilot/ directory
# Output: raw/be.csv
# Based on example1/BE.sas pattern — three specimen types:
#   - STOOL collection (site/home, with SPI timing)
#   - VENOUS BLOOD collection
#   - CAPILLARY BLOOD (HemoCue) collection
# =============================================================================

set.seed(42)

n_subjects <- 8
sites      <- c("SITEA-01", "SITEB-02", "SITEC-03")
countries  <- c("XX", "XX", "XX")
date_anchor <- as.Date("2025-06-01")

subjectids <- character(n_subjects)
for (i in seq_len(n_subjects)) {
  country_idx <- ((i - 1) %% length(countries)) + 1
  subjectids[i] <- sprintf("%s-%s-%04d", countries[country_idx], sites[country_idx], i)
}

# Visit schedule: SCR (day 1), V1 (day 2), V2 (day 60), V3 (day 90)
visits_with_be <- c("SCR", "V1", "V2", "V3")
visit_offsets   <- c(0, 1, 59, 89)

rows <- list()

for (i in seq_len(n_subjects)) {
  ic_date <- date_anchor + sample(-30:0, 1)

  for (v in seq_along(visits_with_be)) {
    visit_date <- ic_date + visit_offsets[v]
    vid <- visits_with_be[v]

    # ---- STOOL collection ----
    stool_collected <- sample(c("Y", "N"), 1, prob = c(0.85, 0.15))
    stool_geo <- if (stool_collected == "Y") {
      sample(c("INVESTIGATIONAL SITE", "HOUSEHOLD ENVIRONMENT"), 1, prob = c(0.7, 0.3))
    } else NA_character_
    stool_reasnd <- if (stool_collected == "N") {
      sample(c("SUBJECT REFUSED", "UNABLE TO PROVIDE"), 1)
    } else NA_character_
    spi_yn <- sample(c("Y", "N"), 1, prob = c(0.8, 0.2))

    rows[[length(rows) + 1L]] <- data.frame(
      SubjectId   = subjectids[i],
      EventId     = vid,
      SpecType    = "STOOL",
      EventType   = "COLLECTING",
      Performed   = stool_collected,
      GeoLocation = if (is.na(stool_geo)) "" else stool_geo,
      ReasonND    = if (is.na(stool_reasnd)) "" else stool_reasnd,
      SpiYN       = spi_yn,
      CollDate    = format(visit_date, "%Y-%m-%d"),
      ClMethod    = "",
      stringsAsFactors = FALSE
    )

    # ---- VENOUS BLOOD collection (only at V1, V2, V3) ----
    if (vid != "SCR") {
      wb_perf <- sample(c("Y", "N"), 1, prob = c(0.9, 0.1))
      wb_reasnd <- if (wb_perf == "N") "DIFFICULT VENOUS ACCESS" else ""
      rows[[length(rows) + 1L]] <- data.frame(
        SubjectId   = subjectids[i],
        EventId     = vid,
        SpecType    = "VENOUS BLOOD",
        EventType   = "COLLECTING",
        Performed   = wb_perf,
        GeoLocation = "",
        ReasonND    = wb_reasnd,
        SpiYN       = "",
        CollDate    = format(visit_date, "%Y-%m-%d"),
        ClMethod    = "",
        stringsAsFactors = FALSE
      )
    }

    # ---- CAPILLARY BLOOD (HemoCue) collection ----
    hc_perf <- sample(c("Y", "N"), 1, prob = c(0.9, 0.1))
    hc_reasnd <- if (hc_perf == "N") "EQUIPMENT MALFUNCTION" else ""
    rows[[length(rows) + 1L]] <- data.frame(
      SubjectId   = subjectids[i],
      EventId     = vid,
      SpecType    = "CAPILLARY BLOOD",
      EventType   = "COLLECTING",
      Performed   = hc_perf,
      GeoLocation = "",
      ReasonND    = hc_reasnd,
      SpiYN       = "",
      CollDate    = format(visit_date, "%Y-%m-%d"),
      ClMethod    = "FINGERSTICK",
      stringsAsFactors = FALSE
    )
  }
}

be <- do.call(rbind, rows)

dir.create("raw", showWarnings = FALSE, recursive = TRUE)
write.csv(be, "raw/be.csv", row.names = FALSE, na = "")
cat("Created raw/be.csv:", nrow(be), "rows x", ncol(be), "cols\n")
