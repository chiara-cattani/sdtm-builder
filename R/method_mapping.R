# ==============================================================================
# Method-to-Rule-Type Mapping
# ==============================================================================
# Fixed mapping from the METHOD column in Study_Metadata.xlsx Variables sheet
# to internal rule_type values used by compile_rules() and derive_variable().
# ==============================================================================

#' Fixed mapping from METHOD values to internal rule types
#'
#' Named character vector where names = METHOD values (uppercase) and
#' values = internal rule_type strings.
#'
#' @details
#' The METHOD column in the Variables sheet of Study_Metadata.xlsx indicates
#' which derivation algorithm to apply for a variable. This mapping translates
#' those into the rule types supported by the sdtmbuilder derivation engine.
#'
#' | METHOD   | rule_type        | Description |
#' |----------|------------------|-------------|
#' | SEQ      | seq              | Generate --SEQ sequence numbers |
#' | STRESN   | numeric_round    | Standardized numeric result |
#' | USUBJID  | unusbjid         | Derive USUBJID from components |
#' | DY       | dy               | Study day relative to RFSTDTC |
#' | DUR      | duration         | Duration between two dates |
#' | EPOCH    | epoch            | Epoch assignment from epoch_map |
#' | VISITNUM | visitnum         | Visit number from visit_map |
#' | VISIT    | visit            | Visit name from visit_map |
#' | VISITDY  | visitdy          | Visit day derivation |
#' | TPT      | tpt              | Timepoint derivation |
#' | BASELINE | baseline_flag    | Baseline flag (--BLFL) |
#' | LASTOBS  | lastobs_flag     | Last observation flag |
#'
#' @noRd
METHOD_RULE_MAP <- c(
  "SEQ"      = "seq",
  "STRESN"   = "numeric_round",
  "USUBJID"  = "unusbjid",
  "DY"       = "dy",
  "DUR"      = "duration",
  "EPOCH"    = "epoch",
  "VISITNUM" = "visitnum",
  "VISIT"    = "visit",
  "VISITDY"  = "visitdy",
  "TPT"      = "tpt",
  "BASELINE" = "baseline_flag",
  "LASTOBS"  = "lastobs_flag"
)


#' Map METHOD values to internal rule types
#'
#' Translates a vector of METHOD strings (from the Variables sheet) into
#' the corresponding `rule_type` values used by [compile_rules()] and
#' [derive_variable()].
#'
#' Unknown/unmapped METHOD values produce a warning and return `NA`.
#'
#' @param method_vector Character vector. METHOD values from the Variables sheet.
#' @return Character vector of the same length with mapped rule_type values.
#'   `NA` for empty/unknown methods.
#'
#' @examples
#' map_method_to_rule_type(c("SEQ", "STRESN", NA, "CUSTOM"))
#' # Returns: c("seq", "numeric_round", NA, NA) with warning about "CUSTOM"
#'
#' @export
map_method_to_rule_type <- function(method_vector) {
  result <- rep(NA_character_, length(method_vector))

  # Skip NAs and empty strings
  non_empty <- !is.na(method_vector) & nchar(trimws(method_vector)) > 0L
  if (!any(non_empty)) return(result)

  methods_upper <- toupper(trimws(method_vector[non_empty]))
  mapped <- METHOD_RULE_MAP[methods_upper]
  result[non_empty] <- unname(mapped)

  # Warn about unmapped methods
  unmapped <- methods_upper[is.na(mapped)]
  if (length(unmapped) > 0L) {
    unique_unmapped <- unique(unmapped)
    warn(glue::glue(
      "Unknown METHOD value(s) not mapped to rule_type: ",
      "{paste(unique_unmapped, collapse = ', ')}. ",
      "These variables will need manual rule_type assignment."
    ))
  }

  result
}
