#' @keywords internal
"_PACKAGE"

#' sdtmbuilder: Metadata-Driven SDTM Domain Builder and Code Generator
#'
#' Auto-generates SDTM domain programs (R scripts / Quarto documents) from
#' structured metadata. Given SDTM target metadata (Study_Metadata.xlsx),
#' controlled terminology (Study_CT.xlsx), and study-level configuration,
#' produces domain-building code for all SDTM domains with realistic
#' derivations.
#'
#' @section Package modules:
#' \describe{
#'   \item{A. Primitives}{S3 classes: config, meta bundle, rule set, build context, validation report}
#'   \item{B. Metadata Ingestion}{Read, validate, normalize target metadata and CT}
#'   \item{C. Rule Compilation}{Convert derivation descriptions to machine-readable rules}
#'   \item{D. Dependency System}{DAG construction, topological sort, cycle detection}
#'   \item{E. Data Access}{Load raw data, standardize names/types, derive core keys}
#'   \item{F. Join & Assembly}{Safe joins, record expansion, checkbox handling}
#'   \item{G. Derivation Library}{Atomic derivation functions for all SDTM variable types}
#'   \item{H. Domain Builders}{Orchestrators and domain-family plugins}
#'   \item{I. Validation}{P21-style conformance checks}
#'   \item{J. Code Generation}{R script and Quarto document generators}
#'   \item{K. Export}{XPT output, define.xml support artifacts}
#'   \item{L. Utilities}{Logging, assertions, error handling}
#' }
#'
#' @name sdtmbuilder-package
#' @aliases sdtmbuilder
#' @importFrom rlang .data .env %||% abort warn inform :=
#' @importFrom dplyr mutate filter select arrange group_by ungroup summarise
#'   left_join inner_join full_join anti_join semi_join bind_rows rename
#'   across everything n row_number case_when if_else coalesce pull distinct
#'   slice_head slice_tail transmute relocate
#' @importFrom tidyr pivot_longer pivot_wider separate unite unnest
#' @importFrom purrr map map_chr map_lgl map_dfr walk imap reduce keep discard
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_extract str_replace str_replace_all
#'   str_to_upper str_to_lower str_trim str_pad str_sub str_c str_split
#' @importFrom tibble tibble as_tibble tribble
#' @importFrom glue glue
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_danger
#'   cli_alert_success cli_progress_bar cli_progress_update
#'   cli_progress_done
NULL
