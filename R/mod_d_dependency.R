# ==============================================================================
# Module D: MANDATORY SDTM DERIVATION ORDER SYSTEM
# ==============================================================================
# Implements automatic derivation ordering based on the 10-category mandatory
# SDTM sequence. No explicit depends_on column required.
#
# CATEGORY ORDER (1-10):
#   1. Topic Grouping Variables        (TESTCD, TERM, TRT, DECOD)
#   2. Identifier Variables             (STUDYID, DOMAIN, USUBJID, SPID)
#   3. Result Qualifiers                (ORRES, ORRESU, STRESC, STRESN)
#   4. Record-Level Variables           (STAT, REASND, occurrence flags)
#   5. Qualifier Variables              (CAT, SCAT)
#   6. Rule-Derived Variables           (Custom sponsor rules)
#   7. Synonym Qualifiers               (TEST from TESTCD)
#   8. Timing Variables                 (DTC, STDTC, DY)
#   9. Variable Qualifiers              (SEV, SER, REL, LOC, METHOD)
#  10. Sequence Variable                (SEQ - LAST)
# ==============================================================================

#' Assign a derivation category to a variable
#'
#' Maps rule_type and variable name to the 10-category mandatory SDTM order.
#' Category 1 = first derivation, Category 10 = last (SEQ).
#'
#' @param var Character. Variable name.
#' @param rule_type Character or NA. Rule type from compiled rules.
#' @param domain Character or NA. Domain name for context.
#' @return Integer (1-10). Lower = earlier derivation.
#' @keywords internal
#' @noRd
.derive_category <- function(var, rule_type = NA_character_, domain = NA_character_) {
  var_upper <- toupper(var)
  rule_type_lc <- tolower(rule_type %||% "")

  # ========== CATEGORY 10: SEQUENCE (ALWAYS LAST) ==========
  if (rule_type_lc == "seq" ||
      grepl("SEQ$", var_upper, perl = TRUE)) {
    return(10L)
  }

  # ========== CATEGORY 2: IDENTIFIERS ==========
  # Core structural keys (needed early for joins/filtering)
  if (var_upper %in% c("STUDYID", "DOMAIN", "USUBJID", "SUBJID") ||
      grepl("^(SPID|GRPID|REFID|SOURCEID)$", var_upper, perl = TRUE) ||
      rule_type_lc %in% c("usubjid", "sourceid")) {
    return(2L)
  }

  # ========== CATEGORY 1: TOPIC GROUPING VARIABLES ==========
  # What the record is ABOUT (must precede all dependent qualifiers)
  if (grepl("TESTCD|TESTCDIN|TERM|TERMVER|TERMININV|" ..
           "TRT|TRTCD|TRTVER|TRTININ|" ..
           "DECOD|DECVERSI|DICTVER|" ..
           "MEDDRA|WHODRUG", var_upper, perl = TRUE, ignore.case = FALSE)) {
    return(1L)
  }

  # ========== CATEGORY 3: RESULT QUALIFIERS ==========
  # ORRES must come before STRES* (standardized results)
  if (grepl("ORRES|ORRESU|STRESC|STRESU|STRESN", var_upper, perl = TRUE)) {
    return(3L)
  }

  # ========== CATEGORY 4: RECORD-LEVEL VARIABLES ==========
  # Status/existence variables (STAT, REASND, etc.)
  if (grepl("REASND|REASDC|STAT$", var_upper, perl = TRUE)) {
    return(4L)
  }

  # ========== CATEGORY 5: QUALIFIER VARIABLES ==========
  # Categories/subcategories (CAT, SCAT)
  if (grepl("CAT$|SCAT$|CATEGORY|SUBCATEGORY", var_upper, perl = TRUE)) {
    return(5L)
  }

  # ========== CATEGORY 8: TIMING VARIABLES ==========
  # Dates, study days, visits, epochs
  if (rule_type_lc %in% c("iso_dtc", "dy", "duration") ||
      grepl("DTC$|DTMC$|STDTC|ENDTC|DY$|ENDDT|STARTDT|" ..
           "EPOCH|VISIT|VISITNUM|VISITDY|TPT|TPTREF|ELTM",
           var_upper, perl = TRUE)) {
    return(8L)
  }
  if (rule_type_lc %in% c("epoch", "visitnum", "visit", "visitdy", "tpt", "ref_time_point")) {
    return(8L)
  }

  # ========== CATEGORY 7: SYNONYM QUALIFIERS ==========
  # Dictionary-decoded synonyms (from coded variables)
  # TEST, LLT, PT, HLT, HLGT, SOC, BODSYS, etc.
  if (grepl("^TEST$|^LLT$|^PT$|^HLT$|^HLGT$|^SOC$|^BODSYS$", var_upper, perl = TRUE) ||
      (grepl("DICT|DICT.", var_upper, perl = TRUE) && !grepl("CD$|VER$", var_upper, perl = TRUE))) {
    return(7L)
  }

  # ========== CATEGORY 9: VARIABLE QUALIFIERS ==========
  # Event properties: SEV, SER, REL, LOC, METHOD, LEAD, LATERAL, etc.
  if (grepl("SEV$|SEVER|SERIOUS|SER$|" ..
           "RELATION|RELAT|LOC$|LOCAT|" ..
           "METHOD|PERIOD|UNIT|FORM|" ..
           "INSTOFF|INSTVAL|LEAD|METNUMFL|" ..
           "ASSAY|ANATOM|LATERAL|SIDE",
           var_upper, perl = TRUE)) {
    return(9L)
  }

  # ========== CATEGORY 6: RULE-DERIVED VARIABLES (DEFAULT) ==========
  # Custom derivations: case_when, if_else, flags, coalesce, concat, etc.
  if (rule_type_lc %in% c("case_when", "if_else", "coalesce", "concat",
                          "regex_extract", "regex_replace", "trim_pad",
                          "baseline_flag", "lastobs_flag", "occurrence", "status")) {
    return(6L)
  }

  # ========== DEFAULT: CATEGORY 6 ==========
  6L
}

#' Compute mandatory SDTM derivation order
#'
#' Automatically orders variables 1-10 based on rule_type and variable name.
#' Does NOT require explicit depends_on metadata.
#'
#' @param rule_set `rule_set` object.
#' @param domain Character. Domain name.
#' @return Character vector of variable names in derivation order.
#' @keywords internal
#' @noRd
.compute_sdtm_derivation_order <- function(rule_set, domain) {
  dom_rules <- rule_set$rules[[domain]]
  if (is.null(dom_rules) || length(dom_rules) == 0L) {
    return(character())
  }

  all_vars <- names(dom_rules)

  # Assign categories (1-10) to each variable
  categories <- vapply(all_vars, function(var) {
    rule <- dom_rules[[var]]
    .derive_category(
      var = var,
      rule_type = rule$type %||% NA_character_,
      domain = domain
    )
  }, integer(1))

  # Sort: primary by category, secondary by variable name (stable)
  all_vars[order(categories, all_vars)]
}

#' Build a variable dependency graph for a domain
#'
#' Uses MANDATORY SDTM DERIVATION ORDER instead of explicit depends_on.
#' Orders variables 1-10 based on rule_type and variable names only.
#'
#' @param rule_set `rule_set` object.
#' @param domain Character.
#' @param allow_cycles Logical. Default `FALSE`.
#' @param late_bind_vars Character vector. Default `character()`.
#' @return Named list with `order`, `cycles`, `phases`.
#' @export
build_dependency_graph <- function(rule_set, domain,
                                   allow_cycles = FALSE,
                                   late_bind_vars = character()) {
  
  dom_rules <- rule_set$rules[[domain]]
  if (is.null(dom_rules) || length(dom_rules) == 0L) {
    return(list(
      graph = NULL,
      order = character(),
      cycles = list(),
      late_bind_plan = tibble::tibble(),
      phases = list(phase1 = character())
    ))
  }

  # Compute mandatory SDTM derivation order
  var_order <- .compute_sdtm_derivation_order(rule_set, domain)

  list(
    graph = NULL,  # No igraph needed with mandatory order
    order = var_order,
    cycles = list(),
    late_bind_plan = tibble::tibble(),
    phases = list(phase1 = var_order)
  )
}

#' Topologically sort rules within a domain
#' @param graph An `igraph` directed graph or output of `build_dependency_graph`.
#' @return Character vector of variable names in derivation order.
#' @export
topo_sort_rules <- function(graph) {
  if (is.list(graph) && !igraph::is_igraph(graph)) {
    if (!is.null(graph$order)) return(graph$order)
    graph <- graph$graph
  }
  if (is.null(graph)) return(character())
  sorted <- igraph::topo_sort(graph, mode = "out")
  igraph::V(graph)$name[sorted]
}

#' Detect cycles in a dependency graph
#' @param graph `igraph` directed graph.
#' @return List of cycle paths.
#' @export
detect_cycles <- function(graph) {
  if (is.null(graph) || !igraph::is_igraph(graph)) return(list())
  comps <- igraph::components(graph, mode = "strong")
  cycles <- list()
  for (i in seq_len(comps$no)) {
    members <- which(comps$membership == i)
    if (length(members) > 1L) {
      cycles <- c(cycles, list(igraph::V(graph)$name[members]))
    }
  }
  cycles
}

#' Resolve late-binding variables
#' @param graph `igraph` graph.
#' @param cycles List of cycle paths.
#' @param late_bind_vars Character vector.
#' @return Named list.
#' @export
resolve_late_binding <- function(graph, cycles, late_bind_vars) {
  if (length(cycles) == 0L) {
    return(list(resolved_graph = graph,
                phases = list(phase1 = if (is.list(graph) && "order" %in% names(graph)) graph$order else igraph::V(graph)$name),
                deferred_edges = tibble::tibble()))
  }
  # Remove edges to late_bind_vars to break cycles
  edges_to_remove <- c()
  el <- igraph::as_edgelist(graph)
  for (i in seq_len(nrow(el))) {
    if (el[i, 2] %in% late_bind_vars) {
      edges_to_remove <- c(edges_to_remove, i)
    }
  }
  if (length(edges_to_remove) > 0L) {
    g2 <- igraph::delete_edges(graph, edges_to_remove)
  } else {
    g2 <- graph
  }
  phase1 <- topo_sort_rules(g2)
  phase2 <- late_bind_vars
  list(resolved_graph = g2,
       phases = list(phase1 = phase1, phase2 = phase2),
       deferred_edges = tibble::tibble())
}

#' Generate a domain-level build plan
#' @param domain Character.
#' @param dep_graph Output of `build_dependency_graph`.
#' @param rule_set `rule_set`.
#' @return Tibble with build steps.
#' @export
plan_build_steps <- function(domain, dep_graph, rule_set) {
  var_order <- dep_graph$order %||% character()
  if (length(var_order) == 0L) {
    return(tibble::tibble(
      step = integer(),
      variable = character(),
      rule_type = character()
    ))
  }

  dom_rules <- rule_set$rules[[domain]]

  tibble::tibble(
    step = seq_along(var_order),
    variable = var_order,
    rule_type = vapply(var_order, function(v) {
      dom_rules[[v]]$type %||% "unknown"
    }, character(1))
  )
}
