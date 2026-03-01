# ==============================================================================
# Module D: MANDATORY SDTM DERIVATION ORDER SYSTEM
# ==============================================================================
# Implements automatic derivation ordering based on the 10-category mandatory
# SDTM sequence.  Dependencies are inferred directly from the DERIVATION column
# in metadata (via the compiled rule_set) — no explicit depends_on column.
#
# CATEGORY ORDER (1-10):
#   1. Topic Grouping Variables        (TESTCD, TERM, TRT, DECOD)
#   2. Identifier Variables             (STUDYID, DOMAIN, USUBJID, SPID)
#   3. Result Qualifiers                (ORRES, ORRESU, STRESC, STRESN)
#   4. Record-Level Variables           (STAT, REASND, occurrence flags)
#   5. Qualifier Variables              (CAT, SCAT)
#   6. Rule-Derived Variables           (Custom sponsor rules)
#   7. Synonym Qualifiers               (TEST from TESTCD, coded fields)
#   8. Timing Variables                 (DTC, STDTC, DY)
#   9. Variable Qualifiers              (SEV, SER, REL, LOC, METHOD)
#  10. Sequence Variable                (SEQ - LAST)
# ==============================================================================


# ---- Internal: category assignment ------------------------------------------

#' Assign a derivation category (1-10) to a variable
#'
#' Maps rule_type and variable name to the 10-category mandatory SDTM order.
#' Category 1 = first in derivation, Category 10 = last (SEQ).
#'
#' @param var Character. Variable name (e.g. `"AETERM"`, `"AESTDTC"`).
#' @param rule_type Character or `NA`. Rule type from compiled rules.
#' @param domain Character or `NA`. Domain code for context (e.g. `"AE"`).
#' @return Integer (1-10).
#' @keywords internal
#' @noRd
.derive_category <- function(var, rule_type = NA_character_,
                             domain = NA_character_) {
  vu <- toupper(var)
  rt <- tolower(rule_type %||% "")


  # === CAT 10 — Sequence (always last) ===
  if (rt == "seq" || grepl("SEQ$", vu, perl = TRUE)) return(10L)

  # === CAT 2 — Identifiers ===
  id_names <- c("STUDYID", "DOMAIN", "USUBJID", "SUBJID")
  if (vu %in% id_names ||
      grepl("(SPID|GRPID|REFID|SOURCEID)$", vu, perl = TRUE) ||
      rt %in% c("usubjid", "sourceid", "constant")) {
    return(2L)
  }

  # === CAT 1 — Topic Grouping ===
  topic_pat <- paste0(
    "TESTCD|TESTCDIN|TERM$|TERMVER|TERMININV|TRT$|TRTCD|TRTVER",
    "|DECOD|DECVERSI|DICTVER|MEDDRA|WHODRUG|MODIFY$"
  )
  if (grepl(topic_pat, vu, perl = TRUE)) return(1L)

  # === CAT 3 — Result Qualifiers ===
  if (grepl("ORRES$|ORRESU$|STRESC$|STRESU$|STRESN$", vu, perl = TRUE)) {
    return(3L)
  }

  # === CAT 4 — Record-Level ===
  if (grepl("REASND$|REASDC$|STAT$|OCCUR$|PRESP$", vu, perl = TRUE)) {
    return(4L)
  }

  # === CAT 5 — Qualifier (CAT / SCAT) ===
  if (grepl("CAT$|SCAT$", vu, perl = TRUE)) return(5L)

  # === CAT 7 — Synonym / coded fields ===
  syn_pat <- paste0(
    "^TEST$|LLT$|LLTCD$|PT$|PTCD$|HLT$|HLTCD$|HLGT$|HLGTCD$",
    "|BODSYS$|BDSYCD$|SOC$|SOCCD$|SOCLST$"
  )
  if (grepl(syn_pat, vu, perl = TRUE)) return(7L)
  if (rt %in% c("ct_decode", "dict_version")) return(7L)

  # === CAT 8 — Timing ===
  timing_pat <- paste0(
    "DTC$|DTMC$|STDTC|ENDTC|DY$|ENDDT|STARTDT",
    "|EPOCH$|VISIT$|VISITNUM$|VISITDY$",
    "|TPT$|TPTNUM$|TPTREF$|ELTM$"
  )
  if (grepl(timing_pat, vu, perl = TRUE)) return(8L)
  timing_rt <- c("iso_dtc", "dy", "duration", "epoch",
                 "visitnum", "visit", "visitdy", "tpt", "ref_time_point")
  if (rt %in% timing_rt) return(8L)

  # === CAT 9 — Variable Qualifiers ===
  vq_pat <- paste0(
    "SEV$|SEVER|SERIOUS|SER$|RELAT|REL$|LOC$|LOCAT",
    "|METHOD$|PERIOD$|UNIT$|FORM$|LEAD$|LATERAL$|SIDE$",
    "|DOSE$|DOSU$|DOSFRM$|DOSFRQ$|ROUTE$"
  )
  if (grepl(vq_pat, vu, perl = TRUE)) return(9L)

  # === CAT 6 — Rule-Derived (default for custom logic) ===
  custom_rt <- c("case_when", "if_else", "coalesce", "concat",
                 "regex_extract", "regex_replace", "trim_pad",
                 "baseline_flag", "lastobs_flag", "occurrence", "status")
  if (rt %in% custom_rt) return(6L)

  # === CAT 6 — CT-assignment (depends on topic) ===
  if (rt == "ct_assign") return(6L)

  # === Default: 6 ===
  6L
}


# ---- Internal: sub-ordering within categories --------------------------------

#' Compute a sub-order key for fine-grained sorting within a category
#'
#' For **Category 3** (results): ORRES before STRES variants.
#' For **Category 8** (timing): DTC before DY, VISIT before VISITDY, etc.
#' For **Category 7** (synonyms): TEST after TESTCD (already in cat 1).
#' Otherwise returns 0 so that alphabetical order applies.
#'
#' @param var Character. Variable name.
#' @param rule_type Character or `NA`.
#' @return Integer sub-key (lower = earlier within the category).
#' @keywords internal
#' @noRd
.derive_subcategory <- function(var, rule_type = NA_character_) {
  vu <- toupper(var)
  rt <- tolower(rule_type %||% "")

  # --- Category 3 sub-order: ORRES(1) → ORRESU(2) → STRESC(3) → STRESN(4) → STRESU(5) ---
  if (grepl("ORRES$", vu))  return(1L)
  if (grepl("ORRESU$", vu)) return(2L)
  if (grepl("STRESC$", vu)) return(3L)
  if (grepl("STRESN$", vu)) return(4L)
  if (grepl("STRESU$", vu)) return(5L)

  # --- Category 8 sub-order: DTC(1) → DY(5), VISIT(1) → VISITNUM(2) → VISITDY(3) ---
  if (grepl("STDTC$", vu))    return(1L)
  if (grepl("ENDTC$", vu))    return(2L)
  if (grepl("DTC$", vu))      return(3L)
  if (grepl("EPOCH$", vu))    return(4L)
  if (rt == "dy")              return(5L)
  if (grepl("DY$", vu))       return(5L)
  if (grepl("^VISIT$", vu) || grepl("VISIT$", vu) && !grepl("NUM|DY", vu))
    return(6L)
  if (grepl("VISITNUM$", vu)) return(7L)
  if (grepl("VISITDY$", vu))  return(8L)
  if (grepl("TPT$", vu))      return(9L)
  if (grepl("TPTNUM$", vu))   return(10L)
  if (grepl("TPTREF$", vu))   return(11L)
  if (grepl("ELTM$", vu))     return(12L)

  0L
}


# ---- Internal: dependency inference from DERIVATION text ---------------------

#' Extract referenced SDTM variables from a compiled rule
#'
#' Parses the derivation parameters (column, dtc_var, ref_var, conditions, etc.)
#' to discover which *other SDTM variables in the same domain* are referenced.
#' This is used to ensure that referenced variables are derived first.
#'
#' @param rule List. A compiled rule object from `rule_set$rules[[domain]][[var]]`.
#' @param dom_vars Character vector. All variable names in the domain.
#' @return Character vector of referenced variable names (may be empty).
#' @keywords internal
#' @noRd
.extract_refs_from_rule <- function(rule, dom_vars) {
  refs <- character()
  p <- rule$params %||% list()

  # --- Direct parameter references ---
  # dtc_var and ref_var in DY / epoch / visitdy rules

  for (pname in c("dtc_var", "ref_var", "start_dtc", "end_dtc",
                  "visit_var", "result_var")) {
    val <- p[[pname]]
    if (!is.null(val) && is.character(val) && length(val) == 1L) {
      val_up <- toupper(val)
      if (val_up %in% dom_vars) refs <- c(refs, val_up)
    }
  }

  # --- Columns / sources list ---
  cols <- p$columns %||% p$sources
  if (!is.null(cols)) {
    for (cv in unlist(cols)) {
      cv_up <- toupper(cv)
      if (cv_up %in% dom_vars) refs <- c(refs, cv_up)
    }
  }

  # --- case_when / if_else conditions: extract uppercase tokens ---
  cond_texts <- character()
  if (!is.null(p$condition) && is.character(p$condition)) {
    cond_texts <- p$condition
  }
  if (!is.null(p$conditions) && is.list(p$conditions)) {
    for (cond in p$conditions) {
      if (!is.null(cond$condition)) cond_texts <- c(cond_texts, cond$condition)
    }
  }
  # VLM branches
  if (!is.null(p$vlm_branches) && is.list(p$vlm_branches)) {
    for (br in p$vlm_branches) {
      if (!is.null(br$condition)) cond_texts <- c(cond_texts, br$condition)
    }
  }
  for (ct in cond_texts) {
    tokens <- regmatches(ct, gregexpr("[A-Z][A-Z0-9_]+", ct))[[1]]
    refs <- c(refs, intersect(tokens, dom_vars))
  }

  # --- method_string (raw DERIVATION text): scan for domain variable names ---
  ms <- rule$method_string
  if (!is.null(ms) && !is.na(ms) && nchar(ms) > 0L) {
    tokens <- regmatches(ms, gregexpr("[A-Z][A-Z0-9_]+", ms))[[1]]
    refs <- c(refs, intersect(tokens, dom_vars))
  }

  unique(refs)
}


# ---- Exported: compute derivation order --------------------------------------

#' Compute mandatory SDTM derivation order for a domain
#'
#' Orders variables 1-10 based on rule_type, variable naming patterns, and
#' intra-rule references parsed from the DERIVATION column.  No explicit
#' `depends_on` metadata is required.
#'
#' @param rule_set `rule_set` object (from [compile_rules()]).
#' @param domain Character. Domain code (e.g. `"AE"`).
#' @return Character vector of variable names in derivation order.
#' @keywords internal
#' @noRd
.compute_sdtm_derivation_order <- function(rule_set, domain) {
  dom_rules <- rule_set$rules[[domain]]
  if (is.null(dom_rules) || length(dom_rules) == 0L) return(character())

  all_vars <- names(dom_rules)

  # 1. Assign primary category (1-10)
  categories <- vapply(all_vars, function(v) {
    .derive_category(v, dom_rules[[v]]$type %||% NA_character_, domain)
  }, integer(1))

  # 2. Assign sub-category for fine-grained ordering within a category
  subcats <- vapply(all_vars, function(v) {
    .derive_subcategory(v, dom_rules[[v]]$type %||% NA_character_)
  }, integer(1))

  # 3. Infer intra-domain references from DERIVATION parameters
  #    If variable B references variable A, and both are in the same category,
  #    ensure A comes before B by adjusting B's sub-key.
  refs_map <- lapply(all_vars, function(v) {
    .extract_refs_from_rule(dom_rules[[v]], all_vars)
  })
  names(refs_map) <- all_vars

  # Bump variables that reference other same-category vars
  for (v in all_vars) {
    for (dep in refs_map[[v]]) {
      if (dep == v) next
      if (categories[[v]] == categories[[dep]] && subcats[[v]] <= subcats[[dep]]) {
        subcats[[v]] <- subcats[[dep]] + 1L
      }
    }
  }

  # 4. Sort: category → subcategory → alphabetical
  all_vars[order(categories, subcats, all_vars)]
}


#' Build a variable dependency graph for a domain
#'
#' Uses the mandatory SDTM 10-category derivation order.  Dependencies are
#' inferred automatically from rule_type, variable naming conventions, and
#' DERIVATION parameters — no explicit `depends_on` column required.
#'
#' @param rule_set `rule_set` object.
#' @param domain Character. Domain code.
#' @param allow_cycles Logical. Default `FALSE`. Ignored (no cycles possible
#'   with mandatory ordering).
#' @param late_bind_vars Character vector. Default `character()`.
#' @return Named list: `order` (char), `cycles` (list), `phases` (list),
#'   `graph` (`NULL`), `late_bind_plan` (tibble).
#' @export
build_dependency_graph <- function(rule_set, domain,
                                   allow_cycles = FALSE,
                                   late_bind_vars = character()) {
  dom_rules <- rule_set$rules[[domain]]
  if (is.null(dom_rules) || length(dom_rules) == 0L) {
    return(list(
      graph          = NULL,
      order          = character(),
      cycles         = list(),
      late_bind_plan = tibble::tibble(),
      phases         = list(phase1 = character())
    ))
  }

  var_order <- .compute_sdtm_derivation_order(rule_set, domain)

  list(
    graph          = NULL,
    order          = var_order,
    cycles         = list(),
    late_bind_plan = tibble::tibble(),
    phases         = list(phase1 = var_order)
  )
}


# ---- Legacy compatibility helpers --------------------------------------------

#' Topologically sort rules within a domain
#'
#' Retained for backward compatibility.  When called with the output of
#' [build_dependency_graph()] it simply returns the pre-computed `$order`.
#'
#' @param graph An `igraph` directed graph **or** the output list of
#'   `build_dependency_graph()`.
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
#'
#' @param graph `igraph` directed graph (or `NULL`).
#' @return List of cycle paths (empty when mandatory ordering is used).
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
#'
#' @param graph `igraph` graph or `NULL`.
#' @param cycles List of cycle paths.
#' @param late_bind_vars Character vector.
#' @return Named list.
#' @export
resolve_late_binding <- function(graph, cycles, late_bind_vars) {
  if (length(cycles) == 0L) {
    return(list(
      resolved_graph = graph,
      phases = list(
        phase1 = if (is.list(graph) && "order" %in% names(graph))
                   graph$order
                 else if (!is.null(graph) && igraph::is_igraph(graph))
                   igraph::V(graph)$name
                 else character()
      ),
      deferred_edges = tibble::tibble()
    ))
  }
  edges_to_remove <- c()
  el <- igraph::as_edgelist(graph)
  for (i in seq_len(nrow(el))) {
    if (el[i, 2] %in% late_bind_vars) edges_to_remove <- c(edges_to_remove, i)
  }
  g2 <- if (length(edges_to_remove) > 0L)
           igraph::delete_edges(graph, edges_to_remove)
         else graph
  phase1 <- topo_sort_rules(g2)
  list(resolved_graph = g2,
       phases = list(phase1 = phase1, phase2 = late_bind_vars),
       deferred_edges = tibble::tibble())
}


#' Generate a domain-level build plan
#'
#' Returns a tibble showing the derivation step number, variable name, and
#' rule type — useful for inspecting the automatically-computed order.
#'
#' @param domain Character. Domain code.
#' @param dep_graph Output of [build_dependency_graph()].
#' @param rule_set `rule_set` object.
#' @return Tibble with columns `step`, `variable`, `rule_type`, `category`.
#' @export
plan_build_steps <- function(domain, dep_graph, rule_set) {
  var_order <- dep_graph$order %||% character()
  if (length(var_order) == 0L) {
    return(tibble::tibble(
      step      = integer(),
      variable  = character(),
      rule_type = character(),
      category  = integer()
    ))
  }

  dom_rules <- rule_set$rules[[domain]]

  tibble::tibble(
    step      = seq_along(var_order),
    variable  = var_order,
    rule_type = vapply(var_order, function(v) {
      dom_rules[[v]]$type %||% "unknown"
    }, character(1)),
    category  = vapply(var_order, function(v) {
      .derive_category(v, dom_rules[[v]]$type %||% NA_character_, domain)
    }, integer(1))
  )
}
