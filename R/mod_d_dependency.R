# ==============================================================================
# Module D: Dependency & Ordering System
# ==============================================================================

#' Build a variable dependency graph for a domain
#' @param rule_set `rule_set` object.
#' @param domain Character.
#' @param allow_cycles Logical. Default `FALSE`.
#' @param late_bind_vars Character vector. Default `character()`.
#' @return Named list with `graph`, `order`, `cycles`, `phases`.
#' @export
build_dependency_graph <- function(rule_set, domain,
                                   allow_cycles = FALSE,
                                   late_bind_vars = character()) {
  dom_rules <- rule_set$rules[[domain]]
  if (is.null(dom_rules) || length(dom_rules) == 0L) {
    return(list(graph = NULL, order = character(), cycles = list(),
                late_bind_plan = tibble::tibble(), phases = list(phase1 = character())))
  }

  all_vars <- names(dom_rules)
  edges_df <- rule_set$dependency_info[[domain]]

  # Build igraph
  if (is.null(edges_df) || nrow(edges_df) == 0L) {
    # No dependencies; sort by order field or alphabetically
    ordered <- all_vars
    if (!is.null(dom_rules[[1]]$order)) {
      orders <- vapply(dom_rules, function(r) as.numeric(r$order %||% 999), numeric(1))
      ordered <- all_vars[order(orders)]
    }
    return(list(graph = NULL, order = ordered, cycles = list(),
                late_bind_plan = tibble::tibble(), phases = list(phase1 = ordered)))
  }

  # Filter edges to only internal vars
  edges_df <- dplyr::filter(edges_df, .data$from_var %in% all_vars,
                             .data$to_var %in% all_vars)

  if (nrow(edges_df) == 0L) {
    ordered <- all_vars
    if (!is.null(dom_rules[[1]]$order)) {
      orders <- vapply(dom_rules, function(r) as.numeric(r$order %||% 999), numeric(1))
      ordered <- all_vars[order(orders)]
    }
    return(list(graph = NULL, order = ordered, cycles = list(),
                late_bind_plan = tibble::tibble(), phases = list(phase1 = ordered)))
  }

  g <- igraph::graph_from_data_frame(
    edges_df[, c("from_var", "to_var")],
    directed = TRUE,
    vertices = data.frame(name = all_vars)
  )

  # Detect cycles
  cycles_found <- detect_cycles(g)

  if (length(cycles_found) > 0L && !allow_cycles) {
    abort(paste("Circular dependency detected in domain", domain, ":",
                paste(cycles_found[[1]], collapse = " -> ")))
  }

  # Topological sort
  sorted <- topo_sort_rules(g)

  # Add any vars not in graph
  missing_vars <- setdiff(all_vars, sorted)
  sorted <- c(missing_vars, sorted)

  list(
    graph = g,
    order = sorted,
    cycles = cycles_found,
    late_bind_plan = tibble::tibble(),
    phases = list(phase1 = sorted)
  )
}

#' Topologically sort rules within a domain
#' @param graph An `igraph` directed graph or output of `build_dependency_graph`.
#' @return Character vector of variable names in derivation order.
#' @export
topo_sort_rules <- function(graph) {
  if (is.list(graph) && !igraph::is.igraph(graph)) {
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
  if (is.null(graph) || !igraph::is.igraph(graph)) return(list())
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
                phases = list(phase1 = igraph::V(graph)$name),
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
  if (length(edges_to_remove) > 0) {
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
#' @param source_meta Tibble.
#' @return Tibble with build steps.
#' @export
plan_build_steps <- function(domain, dep_graph, rule_set, source_meta) {
  var_order <- dep_graph$order
  steps <- tibble::tibble(
    step    = seq_along(var_order),
    phase   = rep("derive", length(var_order)),
    action  = rep("derive", length(var_order)),
    target  = var_order,
    details = vapply(var_order, function(v) {
      r <- rule_set$rules[[domain]][[v]]
      if (!is.null(r)) r$type else "unknown"
    }, character(1)),
    depends_on_step = rep(NA_integer_, length(var_order))
  )
  steps
}
