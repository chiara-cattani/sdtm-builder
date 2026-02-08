# ==============================================================================
# Module F: Join & Record Assembly Utilities
# ==============================================================================

#' Safe join with cardinality assertions
#' @param x Tibble. Left dataset.
#' @param y Tibble. Right dataset.
#' @param by Character vector. Join keys.
#' @param type Character. Default `"left"`.
#' @param cardinality Character. Default `"1:1"`.
#' @param on_violation Character. Default `"error"`.
#' @param dedupe_rule List or `NULL`.
#' @param merge_indicator Character or `NULL`.
#' @param suffix Character vector. Default `c(".x", ".y")`.
#' @return Tibble.
#' @export
safe_join <- function(x, y, by, type = "left", cardinality = "1:1",
                      on_violation = "error", dedupe_rule = NULL,
                      merge_indicator = NULL, suffix = c(".x", ".y")) {
  # Validate by columns exist
  miss_x <- setdiff(by, names(x))
  miss_y <- setdiff(by, names(y))
  if (length(miss_x)) abort(paste("Join keys missing from x:", paste(miss_x, collapse = ", ")))
  if (length(miss_y)) abort(paste("Join keys missing from y:", paste(miss_y, collapse = ", ")))

  # Check cardinality on y side for 1:1 or m:1
  if (cardinality %in% c("1:1", "m:1")) {
    dupes_y <- y %>% dplyr::count(dplyr::across(dplyr::all_of(by))) %>%
      dplyr::filter(.data$n > 1L)
    if (nrow(dupes_y) > 0L) {
      msg <- glue::glue("Cardinality violation ({cardinality}): y has duplicate keys")
      if (on_violation == "error") abort(msg)
      else if (on_violation == "warn") warn(msg)
      else if (on_violation == "dedupe") {
        y <- dplyr::distinct(y, dplyr::across(dplyr::all_of(by)), .keep_all = TRUE)
      }
    }
  }

  join_fn <- switch(type,
    left  = dplyr::left_join,
    inner = dplyr::inner_join,
    full  = dplyr::full_join,
    anti  = dplyr::anti_join,
    semi  = dplyr::semi_join,
    abort(glue::glue("Unknown join type: {type}"))
  )

  result <- join_fn(x, y, by = by, suffix = suffix)

  if (!is.null(merge_indicator)) {
    result[[merge_indicator]] <- dplyr::case_when(
      !is.na(result[[by[1]]]) ~ "matched",
      TRUE ~ "unmatched"
    )
  }

  result
}

#' Resolve domain keys
#' @param data Tibble.
#' @param domain Character.
#' @param target_meta Tibble.
#' @param config `sdtm_config`.
#' @return Named list.
#' @export
resolve_keys <- function(data, domain, target_meta, config) {
  dm <- dplyr::filter(target_meta, .data$domain == !!domain)
  if ("is_key" %in% names(dm)) {
    keys <- dm$var[dm$is_key == TRUE | dm$is_key == "Y"]
  } else {
    keys <- c("STUDYID", "USUBJID", paste0(domain, "SEQ"))
  }
  list(keys = keys, sort_keys = keys)
}

#' Deduplicate records according to a rule
#' @param data Tibble.
#' @param keys Character vector.
#' @param strategy Character. Default `"first"`.
#' @param order_by Character or `NULL`.
#' @param priority_col Character or `NULL`.
#' @param agg_fns Named list or `NULL`.
#' @return Tibble.
#' @export
deduplicate_by_rule <- function(data, keys, strategy = "first",
                                order_by = NULL, priority_col = NULL,
                                agg_fns = NULL) {
  if (!is.null(order_by)) data <- dplyr::arrange(data, dplyr::across(dplyr::all_of(order_by)))
  if (strategy == "first") {
    data <- dplyr::distinct(data, dplyr::across(dplyr::all_of(keys)), .keep_all = TRUE)
  } else if (strategy == "last") {
    data <- data %>% dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
      dplyr::slice_tail(n = 1L) %>% dplyr::ungroup()
  }
  data
}

#' Stack multiple source datasets
#' @param ... Tibbles or a named list.
#' @param source_names Character vector.
#' @param fill_missing Logical. Default `TRUE`.
#' @return Tibble.
#' @export
bind_sources <- function(..., source_names = NULL, fill_missing = TRUE) {
  dfs <- list(...)
  if (length(dfs) == 1L && is.list(dfs[[1]]) && !is.data.frame(dfs[[1]])) {
    dfs <- dfs[[1]]
  }
  if (!is.null(source_names) && length(source_names) == length(dfs)) {
    for (i in seq_along(dfs)) dfs[[i]]$.__source__ <- source_names[i]
  }
  dplyr::bind_rows(dfs)
}

#' Add provenance columns
#' @param data Tibble.
#' @param source_name Character.
#' @param rule_id Character.
#' @return Tibble.
#' @export
add_provenance_cols <- function(data, source_name = NA_character_,
                                rule_id = NA_character_) {
  data$.__source__ <- source_name
  data$.__source_row__ <- seq_len(nrow(data))
  data$.__rule_id__ <- rule_id
  data
}

#' Split delimited values into multiple records
#' @param data Tibble.
#' @param col Character.
#' @param sep Character. Default `";"`.
#' @param trim Logical. Default `TRUE`.
#' @param id_cols Character vector or `NULL`.
#' @param new_col Character or `NULL`.
#' @param drop_empty Logical. Default `TRUE`.
#' @return Tibble.
#' @export
split_records <- function(data, col, sep = ";", trim = TRUE,
                          id_cols = NULL, new_col = NULL,
                          drop_empty = TRUE) {
  if (is.null(new_col)) new_col <- col
  data <- tidyr::separate_rows(data, dplyr::all_of(col), sep = sep)
  if (trim) data[[col]] <- trimws(data[[col]])
  if (drop_empty) data <- dplyr::filter(data, data[[col]] != "" | is.na(data[[col]]))
  if (new_col != col) {
    data[[new_col]] <- data[[col]]
    data[[col]] <- NULL
  }
  data
}

#' Expand checkbox columns into records
#' @param data Tibble.
#' @param cols Character vector.
#' @param map Named list.
#' @param id_cols Character vector.
#' @param checked_values Character vector.
#' @param drop_unchecked Logical. Default `TRUE`.
#' @param unchecked_flag Character or `NULL`.
#' @return Tibble.
#' @export
expand_checkbox <- function(data, cols, map, id_cols,
                            checked_values = c("Y","1","TRUE","Yes","X"),
                            drop_unchecked = TRUE, unchecked_flag = NULL) {
  rows <- list()
  for (i in seq_len(nrow(data))) {
    for (col in cols) {
      val <- toupper(as.character(data[[col]][i]))
      is_checked <- val %in% toupper(checked_values)
      if (is_checked || !drop_unchecked) {
        row_data <- data[i, id_cols, drop = FALSE]
        if (!is.null(map[[col]])) {
          for (nm in names(map[[col]])) row_data[[nm]] <- map[[col]][[nm]]
        }
        if (!is_checked && !is.null(unchecked_flag)) {
          row_data[[unchecked_flag]] <- "NOT DONE"
        }
        rows <- c(rows, list(row_data))
      }
    }
  }
  if (length(rows) == 0) return(tibble::tibble())
  dplyr::bind_rows(rows)
}

#' Expand records across visit schedule
#' @param data Tibble.
#' @param visit_map Tibble.
#' @param by Character vector.
#' @param filter_fn Function or `NULL`.
#' @return Tibble.
#' @export
expand_visits <- function(data, visit_map, by, filter_fn = NULL) {
  result <- tidyr::crossing(data, visit_map)
  if (!is.null(filter_fn)) result <- dplyr::filter(result, filter_fn)
  result
}
