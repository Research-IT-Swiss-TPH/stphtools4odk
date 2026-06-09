# =============================================================================
# ODK Pipeline Utilities
# =============================================================================
# A set of reusable functions to clean, label, and export ODK form submissions.
#
# Typical usage:
#
#   fs  <- odk_get_schema("my_form_id")
#   df  <- ruODK::odata_submission_get(fid = "my_form_id")
#   out <- odk_build_export(df, fs)
#   odk_write_stata(out, "stata/my_form.dta")
#   odk_write_codebook(out, "stata/my_form_codebook.docx")
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Schema retrieval & cleaning
# -----------------------------------------------------------------------------
#' Fetch and clean an ODK form schema
#'
#' @param fid        Form ID string passed to ruODK::form_schema_ext().
#' @param label_col  Column name of the label to use. If NULL (default),
#'                   the first column matching "^label" is used automatically.
#'                   Override when the form has multiple languages
#'                   (e.g. "label_english_(en)", "label_français_(fr)").
#' @param choices_col Column name of the choices to use. If NULL (default),
#'                   the first column matching "^choices" is used automatically.
#'                   Override when the form has multiple languages
#'                   (e.g. "choices_english_(en)", "choices_français_(fr)").
#' @param replacements Optional character vector of regex pattern → replacement
#'                   applied to the `answers` column. Names are regex patterns,
#'                   values are replacements. Default: NULL.
#' @param extra_exclude_patterns Optional character vector of additional regex
#'                   patterns to exclude rows by `ruodk_name`, concatenated
#'                   with the built-in exclusions. Default: NULL.
#'
#' @return A tibble with columns: ruodk_name, lbl, answers, selectMultiple.
odk_get_schema <- function(
    fid,
    label_col   = NULL,
    choices_col = NULL,
    replacements = NULL,
    extra_exclude_patterns = NULL
) {
  clean_text <- function(x) {
    x |>
      stringr::str_remove_all("[\\p{So}]") |>
      stringr::str_remove_all("\\*\\*|<u>|</u>") |>
      stringr::str_squish()
  }

  raw <- ruODK::form_schema_ext(fid = fid)

  if (is.null(label_col)) {
    label_col <- grep("^label", names(raw), value = TRUE)[1]
    if (is.na(label_col)) stop("No 'label' column found in schema; specify `label_col` explicitly.")
  }
  if (is.null(choices_col)) {
    choices_col <- grep("^choices", names(raw), value = TRUE)[1]
    if (is.na(choices_col)) stop("No 'choices' column found in schema; specify `choices_col` explicitly.")
  }

  schema <- raw |>
    dplyr::rename(lbl = dplyr::all_of(label_col),
                  answers = dplyr::all_of(choices_col)) |>
    dplyr::mutate(
      lbl     = clean_text(lbl),
      answers = clean_text(answers),
      answers = stringr::str_remove_all(answers, '(?<=")\\s+')
    )

  # apply string replacements to answers
  for (pattern in names(replacements)) {
    schema <- dplyr::mutate(
      schema,
      answers = stringr::str_replace(answers, pattern, replacements[[pattern]])
    )
  }

  base_exclude <- c("^generated_note_name_")
  exclude_pattern <- paste(
    c(base_exclude, extra_exclude_patterns),
    collapse = "|"
  )

  schema |>
    dplyr::filter(
      !stringr::str_detect(ruodk_name, exclude_pattern)
    ) |>
    dplyr::select(ruodk_name, lbl, answers, selectMultiple)
}

# -----------------------------------------------------------------------------
# 2. Value mapping
# -----------------------------------------------------------------------------

#' Map coded values to labels for a single column
#'
#' @param x        Vector of coded values.
#' @param col_name Name of the column (matched against schema$ruodk_name).
#' @param schema   Form schema tibble from odk_get_schema().
#'
#' @return Vector with codes replaced by labels where a mapping exists.
odk_map_col <- function(x, col_name, schema) {
  choice_map <- NULL
  row_idx    <- which(schema$ruodk_name == col_name)

  if (length(row_idx) > 0) {
    raw <- schema$answers[[row_idx]]
    if (is.character(raw) && raw != "NULL") {
      choice_map <- tryCatch(eval(parse(text = raw)), error = function(e) NULL)
    }
  }

  if (is.list(choice_map) && "values" %in% names(choice_map)) {
    vals      <- as.character(choice_map$values)
    lbls      <- as.character(choice_map$labels)
    valid_idx <- which(!is.na(vals) & vals != "")

    if (length(valid_idx) > 0) {
      lookup   <- stats::setNames(lbls[valid_idx], vals[valid_idx])
      replaced <- lookup[as.character(x)]
      return(ifelse(is.na(replaced), x, replaced))
    }
  }

  x
}


# -----------------------------------------------------------------------------
# 3. Select-multiple expansion
# -----------------------------------------------------------------------------

#' Expand one select_multiple column into binary dummy columns
#'
#' @param df      Data frame containing the column to expand.
#' @param var     Name of the select_multiple column.
#' @param schema  Form schema tibble from odk_get_schema().
#' @param id_col  Name of the row identifier column. Default: "id".
#'
#' @return A tibble with `id_col` plus one 0/1 integer column per choice value.
odk_expand_multi <- function(df, var, schema, id_col = "id") {
  var_sym    <- rlang::sym(var)
  id_sym     <- rlang::sym(id_col)
  prefix     <- paste0(var, "_")

  all_values <- schema |>
    dplyr::filter(ruodk_name == var) |>
    dplyr::pull(answers) |>
    (\(x) eval(parse(text = x)))() |>
    purrr::pluck("values")

  df |>
    dplyr::select(!!id_sym, !!var_sym) |>
    tidyr::separate_rows(!!var_sym, sep = " ") |>
    dplyr::mutate(!!var_sym := factor(!!var_sym, levels = all_values)) |>
    dplyr::mutate(value = 1L) |>
    tidyr::pivot_wider(
      id_cols      = !!id_sym,
      names_from   = !!var_sym,
      values_from  = value,
      values_fill  = 0L,
      names_prefix = prefix,
      names_sort   = TRUE,
      names_expand = TRUE
    ) |>
    dplyr::select(-dplyr::ends_with("_NA"))
}


# -----------------------------------------------------------------------------
# 4. Full export build
# -----------------------------------------------------------------------------

#' Build a labelled export data frame from raw ODK submissions
#'
#' Combines schema filtering, select-multiple expansion, value mapping,
#' variable labelling, and column renaming into one step.
#'
#' @param df      Raw submissions data frame (e.g. from ruODK::odata_submission_get()).
#' @param schema  Cleaned schema tibble from odk_get_schema().
#' @param id_col  Name of the row identifier column. Default: "id".
#'
#' @return A labelled tibble ready for export.
odk_build_export <- function(df, schema, id_col = "id") {

  # --- 4a. Keep only schema variables (+ id) --------------------------------
  df <- df |>
    dplyr::select(dplyr::any_of(c(schema$ruodk_name, id_col)))

  # --- 4b. Expand select_multiple columns ------------------------------------
  multi_vars   <- schema |> dplyr::filter(selectMultiple) |> dplyr::pull(ruodk_name)
  expanded_list <- purrr::map(
    multi_vars,
    ~ odk_expand_multi(df, .x, schema, id_col = id_col)
  ) |>
    purrr::set_names(multi_vars)

  cleaned_df <- df
  for (var in multi_vars) {
    if (var %in% names(cleaned_df)) {
      dummy_df   <- expanded_list[[var]]
      dummy_cols <- setdiff(names(dummy_df), id_col)
      cleaned_df <- cleaned_df |>
        dplyr::left_join(dummy_df, by = id_col) |>
        dplyr::relocate(dplyr::all_of(dummy_cols), .after = dplyr::all_of(var))
    }
  }

  # --- 4c. Map coded values to labels (skip dummy columns) ------------------
  dummy_cols <- expanded_list |>
    purrr::map(~ dplyr::select(.x, -dplyr::all_of(id_col)) |> names()) |>
    unlist()

  export_df <- cleaned_df |>
    dplyr::mutate(dplyr::across(
      -dplyr::all_of(dummy_cols),
      ~ odk_map_col(.x, dplyr::cur_column(), schema)
    ))

  # --- 4d. Build variable labels --------------------------------------------
  dummy_label_list <- purrr::map(multi_vars, function(var) {
    answer_pairs <- schema |>
      dplyr::filter(ruodk_name == var) |>
      dplyr::pull(answers) |>
      (\(x) eval(parse(text = x)))()
    purrr::set_names(answer_pairs$labels, paste0(var, "_", answer_pairs$values))
  }) |>
    purrr::flatten()

  regular_label_list <- schema |>
    dplyr::filter(ruodk_name %in% names(export_df)) |>
    dplyr::filter(!is.na(lbl), lbl != "") |>
    dplyr::select(ruodk_name, lbl) |>
    tibble::deframe() |>
    as.list()

  labelled::var_label(export_df) <- c(regular_label_list, dummy_label_list)

  # --- 4e. Strip form-prefix from column names (skip duplicates) ------------
  new_names     <- stringr::str_remove(names(export_df), "^[^_]+_")
  duplicated_new <- new_names[duplicated(new_names) | duplicated(new_names, fromLast = TRUE)]

  export_df |>
    dplyr::rename_with(~ ifelse(
      .x %in% names(export_df)[new_names %in% duplicated_new],
      .x,
      stringr::str_remove(.x, "^[^_]+_")
    ))
}

# -----------------------------------------------------------------------------
# 5. Export helpers
# -----------------------------------------------------------------------------

#' Write export data frame to a Stata .dta file
#'
#' @param df   Labelled data frame from odk_build_export().
#' @param path Output file path.
odk_write_stata <- function(df, path) {
  haven::write_dta(df, path)
  message("Stata file written: ", path)
}

#' Write a codebook Word document
#'
#' @param df       Labelled data frame from odk_build_export().
#' @param path     Output .docx file path.
#' @param title    Codebook title string.
#' @param subtitle Codebook subtitle string.
odk_write_codebook <- function(
    df,
    path,
    title    = "ODK Codebook",
    subtitle = "Generated by odk_pipeline.R"
) {
  cb <- codebookr::codebook(df, title = title, subtitle = subtitle)
  print(cb, path)
  message("Codebook written: ", path)
}
