#' Export ODK Qualitative Data Into Per-Observation Folders
#'
#' @title Export ODK Qualitative Data
#'
#' @description
#' This function processes a directory containing **ODK Central**
#' CSV export files-consisting of one **main** dataset and optional **repeat group**
#' datasets. It generates a folder structure tailored for qualitative review:
#'
#' * One folder per observation/interview (e.g., `obs001`, `obs002`, ...).
#' * An Excel file containing the row data from the main dataset.
#' * Additional Excel files for each repeat group linked to that row.
#' * Copies of all media files (e.g., `.jpg`, `.amr`, `.m4a`) referenced in both the main
#'   and repeat rows, renamed in a structured, traceable way.
#'
#' The goal is to create a clean, reviewer-friendly qualitative package where each
#' observation has its own folder with all associated metadata and media.
#'
#' @param indir Path to a directory containing:
#'   * One **main** CSV file (the root of all other files).
#'   * One CSV file per **repeat group** (`<main>-<repeat>.csv`).
#'   * A subfolder named `"media"` containing referenced audio/images.
#'   The main file is automatically detected as the filename prefix that appears most
#'   frequently before a hyphen.
#' @param outdir Output directory where observation folders will be created.
#'   If it does not exist, it will be created recursively.
#' @param prefix Character string to prefix the observation folders (default is "obs").
#' @param sorting_col Optional. Name of a column in the main dataset used to create a second-level
#'   folder hierarchy.
#'   For example, if `sorting_col = "district"` and your main row has
#'   `district = "North"`, the output path becomes `outdir/North/obs001/`.
#'   If `sorting_col` is `NULL` or not present in the main dataset, all observation
#'   folders are placed directly under `outdir`.
#' @param verbose Logical. If `TRUE` (default), prints progress messages to the console.
#' @param overwrite Logical. If `TRUE` (default), overwrites existing files in the output directory.
#'
#' @details
#' ## Output structure
#'
#' For each row *i* in the main CSV file, the function creates:
#'
#' ```
#' outdir/
#'   [sorting_folder/]         # optional
#'     obsXXX/                 # e.g., obs001
#'       XXX_main.xlsx         # full label/value pair table for that row
#'       XXX_<repeat>.xlsx     # one file per repeat group (if applicable)
#'       XXX_<variable>.<ext>  # media from the main row
#'       XXX_steps_<variable_row>.<ext>  # media from repeat rows
#' ```
#'
#' Media files referenced in CSV rows (columns ending in `.jpg` or `.amr` or `.m4a`) are copied
#' from `indir/media/` and renamed to embed:
#' * the observation ID (`XXX`).
#' * the variable name.
#' * (for repeats) the repeat-row index.
#'
#' Missing media files trigger a warning but do not stop execution.
#'
#' @return Invisibly returns `NULL` after writing all files.
#'   Side-effects: creates folders and writes Excel files and copied media files.
#'
#' @importFrom openxlsx write.xlsx
#' @importFrom readr read_csv
#' @importFrom stringr str_remove str_replace_all str_extract str_detect str_match
#' @importFrom dplyr filter mutate row_number across everything
#' @importFrom tidyr pivot_longer
#' @importFrom progress progress_bar
#' @importFrom tools file_ext
#'
#' @examples
#' \dontrun{
#' # Standard export
#' structure_qual_observations(
#'   indir = "exports/my_odk_form/",
#'   outdir = "qualitative_output/"
#' )
#'
#' # Export with subfolders based on a column
#' structure_qual_observations(
#'   indir = "exports/my_odk_form/",
#'   outdir = "qualitative_output/",
#'   sorting_col = "enumerator_id"
#' )
#' }
#'
#' @export
structure_qual_observations <- function(
    indir,
    outdir,
    prefix = "obs",
    sorting_col = NULL,
    verbose = TRUE,
    overwrite = TRUE
) {

  # -------------------------------
  # Package checks
  # -------------------------------
  # Define the list of required packages for this function to run
  req_pkgs <- c("openxlsx", "readr", "stringr", "dplyr", "tidyr", "progress")

  # Check if each package is installed and loadable
  lapply(req_pkgs, function(p) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required.", p), call. = FALSE)
    }
  })

  # -------------------------------
  # Identify files in the input directory
  # -------------------------------
  # List all CSV files in the provided input directory
  files <- list.files(indir, pattern = "\\.csv$", recursive = FALSE)

  if (length(files) == 0)
    stop("No CSV files found in 'indir'.", call. = FALSE)

  # Strip ".csv" extension and normalize spaces to hyphens to analyze file roots
  roots <- files |>
    stringr::str_remove("\\.csv$") |>
    stringr::str_replace_all(" - ", "-") |>
    stringr::str_extract("^[^-]+") # Extract the prefix before the first hyphen

  # Determine the main filename root by finding the most frequent prefix
  main_root <- names(sort(table(roots), decreasing = TRUE))[1]
  main_file <- file.path(indir, paste0(main_root, ".csv"))

  # Verify the main file exists
  if (!file.exists(main_file))
    stop("Detected main CSV file does not exist: ", main_file)

  if (verbose)
    message("Main form detected: ", basename(main_file))

  # Identify repeat group files based on the pattern "main_root-something.csv"
  repeat_files <- files[
    stringr::str_detect(files, paste0("^", main_root, "-.+\\.csv$"))
  ]

  # -------------------------------
  # Read main CSV
  # -------------------------------
  # Read the main dataset without showing column type messages
  main_df <- readr::read_csv(main_file, show_col_types = FALSE)

  # Ensure the mandatory ODK 'KEY' column exists for linking repeats
  if (!"KEY" %in% names(main_df))
    stop("Main dataset does not contain required column 'KEY'.", call. = FALSE)

  # Create the output directory if it doesn't exist
  if (!dir.exists(outdir))
    dir.create(outdir, recursive = TRUE)

  # Initialize a progress bar to track processing of observations
  pb <- progress::progress_bar$new(
    total = nrow(main_df),
    format = "Processing rows [:bar] :current/:total (:percent) ETA: :eta"
  )

  # -------------------------------
  # Loop through each observation/interview
  # -------------------------------
  for (i in seq_len(nrow(main_df))) {
    pb$tick()

    # Extract the current row as a single-row dataframe
    row <- main_df[i, , drop = FALSE]

    # Determine subfolder path based on sorting_col (if provided and valid)
    if (!is.null(sorting_col) && sorting_col %in% names(row)) {
      subdir <- file.path(outdir, as.character(row[[sorting_col]]))
      # Create sub-directory if it doesn't exist
      if (!dir.exists(subdir)) dir.create(subdir, recursive = TRUE)
    } else {
      subdir <- outdir
    }

    # Create the specific observation folder (e.g., obs001)
    obs_dir <- file.path(subdir, sprintf("%s%03d", prefix, i))
    if (!dir.exists(obs_dir)) dir.create(obs_dir, recursive = TRUE)

    # Define the root name for Excel files (e.g., 001)
    excel_root <- sprintf("%03d", i)

    # ----------------------------------
    # Write main row as <xxx>_main.xlsx
    # ----------------------------------
    # Transpose the row data into a key-value format for easier reading
    row_df <- data.frame(
      label = names(row),
      value = as.character(unlist(row)),
      stringsAsFactors = FALSE
    )

    main_xlsx_path <- file.path(obs_dir, sprintf("%s_main.xlsx", excel_root))
    openxlsx::write.xlsx(row_df, main_xlsx_path, overwrite = overwrite)

    # Get the unique key for this observation to link with repeats
    uuid <- row$KEY

    # ----------------------------------
    # Copy media in main row
    # ----------------------------------
    # Filter for columns that contain media file references (images/audio)
    media_rows <- row_df |>
      dplyr::filter(
        !is.na(value),
        nzchar(value),
        grepl("\\.(amr|m4a|jpg)$", value, ignore.case = TRUE)
      )

    # Iterate through found media files and copy them to the observation folder
    for (j in seq_len(nrow(media_rows))) {
      src <- file.path(indir, "media", media_rows$value[j])
      if (file.exists(src)) {
        ext <- tools::file_ext(src)
        # Rename file to include observation ID and variable name
        dest <- file.path(obs_dir, sprintf("%s_%s.%s",
                                           excel_root,
                                           media_rows$label[j],
                                           ext))
        file.copy(src, dest, overwrite = overwrite)
      } else if (verbose) {
        warning("Missing media: ", src)
      }
    }

    # ----------------------------------
    # Handle repeat groups
    # ----------------------------------
    for (rf in repeat_files) {
      # Extract the repeat group name from the filename
      repeat_name <- stringr::str_match(rf, ".*-(.+)\\.csv$")[, 2]
      repeat_df <- readr::read_csv(file.path(indir, rf), show_col_types = FALSE)

      # Ensure repeat file can be linked to parent
      if (!"PARENT_KEY" %in% names(repeat_df))
        stop("Repeat file lacks required PARENT_KEY column: ", rf)

      # Filter repeat data for the current observation's UUID
      sub_df <- repeat_df[repeat_df$PARENT_KEY == uuid, , drop = FALSE]

      if (nrow(sub_df) == 0) next

      # Write the filtered repeat data to an Excel file
      repeat_xlsx_path <- file.path(
        obs_dir,
        sprintf("%s_%s.xlsx", excel_root, repeat_name)
      )

      openxlsx::write.xlsx(sub_df, repeat_xlsx_path, overwrite = overwrite)

      # Find media inside repeat rows
      # Transform data to long format to identify media columns easily
      media_hits <- sub_df |>
        dplyr::mutate(row_num = dplyr::row_number()) |>
        dplyr::mutate(across(everything(), as.character)) |>
        tidyr::pivot_longer(cols = -row_num, names_to = "label", values_to = "value") |>
        dplyr::filter(
          !is.na(value),
          nzchar(value),
          grepl("\\.(amr|m4a|jpg)$", value, ignore.case = TRUE)
        )

      # Copy and rename media files from repeat groups
      for (j in seq_len(nrow(media_hits))) {
        src <- file.path(indir, "media", media_hits$value[j])
        if (file.exists(src)) {
          ext <- tools::file_ext(src)
          # Rename file to include obs ID, variable name, and row number within repeat
          dest <- file.path(obs_dir,
                            sprintf("%s_steps_%s_%s.%s",
                                    excel_root,
                                    media_hits$label[j],
                                    media_hits$row_num[j],
                                    ext))
          file.copy(src, dest, overwrite = overwrite)
        } else if (verbose) {
          warning("Missing media: ", src)
        }
      }
    }
  }

  if (verbose)
    message("\u2714 Export completed successfully.")

  invisible(NULL)
}
