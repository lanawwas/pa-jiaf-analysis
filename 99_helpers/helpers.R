# Module constants
aggregated_data_dir <- "Data aggregated for analysis"


#' Generate directories and paths for PiN analysis
#'
#' @param country String with country name
#' @param country_name If `country` is not an easily interpretable
#'     country name, and thus retrieval of an ISO3 code impossible,
#'     pass `country_name` instead.
#' @param file_type One of `"sectoral"`, `"household"`, or `"indicator"`.
#'     Determines the save file path.
#'
#' @return Named list of:
#'     * OCHA data directory
#'     * Cluster data directory
#'     * Save file path
get_paths <- function(country, country_name = NULL, file_type = "sectoral") {
  if (is.null(country_name)) {
    country_name <- country
  }

  save_path_dir <- switch(file_type,
    "sectoral" = "sectoral_pins",
    "indicator" = "indicator_pins",
    "household" = "hh_data",
    stop("`file_type` must be `sectoral`, `indicator`, or `household`.")
  )

  # iso3 for use in save path
  iso3 <- tolower( # nolint
    countrycode::countryname(
      country_name,
      destination = "iso3c"
    )
  )

  data_dir <- Sys.getenv("JIAF_DATA_DIR")

  # directory with ocha provided data
  ocha_dir <- file.path(
    data_dir,
    "Data from country offices - OCHA",
    country
  )

  # directory with cluster provided data
  cluster_dir <- file.path(
    data_dir,
    "Data from country offices - Clusters",
    country
  )

  # file path to save pin data
  save_path <- file.path(
    data_dir,
    aggregated_data_dir,
    save_path_dir,
    glue::glue("{iso3}_pins_2022.csv")
  )

  list(
    ocha_dir = ocha_dir,
    cluster_dir = cluster_dir,
    save_path = save_path
  )
}

#' @param input_type One of `"sectoral"`, `"household"`, or `"indicator"`.
#'     Determines the input file path.
get_paths_analysis <- function(input_type = "sectoral") {

  # TODO: Make name of this a constant
  data_dir <- Sys.getenv("JIAF_DATA_DIR")

  input_type_dir <- switch(input_type,
    "sectoral" = "sectoral_pins",
    "indicator" = "indicator_pins",
    "household" = "hh_data",
    stop("`input_type` must be `sectoral`, `indicator`, or `household`.")
  )

  input_dir <- file.path(
    data_dir,
    aggregated_data_dir,
    input_type_dir
  )

  output_dir <- file.path(
    data_dir,
    "Data analyzed"
  )
  list(input_dir = input_dir, output_dir = output_dir)
}
