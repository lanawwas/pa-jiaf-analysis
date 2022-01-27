library(janitor)

# additional arguments passed to make_clean_names
clean_names <- function(df, ...) {
  names(df) <- make_clean_names(names(df), ...)
  df
}
