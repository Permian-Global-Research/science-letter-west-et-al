#' @title function to create directory if it doesn't exist
#' @param x character; path to directory
#' @return NULL
safe_create <- function(x) {
  if (!dir.exists(x)) dir.create(x)
  return(invisible())
}

#' @title function to append data directory to path
#' @param x character; filename
#' @param data_dir character; path to data directory
#' @return character; path to file
dp <- function(
    x,
    data_dir = "west-data/West et al. (2023) data/Main datasets") {
  safe_create(data_dir)
  file.path(data_dir, x)
}
