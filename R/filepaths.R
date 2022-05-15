#' Open a filepath
#'
#' @inherit fs::file_show
#'
#' @export
op <- function(path = ".", browser = getOption("browser")) {
  fs::file_show(path = path, browser = browser)
}

#' Load all custom functions in a project
#'
#' This helper function loads all the project-specific custom functions from the
#' specified directory. Specifically it calls \code{\link{source}} on all files
#' in the designated folder.
#'
#' @param path (character; default = "R/") the directory that contains scripts
#'   that only contain functions.
#' @param recursive (logical; default = "TRUE") should subfolders be included?
#'
#' @export
#'
load_custom_functions <- function(path = "R/",
                                  recursive = TRUE) {
  assertthat::assert_that(dir.exists(path),
    msg = "Double check that your directory exists"
  )

  funcs <- dir(
    path,
    recursive = recursive,
    full.names = TRUE
  )

  funcs %>%
    purrr::discard(dir.exists) %>%
    purrr::walk(source)
}


#' Download a file to a temporary location
#'
#' Excel documents cannot be loaded from a URL and need to be downloaded to a
#' temporary file. This is a reusable and generalisable wrapper to do just that.
#'
#' @param url (character) the URL to the destination file
#' @param fileext (character; default = \code{file_ext(url)}) the file
#'   extension of the file to download.
#' @param dir (character; default = \code{tempdir()}) the folder into which to
#'   download the file. Use dot notation to download into the working directory.
#' @param name (character; default = \code{NULL}) specify a name if you feel
#'   like it
#'
#' @return a file path string to the downloaded file
#' @export
#' @examples
#' \dontrun{
#' download_file(
#'   url = "https://github.com/baslat/aus_geos_data/blob/master/geos.csv?raw=true",
#'   fileext = "csv",
#'   dir = "./outputs",
#'   name = "geos.csv"
#' )
#' }
#'
download_file <- function(url,
                          fileext = file_ext(url),
                          dir = tempdir(),
                          name = NULL) {
  name <- name %||% (basename(url) %>%
    fs::path_sanitize() %>%
    stringr::str_remove(fileext) %>%
    paste0(".", fileext))


  dir <- normalizePath(dir)
  path <- file.path(dir, name)

  httr::GET(
    url,
    httr::write_disk(
      path = path,
      overwrite = TRUE
    )
  )

  return(path)
}


#' Make directory if needed
#'
#' Takes an input path (either the full path, including file extension, or just
#' the folder path). Checks to see if the directory exists. If the directory
#' does not exist, the function creates that directory. The function does
#' nothing if the directory exits.
#'
#' @param path (string) file path (or directory path)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- "outputs/new_directory/stuff_for_today"
#' make_dir_if_needed(path)
#' }
add_dir <- function(path) {
  assertthat::assert_that(is.character(path),
    msg = "`path` must be a character"
  )

  directory <- path %>%
    fs::path_sanitize() %>%
    normalizePath(
      winslash = "/",
      mustWork = FALSE
    )

  if (!dir.exists(directory)) {
    dir.create(directory)
  }
}


#' Append a timestamp to a filepath
#'
#' Given a filepath this function will append a timestamp to the filename. By
#' default the timestamp will be formatted as \code{\%Y_\%m_\%d_\%H\%M}.
#'
#' @param path (character) the path to the file as if it didn't have an appended
#'   timestamp
#' @param time (datetime, default = \code{Sys.time()}) the datetime that will be
#'   appended to \code{path}.
#' @inheritParams base::strftime
#'
#'
#' @return a filepath
#' @export
#'
#' @examples
#' \dontrun{
#' path <- append_time(
#'   path = "/data/raw.xlsx",
#'   time = ymd_hm("2021-05-20 14:30")
#' )
#'
#' # Returns:
#' # "/data/raw_2021_05_20_1430.xlsx"
#' }
append_time <- function(path,
                        time = Sys.time(),
                        format = "%Y_%m_%d_%H%M") {
  file <- basename(path)

  ext <- paste0(".", tools::file_ext(file))
  file_prefix <- stringr::str_remove(file, ext) # nolint

  fullname <- glue::glue("{file_prefix}_{format(time, format)}{ext}") %>%
    fs::path_sanitize()

  file.path(dirname(path), fullname) %>%
    normalizePath(
      mustWork = FALSE,
      winslash = "/"
    )
}
