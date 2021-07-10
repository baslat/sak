#' Setup folders and .gitignore for projects and repos
#'
#' This function creates standard folders for analytical projects, and exempts
#' \code{/outputs/.} and temporary files from syncing with \code{git}.
#'
#' The folders created are:
#'
#' \itemize{
#'     \item \code{code} for your scripts
#'     \item \code{data} for small, local data
#'     \item \code{outputs} for your outputs, not synced with \code{git}
#'     \item \code{R} for you custom functions
#'     }
#'
#' Temporary files exempted from syncing are:
#' \itemize{
#'     \item History files
#'         \itemize{
#'             \item .Rhistory
#'             \item .Rapp.history}
#'     \item Session Data files
#'         \itemize{
#'             \item .RData}
#'     \item User-specific files
#'         \itemize{
#'             \item .Ruserdata}
#'     \item Example code in package build process
#'         \itemize{
#'             \item *-Ex.R}
#'     \item Output files from R CMD build
#'         \itemize{
#'             \item /*.tar.gz}
#'     \item Output files from R CMD check
#'         \itemize{
#'             \item /*.Rcheck/}
#'     \item RStudio files
#'         \itemize{
#'             \item .Rproj.user/}
#'     \item Produced vignettes
#'         \itemize{
#'             \item vignettes/*.html
#'             \item vignettes/*.pdf}
#'     \item OAuth2 token, see https://github.com/hadley/httr/releases/tag/v0.3
#'         \itemize{
#'             \item .httr-oauth}
#'     \item Knitr and R markdown default cache directories
#'         \itemize{
#'             \item *_cache/
#'             \item /cache/}
#'     \item Temporary files created by R markdown
#'         \itemize{
#'             \item *.utf8.md
#'             \item *.knit.md}
#'     \item R Environment Variables
#'         \itemize{
#'             \item .Renviron}
#'     \item pkgdown site
#'         \itemize{
#'             \item docs/}
#'     \item Output folder
#'         \itemize{
#'             \item outputs/}
#'     \item Translation temp files
#'         \itemize{
#'             \item po/*~}
#'     \item Leaflet file folders
#'         \itemize{
#'             \item *_files/}
#'     \item Office and web documents
#'         \itemize{
#'             \item *.docx}
#'         \itemize{
#'             \item *.html}
#'         \itemize{
#'             \item *.pdf}
#'         \itemize{
#'             \item *.csv}
#'         \itemize{
#'             \item *.xls}
#'         \itemize{
#'             \item *.xlsx}
#'         \itemize{
#'             \item *.ppt}
#'         \itemize{
#'             \item *.pptx}
#'     }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cah::setup_project() # This will make all your folders
#' }
setup_project <- function() {
  # Create folders

  c("code",
    "data",
    "outputs",
    "R") %>%
    purrr::walk(usethis::use_directory)

  # Create .gitignore
  lines <- c("# History files",
             ".Rhistory",
             ".Rapp.history",
             "# Session Data files",
             ".RData",
             "# User-specific files",
             ".Ruserdata",
             "# Example code in package build process",
             "*-Ex.R",
             "# Output files from R CMD build",
             "/*.tar.gz",
             "# Output files from R CMD check",
             "/*.Rcheck/",
             "# RStudio files",
             ".Rproj.user/",
             "# produced vignettes",
             "vignettes/*.html",
             "vignettes/*.pdf",
             "# OAuth2 token, see https://github.com/hadley/httr/releases/tag/v0.3",
             ".httr-oauth",
             "# knitr and R markdown default cache directories",
             "*_cache/",
             "/cache/",
             "# Temporary files created by R markdown",
             "*.utf8.md",
             "*.knit.md",
             "# R Environment Variables",
             ".Renviron",
             "# pkgdown site",
             "docs/",
             "# Output folder",
             "outputs/",
             "# translation temp files",
             "po/*~",
             "# Leaflet folders",
             "*_files/",
             "# Office and web documents",
             "*.docx",
             "*.html",
             "*.pdf",
             "*.csv",
             "*.xls",
             "*.xlsx",
             "*.ppt",
             "*.pptx")
  # Does a gitignore file exist?
  if (file.exists(".gitignore")) {
    # Read current
    current_contents <- readLines(".gitignore")
    discard_lines <- c(
      "# Shiny token, see https://shiny.rstudio.com/articles/shinyapps.html",
      "rsconnect/",
      "",
      "=======")

    lines <- setdiff(current_contents, lines) %>%
      setdiff(discard_lines) %>%
      c(lines)
  }


  # Write gitignore
  usethis::write_over(".gitignore",
                      lines)

}


#' Set up a new package
#'
#' A simple wrapper function for a suite of standard package set up steps.
#' Intended use is:
#' \enumerate{
#'   \item Create and clone a new repo. The name of the repo will be the name of
#'   the package, so you might like to run
#'   \code{available::available("potential_name")} to check if the package name
#'   already exists (you don't want conflicts) or if it's a swear.
#'   \item Open a new R project in the cloned folder. A normal project is fine,
#'   you don't need to select the \code{Package} option.
#'   \item Run \code{usethis::create_package(".")}, which will set up
#'   boilerplate folders and change the RStudio project file.
#'   \item Run \code{cah::setup_package()} which will adds things like package
#'   documentation, an MIT license, tidy styling, a news file, spell check, and
#'   support for tidy eval, pipes and tibbles.
#'   \item Fill in the standard details in the description and readme files,
#'   then develop as normal.
#' }
#'
#' @return Nothing returned, this is run for side effects (which include
#'   creating multiple files and folders).
#' @export
#'
#' @examples
#' \dontrun{
#' usethis::create_package(".")
#'
#' cah::setup_package()
#'
#' }
setup_package <- function() {
  usethis::use_package_doc()
  usethis::use_mit_license()
  usethis::use_news_md()
  usethis::use_testthat()
  usethis::use_spell_check()
  usethis::use_tidy_eval()
  usethis::use_pipe()
  usethis::use_tibble()
  usethis::use_tidy_description()
  usethis::use_tidy_style()
}