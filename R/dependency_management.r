#' Check if packages in DESCRIPTION are actually used
#'
#' This is a helper function to identify if you're using packages you claim to
#' be. It reads all the packages listed in DESCRIPTION, and sees if they are
#' actually called anywhere in the R or \code{testthat} files. It refers to your
#' working directory, so it's only meant to be used when working on a package.
#'
#' @param deps_ignored (character; default = `c("R", "roxygen2")` a vector of
#' characters to ignore
#'
#' @return a logical plus an informative message.
#' @export
#'
check_deps_used <- function(deps_ignored = c("R", "roxygen2")) {
  pwd <- getwd()

  # Is it being run in the testing zone?
  # logic for devtools:check
  if (stringr::str_detect(string = pwd, pattern = "check")) {
    pwd <- stringr::str_remove(
      string = pwd,
      pattern = "\\..*"
    ) # dot followed by anything
  }
  # logic for devtools:test
  if (stringr::str_detect(string = pwd, pattern = "testthat")) {
    pwd <- stringr::str_remove(
      string = pwd,
      pattern = "/tests.*"
    ) # dot followed by anything
  }


  # Read the description file and get the listed dependencies
  packs <- desc::desc_get_deps(pwd) %>%
    dplyr::filter(!(.data$package %in% deps_ignored)) %>%
    dplyr::pull(.data$package) %>%
    rlang::set_names()

  # Read all the .R files
  lines <- list.files(
    path = pwd,
    pattern = ".R$",
    recursive = TRUE,
    full.names = TRUE
  ) %>%
    purrr::map(readLines)



  # Check every file in /R and see if the packages are called
  res <- packs %>%
    purrr::map_lgl(check_files,
      lines = lines
    ) %>%
    purrr::discard(function(x) x == TRUE)

  # If no results, all packages are used
  if (length(res) == 0) {
    message("Success! All the packages in DESCRIPTION appear to be used.")
    return(invisible(TRUE))
  }

  crap_packs <- names(res) %>% # nolint
    paste(collapse = "\n")

  pack_name <- desc::desc_get_field("Package", # nolint
    file = pwd
  )

  warning(glue::glue("Uh-oh! There are {length(res)} unnecessary packages in the DESCRIPTION of `{pack_name}`:\n{crap_packs}"),
    call. = FALSE
  )
  return(invisible(FALSE))
}


#' Helper for `check_deps_used`
#'
#' Unexported helper for inside \code{\link{check_deps_used}}.
#'
#' @param term character, the word to search for
#' @param lines character vector, the lines read from the R files that are
#'   search for \code{term}
#'
#' @return a logical
#'
check_files <- function(term,
                        lines) {
  term_colon <- paste0(term, "::") # nolint
  term_roxygen <- paste("importFrom", term) # nolint


  # Search for the syntax of `package::`
  colon_syntax <- any(
    purrr::map_lgl(
      lines,
      ~ any(
        stringr::str_detect(.,
          pattern = term_colon
        )
      )
    )
  )

  # Search for the syntax of `importFrom package`
  roxygen_syntax <- any(
    purrr::map_lgl(
      lines,
      ~ any(
        stringr::str_detect(.,
          pattern = term_roxygen
        )
      )
    )
  )

  any(colon_syntax, roxygen_syntax)
}

#' Setup `renv` to track a project
#'
#' This function will initialise `renv` in a project, both with specified
#' packages and identified dependencies. If run on an untracked project, it will
#' first initialise `renv` and restart R Studio. If run on a tracked project, it
#' will install the identified packages into the project library, and update the
#' lockfile. You can also use this function as a convenience wrapper to get
#' `renv` to track a new package you want to add to the project.
#'
#' @param starter_packs character (default = `c("sak", "renv")`) a
#'   character vector of extra packages to install. `renv` will try to identify
#'   packages used in the project and install these too. You can specify the
#'   version of a package by using the following notation: `pins@0.4.5`.
#' @param search_for_deps (logical; default = `TRUE`) do you want `renv` to look
#'   through the project and try to identify packages to track?
#'
#' @return nothing, called for side effects
#' @export
#'
#' @examples
#' \dontrun{
#' # Start tracking a new project
#' setup_renv()
#' # renv installs and then restarts, and you need to run it again
#' # but lets track a specific version of `pins` too
#' setup_renv(starter_packs = c("sak", "rsconnect", "renv", "pins@0.4.5"))
#'
#' # some time later, you realise you want to renv to track `zoo`
#' # this function can get renv to do that too
#' setup_renv("zoo")
#' }
setup_renv <- function(starter_packs = c("sak", "renv"),
                       search_for_deps = TRUE) {
  function_needs("renv")
  # Check if there is a project
  needs_to_start <- is.null(renv::project())

  if (needs_to_start) {
    # Initialise renv
    message("Initialising renv and tracking no packages...\nNext steps:\n-- R Studio will restart after this.\n-- Wait for renv to bootstrap (ie, after the restart wait a few seconds, then renv will print a few messages about bootstrapping)\n-- Run this function again (you might see a 'stop' icon after the bootstrap; if so try evaluating `1+1` in the console to see if R is responsive.\n")
    renv::init(
      settings = list(
        r.version = "4.1.0",
        use.cache = TRUE
      ),
      bare = TRUE
    )
  }

  # Find the packs used in the repo
  deps <- c() # nolint
  if (search_for_deps) {
    deps <- sort(unique(renv::dependencies()$Package))
  }

  # Remove any deps that are in starter_packs with a specified version
  spec_vers <- starter_packs %>%
    purrr::keep(stringr::str_detect,
      pattern = "@"
    ) %>%
    stringr::str_remove("@.*") # @ followed by anything

  if (!rlang::is_empty(spec_vers)) {
    deps <- deps %>%
      purrr::discard(stringr::str_detect,
        pattern = spec_vers
      )
  }

  # Add them to the starter packs
  packs <- sort(unique(c(starter_packs, deps)))

  # Show the user,
  packs_nice <- paste0(packs, collapse = ", ")
  message("Installing packages:\n", packs_nice)
  proceed <- ask_to_proceed()

  if (!proceed) {
    message("Exiting!")
    return(NULL)
  }

  # load them into renv
  renv::install(packs)
  message("Snapshotting...")
  renv::snapshot(prompt = FALSE)
  message("Next steps:\n-- Restart your session\n-- Run sak::renv_restore_rspm()\n-- Restart again\n-- Run renv::status()! And ignore any yellow error bars at the top of the screen about packages not being installed!")
}


#' Track dependencies with `capsule`
#'
#' A lightweight way to track dependencies instead of full-blown `renv`.
#'
#'
#'@export
#'
setup_capsule <- function() {
  function_needs("capsule")
  capsule::create(
    dep_source_paths = "R/_packages.R",
    lockfile_path = "renv.lock"
  )
}
