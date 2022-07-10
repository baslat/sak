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
  rlang::check_installed("renv")
  # Check if there is a project
  needs_to_start <- is.null(renv::project())

  if (needs_to_start) {
    # Initialise renv
    message("Initialising renv and tracking no packages...\nNext steps:\n-- R Studio will restart after this.\n-- Wait for renv to bootstrap (ie, after the restart wait a few seconds, then renv will print a few messages about bootstrapping)\n-- Run this function again (you might see a 'stop' icon after the bootstrap; if so try evaluating `1+1` in the console to see if R is responsive.\n")
    renv::init(
      settings = list(
        r.version = paste0(R.Version()[c("major","minor")], collapse = "."),
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
  rlang::check_installed("capsule")
  capsule::create(
    dep_source_paths = file.path("R", "_packages.R"),
    lockfile_path = "renv.lock"
  )
}
