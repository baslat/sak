
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
#'   \item Run \code{sak::setup_package()} which will adds things like package
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
#' sak::setup_package()
#' }
setup_package <- function() {
  if (!is_package()) {
    create_pack <- ask_to_proceed(msg = "This project isn't a package. Do you want to create a package?")
    if (create_pack) {
      usethis::create_tidy_package(".")
    }
  }

  usethis::use_package_doc()
  usethis::use_mit_license()
  usethis::use_news_md()
  usethis::use_testthat()
  usethis::use_spell_check()
  usethis::use_tidy_eval()
  usethis::use_pipe()
  usethis::use_tibble()
  usethis::use_tidy_description()
  usethis::use_roxygen_md()
  usethis::use_lifecycle()
  usethis::use_readme_md()
  setup_lintr_config()
  setup_lintr_testthat()
  usethis::use_tidy_style()
}

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
#' @inheritParams setup_yaml_megalinter
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sak::setup_project(default_branch = "main") # This will make all your folders
#' }
setup_project <- function(default_branch = NULL) {

  default_branch <- default_branch %||% usethis::git_default_branch()
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
    usethis::write_over(
      ".gitignore",
      lines
    )
  # Use a markdown readme
  usethis::use_readme_md()
  # setup lintr config
  setup_lintr_config()

use_renv <- ask_to_proceed("Do you want to track this project with renv?")
if (use_renv) {
  setup_renv()
}

use_capsule <- ask_to_proceed("Do you want to track this project with capsule?")
if (use_capsule) {
  setup_capsule()
}

use_targets <- ask_to_proceed("Do you want to use `{targets}`?")
if (use_targets) {
  # inspired by `tflow::use_tflow()`
  usethis::use_directory("R")
  usethis::use_template(
    template = "_packages.R",
    save_as = file.path("R", "_packages.R"),
    package = "sak"
  )
  usethis::use_template("_targets.R", package = "sak")
  usethis::use_template(".env", package = "sak")
}
    # Setup yamls
# TODO deal with these yaml files
# setup_yaml_megalinter(default_branch = default_branch)
# setup_git_hooks(repo_type = "project")
# setup_yaml_azdo(repo_type = "project")

}

#' Setup proxy and keyring for RStudio Connect
#'
#' `r lifecycle::badge("deprecated")`
#' RStudio Connect needs to connect to the internet via a proxy, but our dev
#' machines won't work if they try to use the same proxy. Run this script at the
#' start of any RMD or Shiny app that requires internet connectivity that you
#' want to publish on RStudio Connect.
#'
#' We need a range of passwords and API keys available and don't want to manage
#' them individually nor hardcode. Keyring files are copied to a read-write
#' location \code{tempdir()} and then unlocked using
#' \code{Sys.getenv("KEYRING_UNLOCK")}. The unlock password is added to the VARS
#' section in RSCONNECT.
#'
#' This is necessary as behind the scenes packrat clones required packages into
#' a read-only environment. Unlocking a keyring requires read-write privileges.
#'
#' This function also sets the timezone to \code{Australia/Sydney} to silence a
#' warning that can occur when calling \code{library(tidyverse)}, so the
#' suggested use is to call \code{sak::setup_rsc()} before any library calls.
#'
#' @return \code{NULL}, but changes some proxy, timezone and keyring settings on
#'   RStudio Connect.
#' @export
#'
#' @examples
#' \dontrun{
#' # At the start of an RMD file, before library calls:
#' ```{r setup, include=FALSE}
#' sak::setup_rsc()
#' ```
#'
#' }
setup_rsc <- function() {
  lifecycle::deprecate_warn("2.0.0",
                            "setup_rsc()",
                            details = "setup_rsc has been replaced with setup_env which is called on loading sak, so setup_rsc is no longer needed.")
  if (get_sak_env() == "rsc") {
    # Timezone settings
    Sys.setenv(TZ = "Australia/Sydney")
    # proxy settings
    Sys.setenv(http_proxy = "http://uint-proxy.internal.niaa.gov.au:8080/")
    Sys.setenv(https_proxy = "http://uint-proxy.internal.niaa.gov.au:8080/")

  }
}

#' Create Lintr test that file
#'
#' This function will take the file test-linted.R from the system files of the package
#' and copy it into the test/testthat directory of your package. This will allow lintr
#' tests to be executed when you run check on your package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sak::setup_lintr_testthat()
#' }
setup_lintr_testthat <- function() {

  # Look at the system file and read the lines of the test-linted.R file
  lintr_file <- system.file("lintr/test-linted.R", package = "sak")

  # Get the name of the Package we are working in from Description File
  package_name <- desc_get(keys = c("Package")) %>%
    as.character()

  # Replace the package name in the lintr test-that file.
  lintr_file_lines <- readLines(lintr_file) %>%
    stringr::str_replace(
      pattern = "place_package_name_here_please",
      replacement = package_name
    )

  usethis::write_over(
    path = "tests/testthat/test-linted.R",
    lines = lintr_file_lines
  )
}


#' Setup the \code{lintr} config file
#'
#' A simple function to create the config file needed for \code{lintr}. Most
#' linters are enable by default, except for \code{line_length_linter} and
#' \code{trailing_whitespace_linter}.
#'
#' @return Nothing, called for side effects.
#' @export
setup_lintr_config <- function() {
  lintr_config <- readLines(system.file("lintr/lintr_config.txt", package = "sak"))
  usethis::write_over(
    path = ".lintr",
    lines = lintr_config
  )
  invisible(NULL)
}


#' Setup a yaml file for megalinter
#'
#' Megalinter is a pipeline program that can lint multiple languages. This
#' function creates a yaml file in the project working directory that configures
#' megalinter to run no AZDO pipelines.
#'
#' @param default_branch (str, default = \code{NULL}) the default branch of the
#'   repo, one of 'main' or 'master'. If \code{NULL} it will try to figure it
#'   out.
#'
#' @return nothing, called for side effects
#' @export
#'
setup_yaml_megalinter <- function(default_branch = NULL) {
  default_branch <- default_branch %||% get_git_default_branch()

  assertthat::assert_that(default_branch %in% c(
    "master",
    "main"
  ),
  msg = "`default_branch` should be 'main' or 'master'"
  )

  # Get the guts of the mega-linter file from inside `sak`
  # and replace the default_repo
  # and write the file
  ml_yaml <- system.file("yaml_files/mega-linter.yml", package = "sak") %>%
    readLines() %>%
    stringr::str_replace(
      pattern = "MAIN_REPO",
      replacement = default_branch
    )

  usethis::write_over(
    path = ".mega-linter.yml",
    lines = ml_yaml
  )
}

#' Create yaml file for AZDO pipeline
#'
#' The function creates the appropriate \code{yaml} file to run scripts (such as
#' package checks or megalinter) inside the \code{sakntainer} on an AZDO
#' pipeline. It does not create the pipeline in AZDO though:you can do this via
#' the AZDO GUI or read the CLI instructions here:
#' \url{https://dev.azure.com/dpmc/PMC/_wiki/wikis/PMC.wiki/60/Setting-up-a-pipeline}
#'
#' @param repo_type (character) What sort of repo is it? Should be one of
#'   \code{"project"} or \code{"package"}. This will affect things like the
#'   types of git hooks and the pipeline yaml template.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Note you will also need to create a pipeline on AZDO, otherwise the yaml
#' # wont trigger anything
#'
#' # For package checks
#' sak::setup_yaml_azdo("package")
#'
#' # For project checks
#' sak::setup_yaml_azdo("project")
#' }
setup_yaml_azdo <- function(repo_type) {

  assertthat::assert_that(repo_type %in% c("project",
                                               "package"),
                          msg = "`repo_type` must be one of 'project' or 'package'.")

  # Select which azdo yaml template to use
  yaml_root <- dplyr::case_when(
    repo_type == "project" ~ "azure-pipelines_project.yml",
    repo_type == "package" ~ "azure-pipelines_package.yml"
  )


  #Look at the system file and read the lines of the yaml file
  yaml_file <- system.file("yaml_files", yaml_root, package = "sak")
  pipeline_file_lines <- readLines(yaml_file)

  written <- usethis::write_over(path = "azure-pipelines.yml",
                                 lines = pipeline_file_lines)

  if (written) {
    # Success message
    glue::glue("azure-pipelines.yml created! However, you also need to create a pipeline on AZDO to run the yaml. Do this with: setup_azdo_pipeline()\n") %>%
      stringr::str_wrap(120) %>%
      message()
  }
}


#' Use Azure CLI to create a pipeline to run the project yaml
#'
#' Creating a yaml file isn't enough to run a CI pipeline; you also need to
#' create the pipeline which can run the yaml. This can be done via the AZDO
#' GUI, however it's point and clicky, so the CLI is thus more programmable.
#' This function calls a bunch of \code{az} functions to create a AZDO pipeline
#' and add a build policy (ie requires tests to pass, requires a code review)
#' before a PR can complete. **It must be run after creating a
#' `azure-pipelines.yml` file in the repo root directory.**
#'
#' @return nothing, called for side effects
#' @export
#'
setup_azdo_pipeline <- function() {

  # Use ifs and invisible NULLS instead of asserts so that the meta setup_ func can continue
  if (!file.exists("azure-pipelines.yml")) {
    message("There is no azure-pipelines.yml file in this repo. Run sak::setup_yaml_azdo() to create one, then try this again. Exiting!")
    return(invisible(NULL))
  }

  # Check for az on path
  az_on_path <- on_path("az", verbose = FALSE)

  if (!az_on_path) {
    message("I cannot find az on your path. Check the wiki on how to set up a new VM, and try again. Exiting!")
    return(invisible(NULL))
  }


  ## Auth ----
  # Are you logged in?
  logged_in <- suppressWarnings(system2("az",
                                        args = "account show",
                                        stdout = TRUE,
                                        stderr = TRUE)) %>%
    stringr::str_detect(pattern = "ERROR: Please run 'az login' to setup account.",
                        negate = TRUE) %>%
    any()


  if (!logged_in) {
    message("You are not logged into Azure CLI. Open a terminal, type `az login`, do what it says, and then come back and run this again. Exiting!")
    return(invisible(NULL))
  }


  # Do you have the right extension?
  got_extension <- !(system2("az",
          args = "extension list",
          stdout = TRUE) %>%
    stringr::str_which("azure-devops") %>%
    rlang::is_empty())


  if (!got_extension) {
    message("You need to azure-devops extension for AZ CLI. Open a terminal, type `az extension add --name azure-devops` and then come back and run this again. Exiting!")
    return(invisible(NULL))
  }

  ## Set initial args for az cli ----
  org_url <- "https://dev.azure.com/dpmc"
  project <- "PMC"
  repo <- basename(system2("git",
                           args = "config --get remote.origin.url",
                           stdout = TRUE))
  display_name <- paste0('"PR policy pipeline: "',  repo, '"')
  approversId <- "[PMC]\\Bisons"
  branch <- get_git_default_branch()

  ## Check if there is already a pipeline ----
  args_check_pipe <- glue::glue("pipelines list --repository {repo}")
  already_pipe <- !(system2("az",
                            args = args_check_pipe,
                            stdout = TRUE,
                            stderr = NULL) %>%
                      paste0(collapse = "\n") == "[]")


  if (already_pipe) {
    message("It looks like there is already a pipeline for this repo. Exiting!")
    return(invisible(NULL))
  }


  # Dynamic argument setting
  args_repo_id <- glue::glue("repos show --org {org_url} -p {project} --repository {repo} --query id -o tsv")

  repo_id <- system2("az",
                     args = args_repo_id,
                     stdout = TRUE)

  ## Check if there is already a build policy ----

  already_policy <- !(system2("az",
                              args = "repos policy list",
                              stdout = TRUE) %>%
                        stringr::str_which(repo_id) %>%
                        rlang::is_empty())

  if (already_policy) {
    message("It looks like there is already a build policy for this repo's pipeline. Exiting!")
    return(invisible(NULL))
  }


  ## Create the pipeline ----

  ### Create a pipeline ----
  # in the repo,
  # on the default branch
  # and point to the existing yaml file
  message("Creating the pipeline...")
  args_pipeline_create <- glue::glue("pipelines create --name {repo} --org {org_url} -p {project} --repository {repo} --repository-type tfsgit  --yaml-path azure-pipelines.yml")
  system2("az",
          args = args_pipeline_create)

  # Success, and it has started running!

  ### Set a build policy on that pipeline ----
  message("Establishing the build policies...")
  args_build_id <- glue::glue('pipelines build definition list --org {org_url} -p {project} --repository {repo} --query "[].id" -o tsv')

  build_id <- system2("az",
                      args = args_build_id,
                      stdout = TRUE)


  args_build_policy <- glue::glue("repos policy build create --org {org_url} -p {project} --branch {branch} --repository-id {repo_id} --blocking true --enabled true --build-definition-id {build_id} --manual-queue-only false --queue-on-source-update-only false --valid-duration 0 --display-name {display_name}")

  system2("az",
          args = args_build_policy,
          stdout = NULL)

  check_success <- !(system2("az",
                             args = "repos policy list",
                             stdout = TRUE) %>%
                       stringr::str_which(repo_id) %>%
                       rlang::is_empty())


  assertthat::see_if(!check_success,
                     msg = "Hmmm... everything ran but I cannot find a build policy on a pipeline for this repo. Something has gone awry! Exiting!")

  message("All done!")
}


#' Setup \code{renv}
#'
#' This function prints instructions and suggestions on how to use \code{renv}
#' to track dependencies in a project. This only prints instructions, because
#' \code{renv} functions are fairly simple to use, but their use is highly
#' context dependent, so instructions made the most sense.
#'
#' @return nothing, just prints a message
#' @export
#'
setup_renv <- function() {
  # Just prints advice on how to use renv

  # Has renv been initialised?
  renv_started <- file.exists("renv.lock") # nolint

  # Specify messages for each case
  no_renv_msg <- "\nI suggest tracking package dependencies with `renv`. To start doing so, run the following and follow the prompts:\n
renv::init(settings = list(r.version = '4.1.0', use.cache = TRUE))"

  already_renv_msg <- "`renv` is already tracking dependencies in this repo. Some common functions are:
renv::status():   see if you packages match the lockfile
renv::install():  install a new package into your renv library (used before calling library on it)
renv::update():   pass a package name to update the lockfile to use a more recent version of that package
renv::snapshot(): overwrite the lockfile with all your installed packages (often used after requiring new packages in a project)
renv::restore():  change your installed packages to match the lockfile (often used to restore your setup to how an old project needed it)
renv::init():     get prompted to do some of the above"

  # Pick the appropriate message and print it
  msg <- dplyr::case_when(
    renv_started == TRUE ~ already_renv_msg,
    renv_started == FALSE ~ no_renv_msg,
    TRUE ~ "I don't know what's going on in this repo..."
  )

  message(msg)
}


#' Setup some git hooks
#'
#' Git hooks are scripts that run before certain git commands. They are repo
#' specific and they are not synced via the repo (meaning each contributor will
#' always have different git hooks). This function sets up some useful hooks for
#' package development or publishing to RSC. You can skip the hooks by running
#' the git command from the CLI and passing \code{--no-verify}, e.g. \code{git
#' commit --no-verify}.
#'
#' For all repo types it will add a pre-commit hook to run the
#' \href{https://github.com/dlenski/wtf}{\code{wtf}} python script. If
#' \code{repo_type == "project"} then this function will also add a pre-push
#' hook to write the manifest for RSC (you are prompted to confirm this as not
#' all projects are published to RSC), and if \code{repo_type == "package"} then
#' the pre-push hook is to tidy the \code{DESCRIPTION} and build the roxygen
#' documentation.
#'
#' @inheritParams setup_yaml_azdo
#'
#' @return nothing, called for side effects
#' @export
#'
setup_git_hooks <- function(repo_type) {

  assertthat::assert_that(repo_type %in% c("project",
                                           "package"),
                          msg = "`repo_type` must be one of 'project' or 'package'.")

  # Pre-commit hook on all repos
  message("Creating a pre-commit hook to treat whitespace...")
  pre_commit_hook <- readLines(
    system.file("git_hooks", "pre-commit_wtf",
                package = "sak")
    )

  usethis::use_git_hook(hook = "pre-commit",
                        script = pre_commit_hook)
  message("Done!")


  # Is Rscript on path?
  rscript_on_path <- on_path("Rscript", verbose = FALSE)

  if (!rscript_on_path) {
    message("I can't find Rscript on your path (sort of). You can't run the pre-push hooks. Exiting!")
    return(invisible(NULL))
  }


  # Early exit if not an RSC project
  if (repo_type == "project") {
    rsc_publish <- utils::menu(
      c("Yes", "No"),
      title = "Is this repo going to be published on RSC?"
      )
    if (rsc_publish == 2) { # 2 is no
      message("No need for a pre-push hook, exiting.")
      return(invisible(NULL))
    }
  }

  # nolint start
  # Switch depending on repo type
  # If package:
  # devtools::document
  # usethis::use_tidy_description

  # If project:
  # rsconnect::writeManifest()
  # nolint end
  pre_push_root <- dplyr::case_when(
    repo_type == "project" ~ "pre-push_project",
    repo_type == "package" ~ "pre-push_package"
  )

  pre_push_msg <- dplyr::case_when(
    repo_type == "project" ~ "Creating a pre-push hook to write an RSC manifest...",
    repo_type == "package" ~ "Creating a pre-push hook to build documentation..."
  )

  pre_push_hook <- readLines(
    system.file("git_hooks", pre_push_root,
                package = "sak")
    )

  message(pre_push_msg)
  usethis::use_git_hook(hook = "pre-push",
                        script = pre_push_hook)
  message("Done!")

}



#' Get the default git branch name
#'
#' Not exported. A handy wrapper to get the default git branch name. Used
#' internally a lot.
#'
#' @return character, the default remote git branch
get_git_default_branch <- function() {

  # I cannot pass the piped bits through the `system2` command, so moving to
  # part git and part stringr
  # full = "symbolic-ref refs/remotes/origin/HEAD | cut -f4 -d/" # nolint
  args = "symbolic-ref refs/remotes/origin/HEAD"

  system2("git", args = args, stdout = TRUE) %>%
    stringr::str_extract(pattern = "(?<=/)\\w+$")

}
