#' Check if packages in DESCRIPTION are actually used
#'
#' This is a helper function to identify if you're using packages you claim to
#' be. It reads all the packages listed in DESCRIPTION, and sees if they are
#' actually called anywhere in the R or \code{testthat} files. It refers to your
#' working directory, so it's only meant to be used when working on a package.
#'
#' @param deps_ignored (character vector; default = "R") a vector of characters to ignore
#'
#' @return a logical plus an informative message.
#' @export
#'
check_deps_used <- function(deps_ignored = "R") {
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
        )
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


#' Helper for check_deps
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
