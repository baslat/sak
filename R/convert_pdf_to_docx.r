#' Convert a pdf to docx
#'
#' Use powershell to open a pdf in word, and save it as docx. It is saved in the
#' same location with the same file name. Requires MS Word to be installed. Note
#' that this will open a hidden dialogue box requiring you to confirm that you
#' want to open Word (at least the first time you run it).
#'
#' @param pdf (character) a pdf to convert
#' @param verbose (logical; default = `TRUE`) print status messages?
#'
#' @return nothing, returned for side effects
#' @export
#' @examples
#' \dontrun{
#' # Convert a single file
#' convert_pdf_to_docx("C:/files/pdfs/norm.pdf")
#'
#' # Convert a folder of files
#' list.files("C:/files/pdfs", full.names = TRUE) %>%
#'   purrr::walk(convert_pdf_to_docx)
#' }
convert_pdf_to_docx <- function(pdf,
                                verbose = TRUE) {
  # Need to do this to prevent unwanted escapes
  pdf <- normalizePath(pdf,
    winslash = "/",
    mustWork = FALSE
  )

  assertthat::assert_that(file.exists(pdf),
    msg = "Are you sure this file exists?"
  )


  dir <- dirname(pdf)
  filename <- basename(pdf)

  # Check it's a pdf
  pdf_ext <- tools::file_ext(pdf)
  if (pdf_ext != "pdf") {
    glue::glue("It looks like this ({filename}) is not a pdf, exiting!") %>%
      message()
    return(invisible(NULL))
  }

  # Check it's not already converted
  if (file.exists(paste0(pdf, ".docx"))) {
    glue::glue("It looks like this ({filename}) has already been converted to docx, exiting!") %>% message()
    return(invisible(NULL))
  }



  # Read the powershell script, and swap in the file name components
  ps <- system.file("pdf_to_docx.ps1", package = "sak") %>%
    readLines() %>%
    purrr::map_chr(stringr::str_replace_all,
      pattern = "REPLACE_ME_DIR",
      replacement = dir
    ) %>%
    purrr::map_chr(stringr::str_replace_all,
      pattern = "REPLACE_ME_FILENAME",
      replacement = filename
    )

  if (verbose) {
    glue::glue("Converting '{pdf}' to .docx (this will take a few seconds)...
               (if it hangs check for hidden dialogue boxes)") %>%
      message()
  }

  # I tried system2 and processx, but none of them worked!
  # Add the -NoProfile flag to prevent the user profile of powershell from running
  system("powershell.exe -NoProfile",
    ignore.stdout = TRUE,
    ignore.stderr = TRUE,
    input = ps
  )

  return(invisible(NULL))
}
