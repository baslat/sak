#' Show the command needed to install linux dependencies
#'
#' When installing R packages on linux, you often need some system dependencies
#' too. [RSPM](https://packagemanager.rstudio.com/client/#/repos/2/packages/sf)
#' provides a list, but you cannot just copy and paste it. So this function
#' reformats it so that you can copy and paste it.
#'
#' @param deps (default = `readClipboard()`) the commands needed to install the
#' dependencies copied from RSPM.
#'
#' @return a character string, on the clipboard
#'
#' @export
#'
show_linux_deps <- function(deps = clipr::read_clip()) {
    rlang::check_installed("clipr")
    packs <- deps |>
        stringr::str_remove(stringr::fixed("apt-get install -y ")) |>
        paste(collapse = " ")

    command <- paste("apt-get install -y", packs)
    clipr::write_clip(command)
    message("Now on the clipboard: ", command)
}
