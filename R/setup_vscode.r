#' Set up a VS Code workspace
#'
#' This function creates a VS Code workspace file tto track project-specific settings.
#'
#' @return nothing, called for side effects
#'
#' @export
#'
setup_vscode <- function() {
    repo <- basename(getwd())

    usethis::use_template(
        template = "vscode.json",
        save_as = paste0(repo, ".code-workspace"),
        package = "sak"
    )
}
