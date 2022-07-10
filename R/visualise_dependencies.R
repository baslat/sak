#' Visualise function dependencies
#'
#' This function creates an interactive visualisation of function dependencies.
#' It can work on functions within a package, or functions loaded into your
#' global environment. Because the underlying functions are a bit weird, the
#' argument syntax isn't quite intuitive. The default behaviour will visualise
#' the function dependencies of what is in your global environment. If you want
#' to visualise the dependencies of a package, you need to pass the prefixed
#' name of a function from that package. Check the examples.
#'
#' @param .f (unquoted function name; default = \code{NULL}) an unquoted and
#'   prefixed function name
#'
#' @inheritDotParams foodwebr::foodweb
#'
#' @return a \code{visNetwork} object
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # To visualise the dependencies of your global environment:
#' sak::load_custom_functions() # You need the functions in the env
#' visualise_dependencies()
#'
#' # To visualise the dependencies of a package
#' # Note if you are in a package repo first run `devtools::load_all()`
#' # You need to pass the name of a function from the package (any function will
#' # do)
#' visualise_dependencies(.f = sak::open_path)
#' }
visualise_dependencies <- function(.f = NULL,
                                   ...) {
  rlang::check_installed("foodwebr")
  rlang::check_installed("tidygraph")
  rlang::check_installed("visNetwork")

  fw <- foodwebr::foodweb(
    FUN = .f,
    filter = FALSE,
    ...
  )

  tidy_fw <- tidygraph::as_tbl_graph(fw)

  # Styling the network
  main_col <- "lightblue"
  hover_col <- "darkblue"
  highlight_col <- "darkblue"

  # Displaying the network
  visNetwork::visIgraph(tidy_fw) %>%
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = TRUE,
        hover = TRUE
      ),
      nodesIdSelection = list(
        enabled = TRUE,
        main = "Select function"
      )
    ) %>%
    visNetwork::visEdges(
      color = list(
        color = main_col,
        hover = hover_col,
        highlight = highlight_col
      ),
      arrows = list(to = list(
        enabled = TRUE,
        scaleFactor = 1
      ))
    ) %>%
    visNetwork::visNodes(
      color = list(
        background = main_col,
        border = hover_col,
        hover = hover_col,
        highlight = highlight_col
      )
    )
}
