#' Sample complete groups from a grouped \code{tibble}
#'
#' This function lets you sample groups from a grouped tibble. The default
#' behaviour of \code{sample()} on a grouped tibble is to sample \strong{within}
#' each group, but sometimes you want to sample a few complete groups. This lets
#' you do that.
#'
#' Doesn't work with \code{sf}s because of the geometry column.
#'
#' @param grouped_df (grouped tibble) a grouped tibble from which you want to
#'   sample groups
#' @param n (number) how many groups do you want?
#' @param replace (boolean; default = FALSE) sample with replacement?
#'
#' @return a grouped tibble containing \code{n} groups from the original data
#' @export
#'
#' @examples
#' dplyr::storms %>%
#'   dplyr::group_by(name) %>%
#'   sample_groups(2)
#'
sample_groups <- function(grouped_df, n, replace = FALSE) {

  # Error checking
  # Is a grouped data frame
  assertthat::assert_that(
    dplyr::groups(grouped_df) %>%
        length() > 0L,
    msg = "grouped_df must be a grouped dataframe"
  )


  # Not an sf
  assertthat::assert_that(
    !inherits(grouped_df, "sf"),
    msg = "sample_groups doesn't work with sf objects, yet..."
  )

  # n is a number is done by the underlying function

  # Extract the grouping variable
  grp_var <- grouped_df %>%
    dplyr::groups() %>%
    unlist() %>%
    as.character()

  # Turn the grouping variable into a symbol for matching later
  gv <- rlang::sym(grp_var)

  # Collapse and sample the grouping col
  random_grp <- grouped_df %>%
    dplyr::summarise() %>%
    dplyr::slice_sample(n = n, replace = replace) %>%
    tibble::rowid_to_column(var = "unique_id")

  # Rejoin just by the sampled grouping col
  grouped_df %>%
    dplyr::right_join(random_grp, by = grp_var) %>%
    dplyr::group_by(!!gv) %>%
    dplyr::select(-.data[["unique_id"]])
}
