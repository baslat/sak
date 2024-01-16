#' Extract elements from a list by name
#'
#' This function takes a list and a name, and recursively returns all elements
#' in the list that have a name matching the provided name. The matching is done
#' using the `grepl` function with `fixed = TRUE`, which means that the name is
#' matched exactly, without any regular expressions.
#'
#' @param x A list from which to extract elements.
#' @param name The name of the elements to extract.
#'
#' @return A vector of elements from the list that have a name matching the provided name.
#'
#' @details The function first unlists the input list using the `unlist`
#' function. It then uses the `grepl` function to find all elements in the list
#' that have a name matching the provided name. The `fixed = TRUE` argument to
#' `grepl` means that the name is matched exactly, without any regular
#' expressions. The matching elements are then returned as a vector.
#'
#' @examples
#' list_data <- list(a = 1, b = 2, c = 3, d = 4)
#' extract_by_name(list_data, "a") # returns 1
#'
#' @export
extract_by_name <- function(x, name) {
	unlist(x)[grepl(
		name,
		names(unlist(x)),
		fixed = TRUE
	)]
}
