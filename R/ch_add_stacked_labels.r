#' Add stacked labels to the right of a chart
#'
#' This function adds [ggrepel::geom_label_repel()] to a chart. It requires you
#' to specify the label aesthetic in the original ggplot. It also extends the
#' x-axis by a certain amount (20\% by default) so the labels have space. Most
#' of the arguments get passed directly to [ggrepel::geom_label_repel()]. This
#' expansion will combine with any other expansion present in the plot, such as
#' arguments based to [ggplot2::scale_x_continuous()]. It needs to be used with a
#' \code{\%>\%} and not a \code{+}.
#'
#' If you get a warning about Aesthetics not being the right length for size,
#' that's a cryptic message telling you to set the size argument. Setting it to
#' 3 usually looks alright.
#'
#' @param .plot (a \code{ggplot2} object) with the \code{label} aesthetic
#'   specified.
#' @param pct_extend (numeric, default = \code{1.2}) how much to expand the
#'   x-axis. The existing x-axis range will be multiplied by this value, so the
#'   default expands by 20%.
#' @param nudge_x (numeric; default = NULL) Horizontal and vertical adjustments
#' to nudge the starting position of each text label.
#' @param segment.colour (character; default = NA) colour of the line segment,
#'   NA means no line segment
#' @param segment.alpha (number; default = 0.5) the transparency of the line
#'   segment
#' @param segment.size (number; default = 0.1) the size of the line segment
#' @param hjust (default = "right") the horizontal justification of the label
#' @param size (number, default = 3) the size of the text in the label
#' @param ... other arguments passed to \code{geom_label_repel}
#' @inheritParams ggrepel::geom_label_repel
#'
#' @return a \code{ggplot2} object
#' @export
#'
#' @examples
#' \dontrun{
#' some_plot |> ch_add_stacked_labels()
#' }
#'
ch_add_stacked_labels <- function(
	.plot,
	pct_extend = 1.2,
	nudge_x = NULL,
	# nolint start
	segment.colour = NA,
	min.segment.length = 0.1,
	max.overlaps = 20,
	segment.alpha = 0.5,
	segment.size = 0.1,
	# nolint end
	hjust = "right",
	direction = "y",
	size = 3,
	...
) {
	rlang::check_installed("ggrepel")
	# Extract elements from the plot
	x <- .plot$mapping$x
	y <- .plot$mapping$y
	size_map <- .plot$mapping$size
	group <- .plot$mapping$group %||% .plot$mapping$colour
	col <- .plot$mapping$colour
	labs <- .plot$mapping$label

	# Get x range
	first_x <- .plot$data %>%
		dplyr::pull(!!x) %>%
		min(na.rm = TRUE)

	last_x <- .plot$data %>%
		dplyr::pull(!!x) %>%
		max(na.rm = TRUE)

	range_x <- last_x - first_x

	# Artificially push the labels to the right x%
	new_last <- first_x + (range_x * pct_extend)


	# Filter the internal plot data to the max x
	df_labels <- .plot$data %>%
		dplyr::select(
			!!group,
			!!col,
			!!size_map,
			!!x,
			!!y,
			!!labs
		) %>%
		dplyr::group_by(!!group) %>%
		dplyr::slice_max(!!x) %>%
		dplyr::ungroup() %>%
		dplyr::mutate(!!rlang::quo_name(x) := last_x)



	# Create a vector of nudge amounts to push all the values to the extended point
	# but this doesnt mean they will necessarily stay there
	nudge <- nudge_x %||% (new_last - dplyr::pull(df_labels, !!x))
	size_text <- size %||% size_map


	# add the labels
	.plot +
		ggrepel::geom_label_repel(
			data = df_labels,
			family = "Calibri",
			nudge_x = as.numeric(nudge),
			segment.colour = segment.colour,
			min.segment.length = min.segment.length,
			max.overlaps = max.overlaps,
			segment.alpha = segment.alpha,
			segment.size = segment.size,
			hjust = hjust,
			direction = direction,
			size = size_text,
			show.legend = FALSE,
			...
		) +
		# add a blank geom to expand
		ggplot2::geom_blank(ggplot2::aes(x = new_last))
}
