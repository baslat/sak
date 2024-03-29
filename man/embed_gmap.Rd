% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping.r
\name{embed_gmap}
\alias{embed_gmap}
\title{Get and embed a \code{ggmap} in a tibble}
\usage{
embed_gmap(left, bottom, right, top, zoom, maptype, ...)
}
\arguments{
\item{left}{(numeric) part of the bounding box}

\item{bottom}{(numeric) part of the bounding box}

\item{right}{(numeric) part of the bounding box}

\item{top}{(numeric) part of the bounding box}

\item{zoom}{(numeric) the zoom level}

\item{maptype}{(character) the map type}

\item{...}{other arguments passed to \code{ggmap::get_stamen_map}}
}
\value{
a \code{ggmap} object wrapped in a list
}
\description{
\code{ggmap} objects won't nicely go into a cell in a \code{tibble}. This
function gets a \code{ggmap} and then wraps it in a list, allowing it to go
into a \code{tibble}.
}
\examples{
\dontrun{
embedded_maps <- base_map_params \%>\%
	filter(geo_level == "state") \%>\%
	mutate(gmap = embed_gmap(left, bottom, right, top, zoom, maptype)) \%>\%
	ungroup()
}
}
