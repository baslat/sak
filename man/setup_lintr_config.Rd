% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_lintr.r
\name{setup_lintr_config}
\alias{setup_lintr_config}
\title{Setup the \code{lintr} config file}
\usage{
setup_lintr_config(...)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[lintr:use_lintr]{lintr::use_lintr}}
  \describe{
    \item{\code{path}}{Path to project root, where a \code{.lintr} file should be created.
If the \code{.lintr} file already exists, an error will be thrown.}
    \item{\code{type}}{What kind of configuration to create?
\itemize{
\item \code{tidyverse} creates a minimal lintr config, based on the default linters (\code{\link[lintr:linters_with_defaults]{linters_with_defaults()}}).
These are suitable for following \href{https://style.tidyverse.org/}{the tidyverse style guide}.
\item \code{full} creates a lintr config using all available linters via \code{\link[lintr:all_linters]{all_linters()}}.
}}
  }}
}
\value{
Nothing, called for side effects.
}
\description{
A simple function to create the config file needed for \code{lintr}. Most
linters are enable by default, except for \code{line_length_linter} and
\code{trailing_whitespace_linter}.
}
