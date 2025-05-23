% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup.R
\name{setup_project}
\alias{setup_project}
\title{Setup folders and .gitignore for projects and repos}
\usage{
setup_project(default_branch = NULL, type = "full")
}
\arguments{
\item{default_branch}{(str, default = \code{NULL}) the default branch of the
repo, one of 'main' or 'master'. If \code{NULL} it will try to figure it
out.}

\item{type}{What kind of configuration to create?
\itemize{
\item \code{tidyverse} creates a minimal lintr config, based on the default linters (\code{\link[lintr:linters_with_defaults]{linters_with_defaults()}}).
These are suitable for following \href{https://style.tidyverse.org/}{the tidyverse style guide}.
\item \code{full} creates a lintr config using all available linters via \code{\link[lintr:all_linters]{all_linters()}}.
}}
}
\description{
This function:
\itemize{
\item{creates standard folders for analytical projects}
\item{exempts \code{/outputs/.}, and data & temp files from syncing with git}
\item{adds a markdown readme}
\item{asks if you want to use \code{{renv}}}
\item{asks if you want to use \code{{targets}}}
}
}
\details{
The folders created are:

\itemize{
\item \code{code} for your scripts
\item \code{data} for small, local data
\item \code{outputs} for your outputs, not synced with \code{git}
\item \code{R} for you custom functions
}

Temporary files exempted from syncing are:
\itemize{
\item History files
\itemize{
\item .Rhistory
\item .Rapp.history}
\item Session Data files
\itemize{
\item .RData}
\item User-specific files
\itemize{
\item .Ruserdata}
\item Example code in package build process
\itemize{
\item \emph{-Ex.R}
\item Output files from R CMD build
\itemize{
\item /}.tar.gz}
\item Output files from R CMD check
\itemize{
\item /\emph{.Rcheck/}
\item RStudio files
\itemize{
\item .Rproj.user/}
\item Produced vignettes
\itemize{
\item vignettes/}.html
\item vignettes/*.pdf}
\item OAuth2 token, see https://github.com/hadley/httr/releases/tag/v0.3
\itemize{
\item .httr-oauth}
\item Knitr and R markdown default cache directories
\itemize{
\item *_cache/
\item /cache/}
\item Temporary files created by R markdown
\itemize{
\item *.utf8.md
\item \emph{.knit.md}
\item R Environment Variables
\itemize{
\item .Renviron}
\item pkgdown site
\itemize{
\item docs/}
\item Output folder
\itemize{
\item outputs/}
\item Translation temp files
\itemize{
\item po/}~}
\item Leaflet file folders
\itemize{
\item *_files/}
\item Office and web documents
\itemize{
\item *.docx}
\itemize{
\item *.html}
\itemize{
\item *.pdf}
\itemize{
\item *.csv}
\itemize{
\item *.xls}
\itemize{
\item *.xlsx}
\itemize{
\item *.ppt}
\itemize{
\item *.pptx}
}
}
\examples{
\dontrun{
sak::setup_project(default_branch = "main") # This will make all your folders
}
}
