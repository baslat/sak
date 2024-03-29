% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stopwatch.r
\name{stopwatch}
\alias{stopwatch}
\title{Time an expression}
\usage{
stopwatch(expr)
}
\arguments{
\item{expr}{Valid \R expression to be timed.}
}
\value{
(\code{lubridate::period}) total elapsed time
}
\description{
This is a wrapper for \code{system.time}, but returns only the total elapsed time.
}
\examples{
\dontrun{

# This will return time_taken and a_result
time_taken <- stopwatch({
  a_result <- Sys.sleep(60)
})
}
}
