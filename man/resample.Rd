% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resample.R
\name{resample}
\alias{resample}
\title{Resample data}
\usage{
resample(.data, time_column = "time.s", Hz = 1L, fill = 0L)
}
\arguments{
\item{.data}{data.frame; the data to be resampled on the basis of
\code{time_column}.}

\item{time_column}{string; identifier for the column in \code{data} that
gives (elapsed) time values. Right-hand-sided formulas will be deparsed,
otherwise this argument is passed to \code{'[['} as-is.}

\item{Hz}{numeric (scalar); the desired sampling frequency.}

\item{fill}{the value with which to "fill" new rows in the returned
data. For most purposes this should be either \code{NA} or a numeric
value (e.g. 0).}
}
\value{
a resampled \code{\link[dplyr]{tbl_df}}, with a  new \code{Hz}
  attribute added.
}
\description{
A useful function for working with data that aren't sampled at
  regular intervals and(or) contain gaps. This can present issues when, for
  example, trying to apply time-windowed rolling operations.
}
\details{
For time-windowed rolling operations, \code{fill} should logically
  be set to \code{0L}; the use of NAs could generate large runs of NAs.
}
\examples{
data <- data.frame(
  timer.s = 1:500,
  x = rnorm(500, 500, 30),
  y = rnorm(500, 1000, 300)
)
gappy_data <- data[sort(sample(1:500, 250)), ]
gappy_data \%>\% resample(~timer.s, Hz = 1, fill = NA) \%>\% head(20)
}

