#' Example cycling data
#'
#' @description Two example cycling datasets are included with the
#'   \code{elpatron} package. The first (\code{lufbra}) is a short and simple
#'   spin around Loughborough University's fine campus. This file has also been
#'   reformatted to .tcx and .gpx formats, both of which are included (along
#'   with the original .fit file) in this package's extdata directory. The
#'   second dataset is from a far more aggressive chaingang with two other
#'   fellow Loughborough cyclists.
#'
#' @section Creation:
#' These data were generated via the following: \preformatted{
#' f <- system.file("extdata/lufbra.fit", package = "elpatron")
#' lufbra <- f \%>\% import_ride \%>\% clean_bikedata
#'
#' f <- system.file("extdata/chaingang.fit", package = "elpatron")
#' chaingang <- f \%>\% import_ride \%>\% clean_bikedata
#' }
#'
#' @format Both data files have been formatted according to the \emph{clean}
#'   structure described in \code{\link{clean_bikedata}}.
#'
#' @name cyclingdata
NULL

#' @rdname cyclingdata
"lufbra"

#' @rdname cyclingdata
"chaingang"

