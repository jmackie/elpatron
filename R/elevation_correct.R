#' Elevation correction
#'
#' @description Extracts (reliable) elevation data from raster "alt" dataset,
#'   given positional coordinates (\code{lon,lat}). Requires the \code{raster}
#'   package to be installed. Essentially a wrapper for
#'   \code{\link[raster]{getData}} and \code{\link[raster]{extract}}.
#'
#' @details Note that raster data may need to be downloaded, and hence an
#'   internet connection will be required if these files do not already exist in
#'   the working directory.
#'
#'   Also note that elevation data are interpolated from the values of the four
#'   nearest raster cells. See the \code{method} argument in
#'   \code{\link[raster]{extract}}.
#'
#' @param lon,lat numeric vectors (of the same length) giving positional
#'   coordinates in units of (decimal) degrees.
#' @param country character; the three letter ISO code for the country in which
#'   the \code{lon,lat} data were collected. See \code{raster::getData("ISO3")}.
#'
#' @return A numeric vector of the same length as \code{lon,lat} of elevation
#'   values (i.e. metres above sea level).
#'
#' @examples \dontrun{
#' data(chaingang)
#'
#' # Show the original elevation profile...
#' plot(elevation.m ~ distance.km, type = "l", data = chaingang)
#'
#' chaingang <- transform(chaingang,
#'                        elev.new = elevation_correct(lon, lat, "GBR"))
#'
#' lines(elev.new ~ distance.km, col = "red", data = chaingang)
#' }
#' @export
elevation_correct <- function(lon, lat, country) {
  stopifnot(requireNamespace("raster", quietly = TRUE),
            length(lon) == length(lat))
  # NB: Won't download twice.
  to_extract <- raster::getData(name = "alt", country = country)
  raster::extract(to_extract, cbind(lon, lat), method = "bilinear")
}
