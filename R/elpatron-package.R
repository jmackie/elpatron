#' elpatron: improved cycling data analysis
#'
#' \code{elpatron} is a package intended to facilitate analysis of bicycling
#' data within the R environment.
#'
#' \code{elpatron} supercedes the \code{cycleRtools} package, if only for having
#' a cooler name. Generally speaking, this package attempts to do much less than
#' \code{cycleRtools}. But what it does, it does much better. File reading
#' functions are now much more efficient; raw data is left largely untouched by
#' default; and certain assumptions that had to be made for more \emph{involved}
#' routines are now left to the user to explicitly resolve. Generally speaking,
#' the best bits of cycleRtools have been skimmed and improved.
#'
#' This package came with the realisation that the advantage of using R is not
#' convenience \emph{per se}, but rather its attendant freedom and flexibility.
#' Hence, rather than provide menial functions for common analysis routines,
#' this package simply aims to be \emph{facilitative}.
#'
#' The package is also intended to work seamlessly with the \code{tidyverse}.
#'
#' @useDynLib elpatron
#' @importFrom Rcpp sourceCpp
#' @importFrom fitdc read_fit unpack
#' @importFrom dplyr tbl_df
#' @importFrom magrittr %>%
#' @importFrom purrr map map_lgl map_chr map_dbl map_df
#' @importFrom lazyeval lazy_dots
#'
#' @import stats
#' @import utils
#' @import graphics
#' @import grDevices
#'
#' @docType package
#' @name elpatron
NULL


.onUnload <- function(libpath) {  # Clean up.
  library.dynam.unload("elpatron", libpath)
}

# ------------------------------------------
# Notes to self:
#   + pugixml needs to be in header-only mode
