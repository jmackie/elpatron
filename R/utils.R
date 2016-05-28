#' Pipe operator
#'
#' See \code{\link[magrittr]{pipe}} and/or \code{\link[dplyr]{chain}}.
#'
#' @name %>%
#' @rdname pipe
#' @export
#' @usage lhs \%>\% rhs
#' @param lhs,rhs an R object and function to apply to it.
NULL


# Cleaner string concat.
`%+%` <- function(lhs, rhs) {
  paste0(lhs, rhs)
}

# Cleaner error throwing.
stop_ <- function(msg) {
  stop(msg, call. = FALSE)
}

warn_ <- function(msg) {
  warning(msg, call. = FALSE, immediate. = FALSE)
}

stringwrap <- function(...) {
  paste0(list(...), collapse = "\n")
}

# e.g. diff_index(1:50 < 21:71)
diff_index <- function(i) {
  cumsum(abs(Diff(i)))
}

is_timer <- function(x) {
  is.numeric(x) && identical(x, x[order(x)])
}

`%notin%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}

add_class <- function(x, .class, prepend = TRUE) {
  if (prepend) {
    class(x) <- c(.class, class(x))
  } else {
    class(x) <- c(class(x), .class)
  }
  x
}

VAM_calc <- function(time.s, elevation.m) {
  # Use central difference.
  if (all(is.na(elevation.m))) return(elevation.m) # Needed?
  VAM <- (tail(elevation.m, -2) - head(elevation.m, -2)) / diff(time.s, lag = 2)
  c(NA, VAM, NA)
}

is_formula <- function(x) {
  inherits(x, "formula")
}

rhs_only <- function(x) {
  inherits(x, "formula") && (length(x) == 2)
}

pkg_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

collapse <- function(...) {
  paste0(..., collapse = "")
}

zlen <- function(x) {
  length(x) == 0
}

inset <- `[<-`

na_rm <- function(x) {
  x[!is.na(x)]
}

make_time_col <- function(x, format = "%FT%T", tz = "") {
  # dplyr doesn't like POSIXlt.
  # Also see: http://www.w3schools.com/xml/schema_dtypes_date.asp
  as.POSIXct(strptime(x, format, tz))
}

posix_to_timer <- function(x) {
  as.numeric(x) - as.numeric(x)[1]
}

# See: https://github.com/kuperov/fit/blob/master/R/fit.R
semicircle_correct <- function(position) {
  (position * 180 / 2 ^ 31 + 180) %% 360 - 180
}

# EXPORTED -------------------------------------------------

#' Faster base::diff
#'
#' A more efficient implementation of base::diff(x, lag = 1, differences = 1).
#'
#' @param x a numeric vector containing the values to be differenced.
#'
#' @return A vector of \code{length(x)}, the first value being 0. This behaviour
#'   differs from \code{\link[base]{diff}}, where the length would be
#'   \code{length(x) - 1} (omitting the zero).
#'
#' @export
Diff <- function(x) {
  DIFF(as.numeric(x))
}

#' Cumulative sum, dropping NAs
#'
#' An Rcpp implementation of \code{\link[base]{cumsum}} that quietly discards
#' \code{NA}s. If an NA is encountered, the previous value is assumed. Also note
#' that the first value in \code{x} should not be \code{NA}.
#'
#' @param x a numeric vector containing the values to be cumulatively summed.
#'
#' @return A vector of \code{length(x)}.
#'
#' @export
Cumsum <- function(x) {
  CUMSUM(as.numeric(x))
}

# MISC -------------------------------------------------

# unusual <- function(x, criterion_freq = 5) {
#   seq_along(x) %in% which(table(y) < criterion_freq)
# }
