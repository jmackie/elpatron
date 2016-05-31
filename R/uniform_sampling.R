#' Make rows uniform
#'
#' @description A useful function for working with data that aren't sampled at
#'   regular intervals and(or) contain gaps. This can present issues when, for
#'   example, trying to apply time-windowed rolling operations.
#'
#' @details For time-windowed rolling operations, empty_fill should logically be
#'   set to \code{0L}; the use of NAs could generate large runs of NAs, causing
#'   complications.
#'
#' @param .data data.frame; the data to be made uniform on the basis of
#'   \code{time_column}.
#' @param time_column identifier for the column in \code{data} that gives
#'   (cumulative) time values. Right-hand-sided formulas will be deparsed,
#'   otherwise this argument is passed to \code{'[['} as-is.
#' @param deltat numeric (scalar); the \emph{typical} sampling frequency of the
#'   data. If \code{NULL} (default), a guess is made via
#'   \code{\link{deltat_guess}}.
#' @param empty_fill the value with which to "fill" new rows in the returned
#'   data. For most purposes this should be either \code{NA} or a numeric value.
#'
#' @examples
#' gappy_data <- data.frame(
#'   timer.s = c(1:100, 201:300),
#'   x = rnorm(200, 500, 30),
#'   y = rnorm(200, 1000, 300)
#' )
#' unif <- uniform_sampling(gappy_data, ~timer.s, empty_fill = NA)
#' head(unif)
#' @export
uniform_sampling <- function(.data, time_column, deltat = NULL, empty_fill = 0L) {

  # To be restored later...
  ocolnames <- colnames(.data)
  oattr <- attributes(.data)

  if (is_formula(time_column)) {  # NSE.
    time_column <- deparse(time_column[[2]])
  }
  if (!is_timer(.data[[time_column]])) {  # Check.
    stop_("Time column in data is invalid.")
  }
  if (is.null(deltat)) {
    deltat <- deltat_guess(.data[[time_column]])
  }

  join <- c(range(.data[[time_column]]), by = deltat)
  join <- do.call(seq, as.list(join))

  fill <- rep_len(0L, length(join))

  .data_full <- lapply(.data, fill, FUN = function(ignore, empty) return(empty))
  .data_full[[time_column]] <- join
  .data_full <- dplyr::as_data_frame(.data_full)

  .data[["..INDEX.."]] <- 1  # Keep track of what's added.

  .data_joined <- dplyr::right_join(x = .data, y = .data_full, by = time_column)

  added_vals <- is.na(.data_joined[["..INDEX.."]])
  keep <- grep(".*\\.x$", colnames(.data_joined), ignore.case = TRUE, value = TRUE)

  out <- .data_joined[c(time_column, keep)] # Drops ..INDEX..

  empty_fill  <- rep_len(empty_fill, sum(added_vals)) # Recycle.
  time_column <- match(time_column, colnames(out)) # Make numeric.

  out[-time_column] <- lapply(out[-time_column], "[<-", added_vals, empty_fill)

  # Handle returned data attributes.
  colnames(out) <- ocolnames
  mostattributes(out) <- oattr
  attr(out, "deltat") <- deltat

  out
}

#' Guess data sampling frequency
#'
#' A convenience function for guessing the sampling frequency of a dataset.
#'
#' @param time.x numeric vector; timer values, in any units.
#'
#' @return A numeric (scalar) giving the typical difference between time points.
#'
#' @export
deltat_guess <- function(time.x) {
  stopifnot(is_timer(time.x))
  runs <- rle(Diff(time.x))
  runs$values[which.max(runs$lengths)]
}
