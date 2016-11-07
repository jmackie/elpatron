#' Resample data
#'
#' @description A useful function for working with data that aren't sampled at
#'   regular intervals and(or) contain gaps. This can present issues when, for
#'   example, trying to apply time-windowed rolling operations.
#'
#' @details For time-windowed rolling operations, \code{fill} should logically
#'   be set to \code{0L}; the use of NAs could generate large runs of NAs.
#'
#' @param .data data.frame; the data to be resampled on the basis of
#'   \code{time_column}.
#' @param time_column string; identifier for the column in \code{data} that
#'   gives (elapsed) time values. Right-hand-sided formulas will be deparsed,
#'   otherwise this argument is passed to \code{'[['} as-is.
#' @param Hz numeric (scalar); the desired sampling frequency.
#' @param fill the value with which to "fill" new rows in the returned
#'   data. For most purposes this should be either \code{NA} or a numeric
#'   value (e.g. 0).
#'
#' @return a resampled \code{\link[dplyr]{tbl_df}}, with a  new \code{Hz}
#'   attribute added.
#'
#' @examples
#' data <- data.frame(
#'   timer.s = 1:500,
#'   x = rnorm(500, 500, 30),
#'   y = rnorm(500, 1000, 300)
#' )
#' gappy_data <- data[sort(sample(1:500, 250)), ]
#' gappy_data %>% resample(~timer.s, Hz = 1, fill = NA) %>% head(20)
#' @export
resample <- function(.data, time_column = "time.s",
                     Hz = 1L, fill = 0L) {

  # To be restored later...
  ocolnames <- colnames(.data)
  oattr <- attributes(.data)

  if (is_formula(time_column)) {  # NSE
    time_column <- deparse(time_column[[2]])
  }
  if (!is_timer(.data[[time_column]])) {   # check
    stop_("Time column is invalid.")
  }

  join <- c(range(.data[[time_column]]), by = 1 / Hz)
  join <- do.call(seq, as.list(join))

  empty_column <- rep_len(0L, length(join))

  .data_full <- lapply(.data, empty_column,
                       FUN = function(ignore, empty) return(empty))

  .data_full[[time_column]] <- join
  .data_full <- dplyr::as_data_frame(.data_full)

  .data[["..INDEX.."]] <- 1  # keep track of what's added

  .data_joined <- dplyr::right_join(x = .data, y = .data_full, by = time_column)

  added_vals <- is.na(.data_joined[["..INDEX.."]])
  keep <- grep(".*\\.x$", colnames(.data_joined),
               ignore.case = TRUE, value = TRUE)

  out <- .data_joined[c(time_column, keep)] # drops `..INDEX..`

  filler      <- rep_len(fill, sum(added_vals))     # recycle
  time_column <- match(time_column, colnames(out))  # make numeric

  out[-time_column] <- lapply(out[-time_column], `[<-`, added_vals, filler)

  # Restore attributes.
  colnames(out) <- ocolnames
  transfer_attrs(out) <- oattr
  attr(out, "Hz") <- Hz

  out
}

#' REDUNDANT: Guess data sampling frequency
#'
#' A convenience function for guessing the sampling frequency of a dataset.
#'
#' @param time.x numeric vector; timer values, in any units.
#'
#' @return A numeric (scalar) giving the typical difference between time points.
#'
deltat_guess <- function(time.x) {
  stopifnot(is_timer(time.x))
  runs <- rle(Diff(time.x))
  runs$values[which.max(runs$lengths)]
}
