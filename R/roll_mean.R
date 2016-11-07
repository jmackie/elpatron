#' Efficient rolling mean
#'
#' @description Generate a (right-aligned) rolling mean for a vector, optionally
#'   applying exponential weights.
#'
#' @details This function rolls over \code{x} by "element", i.e. \code{window =
#'   10} will average over 10 \emph{adjacent} values in \code{x}. This is
#'   emphasised as often a time-windowed rolling average is desired. The latter
#'   can only be achieved if data are sampled uniformly, i.e. a \code{window =
#'   10} rolling mean will also give a 10-second moving average with data
#'   sampled at 1 Hz. If data are not sampled consistently, consider resampling
#'   with \code{\link{resample}}.
#'
#'   The rolling mean returned here is right-aligned, padded with leading zeros.
#'   That is, there are \code{window - 1} zeros at the start of the return
#'   vector.
#'
#'   Note that the \code{window} argument is silently truncated
#'   (\code{\link[base]{trunc}}). This because a floating-point window size
#'   makes no sense...
#'
#' @param x numeric vector to be rolled over.
#' @param window integer; the window size in \emph{vector element} units. This
#'   argument is silently truncated.
#' @param na.rm logical; should NAs be discarded?
#' @param ema logical; return an "\strong{e}xponentially weighted
#'   \strong{m}oving \strong{a}verage"?
#'
#' @return A vector of the same length as \code{x}.
#'
#' @seealso \code{\link{roll_mean.time}} for rolling over \emph{messily} sampled
#'   data.
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' data(chaingang)
#'
#' # The 'chaingang' dataset is sampled at 1 Hz
#' # without any breaks.
#' diff(chaingang$time.s) %>% unique  # NB!
#'
#' # 30 second rolling mean, and 25 second exponentially-
#' # weighted rolling mean.
#' chaingang <- mutate(chaingang,
#'                     power.30mean = roll_mean(power.W, 30),
#'                     power.25ema = roll_mean(power.W, 25, ema = TRUE))
#'
#' chaingang <- chaingang[-(1:29), ]  # Remove zero padding.
#'
#' plot(power.W ~ time.s, type = "l", col = "gray", data = chaingang)
#' lines(power.25ema ~ time.s, data = chaingang)
#'
#' # FYI:
#' normalised_power <- mean(chaingang$power.30mean^4) ^ (1/4)
#' xPower <- mean(chaingang$power.25ema^4) ^ (1/4)
#' @export
roll_mean <- function(x, window, na.rm = TRUE, ema = FALSE) {
  ROLLMEAN(as.numeric(x), trunc(window), na.rm, ema)
}

#' Roll over time windows.
#'
#' @description A less efficient implementation of \code{\link{roll_mean}} that
#'   can roll over \emph{inconsistently sampled} data by a specified time
#'   window.
#'
#' @details In essence, this function iterates of elements in \code{x}, looking
#'   back through previous elements until the corresponding element in
#'   \code{time} exceeds the specified \code{window}. For example, if the
#'   iteration was at \code{x[100]}, where \code{time[100] == 60} (seconds), a
#'   cumulative mean would be applied to previous values of \code{x} until
#'   \code{time[i] == 30}, if \code{window = 30}.
#'
#' @param x numeric vector to be rolled over.
#' @param time numeric vector of sampling time values.
#' @param window integer; the window size in \emph{time} units.
#' @param na.rm logical; should NAs be discarded?
#'
#' @return A vector of the same length as \code{x}.
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' data(chaingang)
#'
#' # 'chaingang' is sampled uniformly at 1 Hz, but lets
#' # pretend it's messy...
#' chaingang <- sample_n(chaingang, 1500) %>% arrange(time.s)
#'
#' diff(chaingang$time.s) %>% table
#'
#' # With this function, we can still get a 30 second rolling
#' # average:
#' chaingang <- mutate(chaingang,
#'                     power.30mean = roll_mean.time(power.W, time.s, 30))
#'
#' plot(power.W ~ time.s, type = "l", col = "gray", data = chaingang)
#' lines(power.30mean ~ time.s, data = chaingang)
#' @export
roll_mean.time <- function(x, time, window, na.rm = TRUE) {
  stopifnot(is_timer(time))
  ROLLMEAN_TIME(as.numeric(x), as.numeric(time), window, na.rm)
}
