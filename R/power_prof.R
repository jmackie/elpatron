#' Cycling power profiling
#'
#' @description Generates best powers for a range of time intervals given
#'   vectors of power (Watts) and time (seconds).
#'
#' @details In the interest of efficiency, this function returns a power profile
#'   for which time windows are not known exactly \emph{a priori}. As data can
#'   be, and often are, sampled with inconsistencies, deriving best powers for
#'   known durations is computationally expensive. However, absent any
#'   inconsistencies (e.g. breaks in the data), the returned time windows will
#'   be of duration: \code{windows / sampling_frequency}. Hence, the default
#'   range for the profile should be roughly 2 to 30 minutes (in 10 second
#'   increments) assuming 1 Hz sampling.
#'
#'   If there \emph{are} large breaks in the data, specious values might be
#'   returned. This because head unit devices will record a power value when
#'   recording is resumed after a pause, and this power value will thus be
#'   associated with a large delta time value. In effect, the power value logged
#'   when recording is resumed will be treated as the \emph{mean} power output
#'   for that entire break. With obvious implications. If such a situation is
#'   encountered, this function will throw a warning if \code{quietly = FALSE}.
#'
#' @param power.W,time.s numeric vectors (of equal length) describing power
#'   readings in Watts and the times at which they were recorded (in seconds).
#' @param windows numeric; window(s) over which to derive best powers. If data
#'   are sampled consistently at 1 Hz, these effectively correspond to time
#'   windows in units of seconds. Hence, assuming 1 Hz sampling, the default is
#'   2-30 minutes in 10 second increments.
#' @param quietly logical; should warning messages be suppressed?
#'
#' @return A \code{\link[dplyr]{tbl_df}} with two columns: a vector of time
#'   windows and a vector of corresponding maximal mean powers.
#'
#' @examples
#' data(chaingang)
#' pt <- with(chaingang, power_prof(power.W, time.s))
#' plot(pt)
#' @export
power_prof <- function(power.W, time.s, windows = seq(2*60, 30*60, 10),
                       quietly = FALSE) {

  stopifnot(length(power.W) == length(time.s), is_timer(time.s))

  if (any(Diff(time.s) > 20) && !quietly) {
    msg <- stringwrap(
      "Breaks (>20-seconds) detected in the data.",
      "This could generate specious power values.",
      "Consider resampling the data (?resample)."
    )
    warn_(msg)
  }

  work.J <- power.W[-1] / Diff(time.s)[-1]  # Don't:  / 0.
  cumwork.J <- Cumsum(c(0, work.J))

  prof <- purrr::map(windows, best_power, cumwork.J, time.s)

  dplyr::arrange_(dplyr::bind_rows(prof), ~time.s)
}

best_power <- function(n, cumwork.J, time.s) {
  i <- seq_along(cumwork.J) %notin% seq_len(n)

  time_windows <- time.s[i] - time.s[rev(i)] # Shift.
  rollmeans <- (cumwork.J[i] - cumwork.J[rev(i)]) / time_windows

  best <- which.max(rollmeans)  # na.rm = TRUE.
  list(time.s = time_windows[best], best_power.W = rollmeans[best])
}
