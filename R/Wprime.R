#' Calculate W' balance
#'
#' @description This function provides an efficient implementation of Dr.
#'   Skiba's W' balance algorithm (Skiba et al., 2012).
#'
#' @details If the \code{Wprime.kJ} argument is supplied, W' balance \emph{per
#'   se} is returned; that is, the output starts at "Wprime.kJ" and will decline
#'   with periods of supra-CP power output. This approach, while most familiar,
#'   relies on a valid estimate of total W' in order that W' \emph{balance} is
#'   not seen to go < 0. This requirement can be circumvented by simply
#'   returning W' \emph{expended}; that is, the ongoing work expended above CP.
#'   W' \emph{expended} will be returned if the \code{Wprime.kJ} argument is
#'   left as \code{NULL} (default).
#'
#' @references Skiba PF, Chidnok W, Vanhatalo A, Jones AM. Modeling the
#'   Expenditure and Reconstitution of Work Capacity above Critical Power.
#'   Medicine & Science in Sports & Exercise 44: 1526–1532, 2012.
#'
#'   Skiba PF, Jackman S, Clarke D, Vanhatalo A, Jones AM. Effect of Work and
#'   Recovery Durations on W' Reconstitution during Intermittent Exercise.
#'   Medicine & Science in Sports & Exercise 46: 1433–1440, 2014.
#'
#'   Skiba PF, Fulford J, Clarke DC, Vanhatalo A, Jones AM. Intramuscular
#'   determinants of the ability to recover work capacity above critical power.
#'   European Journal of Applied Physiology ( November 26, 2014). doi:
#'   10.1007/s00421-014-3050-3.
#'
#' @param time.s,power.W numeric vectors of similar length describing elapsed
#'   time and corresponding power output recordings.
#' @param CP,Wprime.kJ numerics (scalar) describing the two parameters of the
#'   basic critical power model, given in units of watts and kilojoules,
#'   respectively.
#' @param data optional; a \code{data.frame} in which to find \code{time.s} and
#'   \code{power.W} arguments. If provided (i.e. not \code{NULL}), \code{time.s}
#'   and \code{power.W} arguments should be bare, unquoted column names.
#' @param smooth logical; should power data be averaged into sections of sub-
#'   and supra-CP power output?
#'
#' @return A numeric vector of W' expended/balance values.
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' data(chaingang)
#'
#' chaingang <- mutate(chaingang,
#'                     Wbal = Wbalance(time.s, power.W, CP = 325))
#'
#' # For an estimate of W'
#' max(chaingang$Wbal)
#'
#' # This is probably being inflated by passing raw power data
#' # to the function. A solution:
#' chaingang <- mutate(chaingang,
#'                     power.W = roll_mean(power.W, 25, ema = TRUE),
#'                     Wbal = Wbalance(time.s, power.W, CP = 325))
#'
#' max(chaingang$Wbal)  # 19.4 is more realistic.
#'
#' plot(Wbal ~ time.s, type = "l", col = "red", data = chaingang)
#'
#' # This representation is probably unintuitive to most.
#' # Hence, some modified approaches:
#'
#' # Reverse the y axis...
#' ylim <- extendrange(chaingang$Wbal) %>% rev
#' plot(Wbal ~ time.s, type = "l", col = "red", ylim = ylim, data = chaingang)
#' title("Flipped y axis")
#'
#' # Subtract an estimate of W'
#' Wprime <- max(chaingang$Wbal)
#' plot(Wprime - Wbal ~ time.s, type = "l", col = "red", data = chaingang)
#' title("W' subtracted")
#' @export
Wbalance <- function(time.s, power.W, CP, Wprime.kJ = NULL, data = NULL, smooth = FALSE) {
  if (!is.null(data)) {
    time.s  <- eval(substitute(time.s), envir = data)
    power.W <- eval(substitute(power.W), envir = data)
  }

  stopifnot(length(time.s) == length(power.W))

  if (smooth) {
    f <- diff_index(power.W <= CP)
    power.W <- ave(power.W, f, FUN = mean)
  }

  Wexp <- WEXPEND(time.s, power.W, CP) / 1000 # kJ.
  attr(Wexp, "CP") <- CP

  if (!is.null(Wprime.kJ)) {
    Wprime.kJ - Wexp  # Return W' balance.
  } else {
    Wexp              # Return raw expenditure.
  }
}
