#' HH:MM:SS conversion
#'
#' Convert between seconds and the more readable "HH:MM:SS" time format.
#'
#' @param hms character string; a time in the form "HH:MM:SS" ("HH:" is
#'   optional).
#' @param sec numeric; seconds values.
#' @param digits numeric (scalar); number of decimal places to be used for the
#'   seconds value in the "hours:minutes:seconds" string.
#' @param strip_zeros logical; remove leading zeros? E.g. "00:26:01" becomes
#'   "26:01".
#'
#' @examples
#' x <- c("00:21:05", "25:51", NA, "00:26:01.1", "01:05:02.0")
#' (x <- hms_to_sec(x))
#' (x <- sec_to_hms(x, strip_zeros = TRUE))
#' @name hhmmss
NULL
# ------------------------------------------------------------------------------
#' @rdname hhmmss
#' @export
hms_to_sec <- function(hms) {
  #hms <- c("30:20:10.1", "40:20:10.1")
  stopifnot(all(is.character(hms)))
  hms <- strsplit(hms, ":")
  vapply(hms, FUN.VALUE = numeric(1), FUN = function(x) {
    sum(rev(as.numeric(x)) * c(1, 60, 60 ^ 2)[seq_along(x)])
  })
}
#' @rdname hhmmss
#' @export
sec_to_hms <- function(sec, digits = 1, strip_zeros = FALSE) {
  #sec <- rep(20*60 + 21.2, 2)
  stopifnot(is.numeric(sec))
  h  <- sec / (60 ^ 2)
  m  <- (h - (h <- floor(h))) * 60
  s  <- (m - (m <- floor(m))) * 60
  fmt <- "%02d:%02d:%0"
  fmt.digits <- if (digits <- abs(digits)) {
    paste0(digits + 3, ".", digits, "f")
  } else {
    "2d"
  }
  out  <- sprintf(fmt %+% fmt.digits, h, m, if (!digits) round(s) else s)
  out[grep("NA", out, ignore.case = TRUE)] <- NA
  if (strip_zeros) {
    out <- sub("^(00:){1,}", "", out)
  }
  out
}
