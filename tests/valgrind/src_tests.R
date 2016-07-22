library(elpatron)

# Functions to test:
# Rcpp functions are UPPER_CASE:
# > fns <- grep("[[:upper:]]$", ls(loadNamespace("elpatron")), value = TRUE)
# > cat(paste(fns, collapse = "\n"))
# ----------------------------------
# CUMSUM
# DIFF
# EMA_WEIGHTS
# PARSE_FIT
# PARSE_GPX
# PARSE_PWX
# PARSE_TCX
# PWX_START_TIME
# ROLLMEAN
# ROLLMEAN_TIME
# TCX_LAPS
# WEXPEND
# ----------------------------------
# NOTE: dump function outputs to `.` to suppress printed output.

elpatron:::CUMSUM(1:10) -> .
elpatron:::DIFF(1:10) -> .
elpatron:::EMA_WEIGHTS(10) -> .

fls <- list.files(
  system.file("extdata", package = "elpatron"),
  full.names = TRUE)

names(fls) <- tools::file_ext(fls)

# NOTE: valgrind will complain about the FIT SDK src code.
#       I don't know how to fix that.
elpatron:::PARSE_FIT(fls["fit"]) -> .
elpatron:::PARSE_GPX(fls["gpx"]) -> .
elpatron:::PARSE_PWX(fls["pwx"]) -> .
elpatron:::PWX_START_TIME(fls["pwx"]) -> .
elpatron:::PARSE_TCX(fls["tcx"]) -> .
elpatron:::TCX_LAPS(fls["tcx"]) -> .

.x <- rnorm(100, 500, 200)
elpatron:::ROLLMEAN(.x, 30, na_rm = FALSE, ema = FALSE) -> .
elpatron:::ROLLMEAN(.x, 30, na_rm = FALSE, ema = TRUE) -> .

.t <- sort(sample(1:500, 100))
elpatron:::ROLLMEAN_TIME(.x, .t, 30, na_rm = FALSE) -> .

.pwr <- c(rnorm(100, 500, 50), rnorm(100, 200, 50))
.t   <- seq_along(.pwr)
elpatron:::WEXPEND(.t, .pwr, 300) -> .
