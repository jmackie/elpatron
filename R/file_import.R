#' Ride file parsing
#'
#' @description The generic \code{import_ride} dispatches methods based on the
#'   file extension of \code{file_path}. See \strong{Details} for currently
#'   supported file formats. The default method (i.e. for unsupported formats)
#'   will raise an error.
#'
#' @details These functions will do very little with the parsed data by default.
#'   This is to allow for more flexibility, but can cause headaches for batch
#'   analysing files of mixed formats (see \code{\link{clean_bikedata}} for a
#'   solution).
#'
#'   Timestamps are converted to \code{\link[base]{POSIXct}} when available.
#'
#'   Currently supported formats are: \itemize{
#'   \item{\strong{pwx:} }{Training Peaks pwx files (an XML format).}
#'   \item{\strong{fit: }}{Garmin fit files (a binary format).}
#'   \item{\strong{tcx: }{Garmin training center files (an XML format).}}
#'   \item{\strong{gpx: }{GPS Exchange Format (a lame XML format).}}
#'   \item{\strong{srm: }{SRM power control files (a binary format).}}
#'   }
#'
#'   Tabular data are always returned as dplyr \code{\link[dplyr]{tbl}} objects.
#'   This ensures clean printing and makes data easier to work with.
#'
#'   The method for SRM power control files uses \code{python} code. Hence
#'   \code{import_ride.srm} will raise an error if the python executable (given
#'   in the argument \code{.python_exec}) is not found on the system path. The
#'   method will also make use of the \code{readr} package if it is available
#'   (\emph{advised}). Note the python script was developed with python version
#'   2.7.11.
#'
#' @param file_path character string; path to the file to be read. Can be
#'   absolute or relative.
#' @param ... further arguments passed to or from other methods.
#'
#' @param raw logical; if \code{TRUE} all data retrieved from the fit file is
#'   returned as a list. \code{raw = FALSE} (default) will just return
#'   "records", which are generally what's expected/of interest.
#' @param make_laps logical; append a lap column to the data (if available)?
#'
#' @param .python_exec character; path to a python executable. E.g.
#'   \code{Sys.which("python")}. The script was developed with python v2.7.11.
#'
#' @section Notes: Garmin Fit files return positional coordinates in units of
#'   semicircles. These are converted to degrees by default (if \code{raw =
#'   FALSE}). The present implementation does not correctly parse
#'   \emph{enhanced} fields, so these are discarded; not that these are of
#'   interest for most human-powered activities!
#'
#' @section Acknowledgements: Fit files are parsed using code from the
#'   \href{https://www.thisisant.com/resources/fit}{ANT+ FIT SDK}. This code was
#'   ported to \code{\link[Rcpp]{Rcpp}} and kindly contributed by
#'   \href{https://github.com/kuperov}{Alex Cooper}
#'
#'   XML file formats are parsed using the \href{http://pugixml.org/}{pugixml}
#'   C++ library.
#'
#'   SRM parsing code was adapted from the code in the Golden Cheetah repository
#'   (which can be found
#'   \href{https://github.com/GoldenCheetah/GoldenCheetah/blob/master/src/FileIO/SrmRideFile.cpp}{here}).
#'
#'   All third-party licenses can be found in \code{system.file("licenses",
#'   package = "elpatron")}.
#'
#' @return With very few exceptions, these functions return data as a
#'   \code{\link[dplyr]{tbl_df}}.
#'
#' @seealso \code{\link{clean_bikedata}} for creating consistent output from
#'   these functions.
#'
#' @examples \dontrun{
#' ride_files <- list.files(
#'   system.file("extdata", package = "elpatron"),
#'   full.names = TRUE)
#'
#' (f <- sample(ride_files, 1))
#' import_ride(f)
#'
#' ## If you want all data from the .fit format...
#' f <- grep("fit$", ride_files, value = TRUE)
#' import_ride(f, raw = TRUE)
#' }
#' @export
import_ride <- function(file_path, ...) {
  # For file.exists, tilde expansion, etc.
  file_path <- normalizePath(file_path[1], mustWork = TRUE)

  # Method dispatch on file extension.
  filext <- tolower(tools::file_ext(file_path))
  class(file_path) <- filext

  UseMethod("import_ride", file_path)
}

# ---------------------- #
#          .FIT          #
# ---------------------- #
#' @rdname import_ride
#' @export
import_ride.fit <- function(file_path, raw = FALSE, make_laps = TRUE, ...) {
  fit_ls <- PARSE_FIT(file_path)
  if (raw) return(fit_ls)

  out <- dplyr::as_data_frame(fit_ls$record)

  col_names <- paste(colnames(out), attr(fit_ls$record, "units"), sep = ".")
  colnames(out) <- col_names

  # Timestamps (from Fit SDK; file types descrip; p. 49):
  # "Seconds since UTC 00:00 Dec 31 1989"
  out$timestamp.posix <- as.POSIXct(out$timestamp.s,
                                    tz = "UTC", origin = "1990-01-01")

  # Coordinates: semicircles to degrees.
  if (any(lonlat <- grepl("position", col_names))) {
    out[lonlat] <- lapply(out[lonlat], semicircle_correct)  # To degrees.
    degree_names <- gsub("semicircles", "degrees", colnames(out[lonlat]))
    colnames(out) <- replace(colnames(out), lonlat, degree_names)
  }

  # Enhanced columns aren't parsed correctly:
  # https://www.thisisant.com/forum/viewthread/4561
  out <- dplyr::select_(out, quote(-contains("enhanced")))

  # Create lap column.
  if (make_laps) {
    out$lap <- 0  # Recycle.
    out$lap <- with(out, inset(lap, timestamp.s %in% fit_ls$lap$start_time, 1))
    out$lap <- cumsum(out$lap)
  }

  attr(out, "file_ext") <- "fit"
  out
}

# ---------------------- #
#          .PWX          #
# ---------------------- #
#' @rdname import_ride
#' @export
import_ride.pwx <- function(file_path, ...) {
  pwx_ls <- PARSE_PWX(file_path)
  if (zlen(pwx_ls)) stop_("Error reading file.")
  out   <- map_df(pwx_ls, as.list.default)
  out[] <- lapply(out, try_as_numeric)

  start_t <- PWX_START_TIME(file_path)
  if (nzchar(start_t)) {
    start_t <- make_time_col(start_t, tz = "UTC")  # I assume these are UTC...
    out <- dplyr::mutate_(out, timestamp.posix = ~start_t + timeoffset)
  }

  attr(out, "file_ext") <- "pwx"
  out
}

# ---------------------- #
#          .TCX          #
# ---------------------- #
#' @rdname import_ride
#' @export
import_ride.tcx <- function(file_path, make_laps = TRUE, ...) {
  tcx_ls <- PARSE_TCX(file_path)
  if (zlen(tcx_ls)) stop_("Error reading file.")

  # Need to fill in the gaps.
  out <- xml_fill_dim(tcx_ls)

  if (make_laps && !zlen(lap_starts <- TCX_LAPS(file_path))) {
    out$lap <- inset(rep_len(0, nrow(out)), 1, 1)
    lapi <- map_dbl(lap_starts, match, out$Time)
    out$lap <- cumsum(inset(out$lap, na_rm(lapi), 1))
  }

  # Parse timestamp by default.
  if (!is.null(out$Time)) {
    out$timestamp.posix <- make_time_col(out$Time, tz = "UTC")
  }

  attr(out, "file_ext") <- "tcx"
  out
}

# ---------------------- #
#          .GPX          #
# ---------------------- #
#' @rdname import_ride
#' @export
import_ride.gpx <- function(file_path, ...) {
  gpx_ls <- PARSE_GPX(file_path)
  if (zlen(gpx_ls)) stop_("Error reading file.")

  # Need to fill in the gaps.
  out <- xml_fill_dim(gpx_ls)

  colnames(out) <- sub("^.*:", "", colnames(out))  # From extensions.
  if (!is.null(out$time)) {
    out$timestamp.posix <- make_time_col(out$time, tz = "UTC")
  }

  attr(out, "file_ext") <- "gpx"
  out
}
# ---------------------- #
#          .SRM          #
# ---------------------- #
#' @rdname import_ride
#' @export
import_ride.srm <- function(file_path, ..., .python_exec = "python") {
  srm_file_check(file_path)

  if (!nchar(Sys.which(.python_exec))) {
    stop_("Python executable not found on system path, returning NULL.")
    return(NULL)
  }

  srm_parser <- system.file("py/srm_parser.py", package = "elpatron")
  temp_file  <- tempfile("elpatron_SRM_", fileext = ".csv")
  system2(.python_exec, c(srm_parser, file_path, temp_file))  # Read to file.

  if (pkg_available("readr")) {
    coltypes <- collapse(rep_len("d", 11))
    out <- readr::read_delim(
      file = temp_file, delim = "\t", col_names = TRUE, col_types = coltypes
    )
  } else {
    out <- read.delim(file = temp_file, header = TRUE)
    out <- dplyr::as_data_frame(out)
  }

  # Make timestamps.
  out$timestamp.posix <- (as.POSIXct(srm_file_date(file_path))
                          + out$timeoffset)

  attr(out, "file_ext") <- "srm"
  out
}


# ************************* #
#          DEFAULT          #
# ************************* #
#' @export
import_ride.default <- function(file_path, ...) {
  stop_("Unsupported file extension.")
}


# -------------------------------
#      File reading utils
# -------------------------------
unify <- function(x, y, return_list = TRUE) {
  out <- c(x, y[names(y) %notin% names(x)])
  if (return_list) as.list(out[names(y)]) else out[names(y)]
}

try_as_numeric <- function(x) {
  x.num <- suppressWarnings(as.numeric(x))
  if (all(is.na(x.num))) x else x.num
}

xml_fill_dim <- function(.list) {
  full_member <- .list[[which.max(lengths(.list))]]
  empty_template <- rep.int(NA, length(full_member))
  names(empty_template) <- names(full_member)

  out   <- dplyr::bind_rows(lapply(.list, unify, empty_template))
  out[] <- lapply(out, try_as_numeric)  # Keeping attrs.
  out
}

srm_file_check <- function(file_path) {
  # These checks are made in the python script. But it's cleaner
  # to raise errors in R.
  srm_conn <- file(file_path, "rb")
  on.exit(close(srm_conn))

  magic <- readBin(srm_conn, "int", n = 4L, size = 1L,
                   signed = FALSE, endian = "little")

  # Bad file.
  if (intToUtf8(magic[1:3]) != "SRM") {
    stop_("Unrecognised file stream. Is this an SRM file?")
  }
  return(0)
}

srm_file_date <- function(file_path) {  # Days since 1880-01-01.
  srm_conn <- file(file_path, "rb")
  on.exit(close(srm_conn))
  seek <- 4L   # magic
  readBin(srm_conn, "int", n = seek, size = 1L,
          signed = FALSE, endian = "little")
  # Reading an unsigned short.
  days <- readBin(srm_conn, "int", n = 1, size = 2L,
                  signed = FALSE, endian = "little")

  as.Date("1880-01-01") + days
}
