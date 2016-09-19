#' Clean raw cycling device data.
#'
#' @description These functions implement a consistent and predictable column
#'   structure for incoming cycling device data. This should permit, where
#'   necessary, batch operations on various different file formats. See below
#'   for details of this structure.
#'
#' @details Note that these functions do not retain any of the original data
#'   columns. If extra columns want to be retained, extra arguments can be
#'   passed down to \code{\link[dplyr]{select}} via \code{...}
#'
#'   If any required columns are missing in the original \code{data}, those
#'   columns of the specification (below) that depend on them will still be
#'   returned, but filled with \code{NA}s.
#'
#'   When imported with \code{\link{import_ride}}, the returned data is given a
#'   file extension attribute. This attribute is the basis for method dispatch
#'   here.
#'
#' @template clean_structure
#'
#' @param data ride data (from \code{\link{import_ride}}) to be "cleaned".
#' @param ... arguments to be passed to \code{\link[dplyr]{select}}.
#'
#' @return a \code{\link[dplyr]{tbl_df}} with the column structure as described
#'   above, with a \code{"start_time"} attribute appended where available.
#'
#' @examples
#' ride_file <- system.file("extdata/lufbra.fit", package = "elpatron")
#'
#' parsed_ride <- import_ride(ride_file, make_laps = TRUE)
#'
#' ## Simple cleaning:
#' clean_bikedata(parsed_ride)
#'
#' ## We can also make use of dplyr's select_helpers
#' ## (see ?dplyr::select_helpers)
#'
#' library(dplyr, warn.conflicts = FALSE)
#' clean_bikedata(parsed_ride, contains("torque"))
#'
#' ## Trying to hold on to non-existent fields won't throw errors.
#' clean_bikedata(parsed_ride, contains("epo_concentration"))
#' @export
clean_bikedata <- function(data, ...) {
  class(data) <- attr(data, "file_ext")  # attr comes from import_ride.* functions
  UseMethod("clean_bikedata", data)
}

# Cleaning instructions take the form of:
#   dependency ~ dplyr::transmute argument.
#
# NB: A lhs dot implies: *copy the rhs*.
# NB: A rhs-only formula with NA means this column is known
#     to always be unavailable.

# ---------------------- #
#          .FIT          #
# ---------------------- #
#' @export
clean_bikedata.fit <- function(data, ...) {
  instructions <- list(
    time.s      = timestamp.s ~ timestamp.s - timestamp.s[1],
    lon         = . ~ position_long.degrees,
    lat         = . ~ position_lat.degrees,
    distance.km = distance.m ~ distance.m / 1000,
    speed.kmh   = `speed.m/s` ~ (`speed.m/s` * 60^2) / 1000,
    elevation.m = . ~ altitude.m,
    VAM         = altitude.m ~ VAM_calc(time.s, altitude.m),
    work.kJ     = power.watts ~ Cumsum(power.watts * Diff(time.s) / 1000),
    power.W     = . ~ power.watts,
    cadence.rpm = . ~ cadence.rpm,
    hr.bpm      = . ~ heart_rate.bpm,
    temp.C      = . ~ temperature.C,
    lap         = . ~ lap
  )
  cleaner(data, instructions, ...)  # Dots == dplyr::select arguments.
}

# ---------------------- #
#          .PWX          #
# ---------------------- #
#' @export
clean_bikedata.pwx <- function(data, ...) {
  instructions <- list(
    time.s      = . ~ timeoffset,
    lon         = ~NA,
    lat         = ~NA,
    distance.km = dist ~ dist / 1000,
    speed.kmh   = spd ~ (spd * 60^2) / 1000,
    elevation.m = . ~ alt,
    VAM         = ~NA,  # Not enough precision in `alt`.
    work.kJ     = pwr ~ Cumsum(pwr * Diff(timeoffset) / 1000),
    power.W     = . ~ pwr,
    cadence.rpm = . ~ cad,
    hr.bpm      = . ~ hr,
    temp.C      = . ~ temp,
    lap         = . ~ lap
  )
  cleaner(data, instructions, ...)
}

# ---------------------- #
#          .TCX          #
# ---------------------- #
#' @export
clean_bikedata.tcx <- function(data, ...) {
  instructions <- list(
    time.s      = Time ~ as.numeric(strptime(Time, "%FT%T")),  # DO THIS ELSEWHERE!
    time.s      = Time ~ time.s - time.s[1],
    lon         = . ~ LongitudeDegrees,
    lat         = . ~ LatitudeDegrees,
    distance.km = DistanceMeters ~ DistanceMeters / 1000,
    # `Speed` might be missing.
    speed.kmh   = DistanceMeters ~ Diff(distance.km) / Diff(time.s) * 60^2,
    elevation.m = . ~ AltitudeMeters,
    VAM         = AltitudeMeters ~ VAM_calc(time.s, AltitudeMeters),
    work.kJ     = Watts ~ Cumsum(Watts * Diff(time.s) / 1000),
    power.W     = . ~ Watts,
    cadence.rpm = . ~ Cadence,
    hr.bpm      = . ~ HeartRateBpm,
    temp.C      = ~NA,
    lap         = . ~ lap
  )
  cleaner(data, instructions, ...)
}

# ---------------------- #
#          .GPX          #
# ---------------------- #
#' @export
clean_bikedata.gpx <- function(data, ...) {
  instructions <- list(
    time.s      = time ~ posix_to_timer(timestamp.posix),
    lon         = . ~ lon,
    lat         = . ~ lat,
    distance.km = ~NA,
    speed.kmh   = ~NA,
    elevation.m = . ~ ele,
    VAM         = ~NA,  # Not enough precision.
    work.kJ     = ~NA,
    power.W     = ~NA,
    cadence.rpm = . ~ cad,
    hr.bpm      = . ~ hr,
    temp.C      = . ~ atemp,
    lap         = . ~ lap
  )
  cleaner(data, instructions, ...)
}

# ---------------------- #
#          .SRM          #
# ---------------------- #
#' @export
clean_bikedata.srm <- function(data, ...) {
  instructions <- list(
    time.s      = timeoffset ~ posix_to_timer(timestamp.posix),
    lon         = . ~ lon,
    lat         = . ~ lat,
    distance.km = . ~ km,
    speed.kmh   = . ~ kph,
    elevation.m = alt ~ if (all(!alt)) NA else alt,  # could be filled with zeros
    VAM         = ~NA,    # ignore for now.
    work.kJ     = watts ~ Cumsum(watts * Diff(time.s) / 1000),
    power.W     = . ~ watts,
    cadence.rpm = . ~ cad,
    hr.bpm      = . ~ hr,
    temp.C      = . ~ temp,
    lap         = . ~ lap
  )
  cleaner(data, instructions, ...)
}

# ************************* #
#          DEFAULT          #
# ************************* #
#' @export
clean_bikedata.default <- function(data, ...) {
  stop_("Unrecognised file extension.")
}

# ---------------------------------------------------------------------
cleaner <- function(data, instructions, ...) {
  oattr <- attributes(data)  # restore later

  .instr <- parse_instructions(instructions)
  # Unpack (because it looks nicer)...
  needs    <- .instr$needs
  new_cols <- .instr$new_columns

  available <- map_lgl(needs, `%in%`, names(data))
  new_cols[!available] <- NA  # will recycle in the data_frame

  out <- dplyr::transmute_(data, .dots = new_cols)  # drops old columns

  # Handle dots (i.e. dplyr::select arguments).
  dots <- lazyeval::lazy_dots(...)
  extra_columns <- dplyr::select_(data, .dots = dots)
  if (length(extra_columns)) {  # columns to append?
    out <- dplyr::bind_cols(out, extra_columns)
  }

  out <- dplyr::filter_(out, ~time.s != 0)  # see column_spec

  # Handle attributes.
  transfer_attrs(out) <- oattr
  if (!is.null(data$timestamp.posix)) {
    attr(out, "start_time") <- data$timestamp.posix[1]
  }

  out
}

parse_instructions <- function(instructions) {
  formula_env <- environment(instructions[[1]])
  lhs <- lapply(instructions, function(x) {
    # Unimpressed face if column isn't available.
    if (rhs_only(x)) "._." else deparse(x[[2]])
  })
  rhs <- lapply(instructions, function(x) {
    deparse(x[[if (rhs_only(x)) 2 else 3]])
  })
  # A dot means transpose rhs.
  dots <- lhs == "."
  lhs[dots] <- rhs[dots]

  rhs <- lapply(rhs, e = formula_env, function(x, e) {
    as.formula(paste0("~",x), env = e)
  })

  list(needs = lhs, new_columns = rhs)
}
