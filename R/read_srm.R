# An R implementation of the Golden Cheetah SRM parsing code:
# https://github.com/GoldenCheetah/GoldenCheetah/blob/master/src/FileIO/SrmRideFile.cpp

read_srm <- function(file_path) {

  formals(unpack)$endianness <- "little"  # creates a local copy
  conn <- file(file_path, "rb")
  on.exit(close(conn))

  # Check the file format
  # ---------------------
  magic <- read_bin_string(conn, 4L)

  if (substr(magic, 1, 3) != "SRM") {
    stop_("Unrecognised file stream. Is this an SRM file?")
  }

  version <- as.numeric(substr(magic, 4, 4))

  # File header
  # -----------
  header <- list(
    days_since_1880 = unpack("H", conn),
    wheel_circum = unpack("H", conn),
    # Recording interval (1 / Hz) in seconds.
    recording_interval = unpack("B", conn) / unpack("B", conn),
    block_count = unpack("H", conn),
    marker_count = unpack("H", conn),
    padding = unpack("x", conn),
    comment_len = unpack("B", conn))

  # Skip over the 70 byte comment.
  seek.connection(conn, 70, origin = "current")

  # Markers
  # -------
  marker_comment_len = if (version < 6) 3 else 255

  markers <- lapply(seq_len(header$marker_count + 1), function(i) {
    # SRM files **always contain at least one marker**
    # that encompasses the entire srmfile (hence marker_count + 1).
    out <- list(
      note = read_bin_string(conn, marker_comment_len),
      active = as.logical(unpack("B", conn)),   # bool
      start = if (version < 9) unpack("H", conn) else unpack("I", conn),
      end = if (version < 9) unpack("H", conn) else unpack("I", conn),
      average_watts = unpack("H", conn),
      average_hr = unpack("H", conn),
      average_cadence = unpack("H", conn),
      average_speed = unpack("H", conn),
      pwc150 = unpack("H", conn))

    se <- c("start", "end")   # for brevity

    out[se] <- replace(out[se], out[se] < 1, 1)
    # Some srmwin versions wrote markers with start > end.
    out[se] <- sort(unlist(out[se]))
    out
  })

  # Blocks
  # ------
  blocks <- lapply(seq_len(header$block_count), function(i) {
    list(
      sec_since_midnight = unpack("I", conn) / 100,   # hsec to sec
      data_chunks = if (version < 9) unpack("H", conn) else unpack("I", conn))
  })

  block_data_count <- sum(map_dbl(blocks, "data_chunks"))

  # Skip over calibration data.
  seek.connection(conn, 4, origin = "current")

  data_count <- if (version < 9) unpack("H", conn) else unpack("I", conn)
  seek.connection(conn, 1, origin = "current")  # padding

  nrows <- max(data_count, block_data_count)

  # Data chunks
  # -----------
  data_chunks <- rep(list(NULL), nrows)

  # Some setup before reading the chunks:
  fields = c("watts", "cad", "hr", "kph", "alt", "temp", "km",
             "lat", "lon", "timeoffset", "lap")
  env = environment()   # lapply(fields, get, envir = env)
  rec_interval_sec <- header$recording_interval   # cleaner

  i_marker = if (header$marker_count) 2 else 1    # any laps?
  current_marker = markers[[i_marker]]
  i_block <- block_progress <- 1
  new_block <- FALSE
  current_block <-  blocks[[i_block]]
  lap <- 1

  for (i in seq_len(nrows)) {

    if (version < 7) {
      # This is horrible, I apologise.
      spd_pwr <- c(unpack("B", conn), unpack("B", conn), unpack("B", conn))
      kph <- bitwOr(
        bitwShiftL(bitwAnd(spd_pwr[2], 0xf0), 3),
        bitwAnd(spd_pwr[1], 0x7f)) * 3 / 26
      watts <- bitwOr(
        bitwAnd(spd_pwr[2], 0x0f),
        bitwShiftL(spd_pwr[3], 0x4))
      cad <- unpack("B", conn)
      hr  <- unpack("B", conn)
      alt <- temp <- NA

    } else {
      watts <- unpack("H", conn)
      cad   <- unpack("B", conn)
      hr    <- unpack("B", conn)
      kph   <- unpack("i", conn)
      alt   <- unpack("i", conn)
      temp  <- unpack("h", conn) * 0.1

      kph <- if (kph < 0) 0 else kph * 3.6 / 1000
    }

    if (version == 9) {
      lat <- unpack("i", conn) * 180.0 / 0x7fffffff
      lon <- unpack("i", conn) * 180.0 / 0x7fffffff

    } else {
      lat <- lon <- NA
    }

    # And generate a distance field (cumsum'd later).
    km <- rec_interval_sec * kph / 3600

    # Time offset:
    # Since the Powercontrol unit may go into power save mode when idle, the data
    # may represent multiple discontinuous blocks. Each data block header contains
    # the time stamp.
    if (block_progress > current_block$data_chunks) {
      i_block <- i_block + 1
      current_block  <- blocks[[i_block]]
      block_progress <- 1  # reset
      new_block <- TRUE    # flag
    }

    if (i-1 && new_block) {
      timeoffset <- current_block$sec_since_midnight
      new_block  <- FALSE    # reset

    } else if (i-1) {
      # Increment previous value by recording interval.
      timeoffset <- timeoffset + rec_interval_sec

    } else {
      timeoffset <- blocks[[1]]$sec_since_midnight
    }

    block_progress <- block_progress + 1  # increment

    # Laps:
    if (i_marker < length(markers) && (i == current_marker["end"])) {
      i_marker <- i_marker + 1
      current_marker <- markers[[i_marker]]
      lap <- lap + 1
    }
    data_chunks[[i]] <- lapply(fields, get, envir = env)
  }

  data_chunks <- lapply(data_chunks, `names<-`, fields)

  out <- dplyr::bind_rows(data_chunks)
  out$km <- Cumsum(out$km)

  # Need to check this!
  attr(out, "start_date") <- as.POSIXct(header$days_since_1880 * 24 * 60^2,
                                        origin = "1880-01-01")
  out
}
# x <- read_srm("/home/jmach/R/projects/elpatron/inst/extdata/example.srm")
