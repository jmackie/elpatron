#' @section Clean structure: In general, fields should be named as
#'   \code{"field.units"}, where the field is generally written in full. Time
#'   derivatives should be adjacent to their "parent" (e.g. distance, speed;
#'   work, power). If data for any of these columns are missing the column
#'   should be included, but filled with \code{NA}s. The SI unit system should
#'   be adhered to.
#'
#' \describe{
#'
#'   \item{time.s}{
#'   Elapsed time \emph{since} the start of recording, in seconds. Should not
#'   start at 0.
#'   }
#'
#'   \item{lon, lat}{
#'   Positional coordinates in degrees. Keeping these fields in lon,lat order is
#'   in keeping with the x,y convention.
#'   }
#'
#'   \item{distance.km, speed.kmh}{
#'   Self-explanatory: cumulative distance covered (kilometres) and speed
#'   (kilometres per hour).
#'   }
#'
#'   \item{elevation.m, VAM}{
#'   Metres above sea level and "vertical ascent metres per second".
#'   }
#'
#'   \item{work.kJ, power.W}{
#'   Self-explanatory: cumulative work done (kilojoules) and power output (Watts).
#'   }
#'
#'   \item{cadence.rpm}{
#'   Pedal (angular) velocity in revolutions per minute.
#'   }
#'
#'   \item{hr.bpm}{
#'   Heart rate in beats per minute.
#'   }
#'
#'   \item{temp.C}{
#'   Ambient temperature in degrees Celsius.
#'   }
#'
#'   \item{lap}{
#'   A lap counter, starting from 1.
#'
#'   }
#'
#' }
