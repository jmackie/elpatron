context("Testing file reading/cleaning")

#' These tests depend on the following packages:
#'  + xml2
#'  + readr
#'
#' The latter tests will also make use of my own repository
#' of ride files (various formats), but absent these extdata
#' files will be used.
#'
#' NB: These tests are written with my own local machine in mind.

test_that("Reading extdata directory", {
  files <- system.file("extdata", package = "elpatron") %>%
    list.files(full.names = TRUE)

  # Read all.
  for (f in files) import_ride(f) %>% expect_s3_class("tbl_df")

  # Check unsupported file extension.
  f <- "bogus_file.txt"
  expect_error(import_ride(f))

  # Check raw .fit return.
  f <- grep("lufbra.fit", files, value = TRUE)
  expect_type(import_ride(f, raw = TRUE), "list")
})


test_that("Cleaning extdata directory", {
  files <- system.file("extdata", package = "elpatron") %>%
    list.files(full.names = TRUE)

  # Clean files should have (by default) 12 columns.
  # see ?clean_bikedata
  for (f in files) {
    import_ride(f) %>% clean_bikedata %>% expect_length(12)
  }
})

# Check the output of individual import functions ----------------------------

# Use the ride file repository on my system if it's available.
get_test_file <- function(ext) {
  testdir <- paste0("../../.testfiles/", ext)
  if (dir.exists(testdir)) {
    file_path <- list.files(testdir, full.names = TRUE) %>% sample(1)
  } else {
    example_files <- system.file("extdata", package = "elpatron") %>%
      list.files(full.names = TRUE)
    ext_re <- paste0(ext, "$")
    file_path <- example_files %>% grep(ext_re, ., value = TRUE)
  }
  file_path
}

# Don't throw errors if dependencies aren't available:
returnifnot <- function(...) {
  proceed <- function(...) invisible("Fine!")
  tests <- vapply(list(...), force, logical(1))
  if (any(!tests)) return else proceed
}

is_posix <- function(x) {
  any(grepl("posix", class(x), ignore.case = TRUE))
}

# Dynamically choose testthat expectation (for XML files).
expect_Equal <- function(x1, x2, ...) {
  if (is_posix(x1) || is_posix(x2)) {
    return(TRUE)  # Ignore time column.
  } else if (is.character(x1)) {
    expect_identical(x1, x2, ...)
  } else if (is.numeric(x1)) {
    expect_equal(x1, x2, ...)
  }
}

# ---- Helper functions for parsing (nested) XML -----------
try_as_numeric <- elpatron:::try_as_numeric

recursive_text <- function(x) {
  x.children <- xml2::xml_children(x)
  out <- lapply(x.children, recursive_extract)
  # Flatten the list and make numeric.
  unlist(out, recursive = TRUE, use.names = TRUE) %>% lapply(try_as_numeric)
}

recursive_extract <- function(child) {
  if (xml2::xml_length(child)) {
    recursive_text(child)
  } else {
    nm <- xml2::xml_name(child)
    if (nm == "Value") nm <- xml2::xml_name(xml2::xml_parent(child))
    xml2::xml_text(child) %>% setNames(nm)
  }
}

# ---------------------- #
#          .PWX          #
# ---------------------- #
test_that("PWX files are parsed correctly", {
  returnifnot(require(xml2))()
  file_path <- get_test_file("pwx")

  imported <- import_ride(file_path)

  samples <- read_xml(file_path) %>% xml_find_all("//d1:sample", xml_ns(.))

  expect_equal(nrow(imported), length(samples))

  i <- sample(seq_along(samples), 1)  # Pick a random "row".
  check <- recursive_text(samples[i])

  for (n in names(check)) {
    expect_Equal(imported[[n]][i], check[[n]], info = file_path)
  }
})

# ---------------------- #
#          .TCX          #
# ---------------------- #

test_that("TCX files are parsed correctly", {
  returnifnot(require(xml2))()
  file_path <- get_test_file("tcx")

  imported <- import_ride(file_path)

  trkpoints <- read_xml(file_path) %>% xml_find_all("//d1:Trackpoint", xml_ns(.))

  expect_equal(nrow(imported), length(trkpoints))

  i <- sample(seq_along(trkpoints), 1)  # Pick a random "row".
  check <- recursive_text(trkpoints[i])

  for (n in names(check)) {
    expect_Equal(imported[[n]][i], check[[n]], info = file_path)
  }
})

# ---------------------- #
#          .GPX          #
# ---------------------- #
test_that("GPX files are parsed correctly", {
  returnifnot(require(xml2))()

  # No other test files for .gpx, because they're lame.
  file_path <- system.file("extdata/lufbra.gpx", package = "elpatron")

  imported <- import_ride(file_path)

  trkpoints <- read_xml(file_path) %>% xml_find_all("//d1:trkpt", xml_ns(.))

  expect_equal(nrow(imported), length(trkpoints))

  i <- sample(seq_along(trkpoints), 1)  # Pick a random "row".
  check <- recursive_text(trkpoints[i])

  for (n in names(check)) {
    expect_Equal(imported[[n]][i], check[[n]], info = file_path)
  }
})

# ---------------------- #
#          .FIT          #
# ---------------------- #
fit_csv_tool <- function(file_path) {
  # We need the FitCSVTool to be in the parent
  # directory (i.e. in tests dir).
  tool <- file.path("../../.testfiles/FitCSVTool.jar")

  returnifnot(file.exists(tool))(NULL)

  tmp_file <- tempfile("fit_test_", fileext = "")
  cmd <- sprintf(
    "java -jar %s -b %s %s --data record --defn none",
    tool, file_path, tmp_file
  )
  system(cmd, ignore.stdout = TRUE, ignore.stderr = FALSE)

  to_read <- paste0(tmp_file, "_data.csv")
  out <- suppressWarnings(readr::read_csv(to_read))
  out <- out[!is.na(colnames(out))]
  clean_names <- gsub("record\\.", "", colnames(out)) %>%
    make.names %>%
    gsub("^X\\.", "", .)
  colnames(out) <- clean_names
  out
}

test_that("FIT files are parsed correctly", {
  returnifnot(require(readr))()
  file_path <- get_test_file("fit")

  raw <- import_ride(file_path, raw = FALSE) %>% (dplyr::as_data_frame)
  check <- fit_csv_tool(file_path)

  # For other users running these checks, abort due to
  # absence of the FitCSVTool.jar
  returnifnot(!is.null(check))()

  # NA omit consistently.
  complete_rows <- apply(raw, 1, function(row) !any(is.na(row)))

  raw   <- raw[complete_rows, ]
  check <- check[complete_rows, ]

  # Make degrees from semicircles.
  lonlat <- grepl("position", colnames(check))
  check[lonlat] <- lapply(check[lonlat], function(x) x * 180 / 2 ^ 31)
  degree_names <- gsub("semicircles", "degrees", colnames(check[lonlat]))
  colnames(check) <- replace(colnames(check), lonlat, degree_names)

  for (name in names(raw)) {
    check_names <- names(check)
    name_match <- check_names[pmatch(make.names(name), check_names)]
    if (is.na(name_match) || grepl("enhanced", name_match)) next  # Look into this.
    expect_equal(raw[[name]], check[[name_match]], info = file_path)
  }
})

# ---------------------- #
#          .SRM          #
# ---------------------- #
srmcmd_read <- function(file_path) {
  returnifnot(nzchar(Sys.which("srmcmd")))(NULL)
  out <- system2("srmcmd", c("--read", file_path), stdout = TRUE)
  read.delim(text = out, header = TRUE)
}

test_that("SRM files are parsed correctly", {
  file_path <- get_test_file("srm")

  raw   <- import_ride(file_path)
  check <- srmcmd_read(file_path)

  expect_equal(nrow(raw), nrow(check), info = file_path)

  common_cols <- list(
    c("watts", "pwr"),
    c("cad", "cad"),
    c("hr", "hr"),
    c("alt", "ele"),
    c("temp", "temp")
  )

  for (n in common_cols) {
    expect_equal(raw[[n[1]]], check[[n[2]]], info = file_path)
  }
})
