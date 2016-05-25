context("Testing rolling means")

test_that("ema weights", {
  ema_weights_R <- function(x) {
    alpha <- 2 / (x + 1)
    i <- 1:x
    sm <- sum((alpha * (1 - alpha) ^ (1 - i)))
    (alpha * (1 - alpha) ^ (1 - i)) / sm
  }
  expect_equal(EMA_WEIGHTS(10), ema_weights_R(10))
})

test_that("roll_mean: na.rm = FALSE, ema = FALSE", {
  win <- sample(30:60, 1)
  x <- runif(100, 100, 500)

  roll_test <- roll_mean(x, win, na.rm = FALSE, ema = FALSE)

  expect_equal(
    mean(x[1:win], na.rm = FALSE),
    roll_test[win]
  )

  # 10 random windows to check; ignoring initial zeros.
  i  <- seq_along(x)[-(1:win)] %>% sample(10)

  roll_checks <- purrr::map_dbl(i, ~mean(x[seq(.x - win + 1, .x)], na.rm = TRUE))

  expect_equal(
    roll_checks,
    roll_test[i]
  )
})

test_that("roll_mean: na.rm = FALSE, ema = TRUE", {
  win <- sample(30:60, 1)
  x <- runif(100, 100, 500)
  weights <- EMA_WEIGHTS(win)

  roll_test <- roll_mean(x, win, na.rm = FALSE, ema = TRUE)

  expect_equal(
    sum(x[1:win] * weights),
    roll_test[win]
  )

  # 10 random windows to check; ignoring initial zeros.
  i  <- seq_along(x)[-(1:win)] %>% sample(10)

  roll_checks <- purrr::map_dbl(i, ~sum(x[seq(.x - win + 1, .x)] * weights))

  expect_equal(
    roll_checks,
    roll_test[i]
  )
})

test_that("roll_mean: na.rm = TRUE, ema = FALSE", {
  win <- sample(30:60, 1)
  x <- runif(100, 100, 500)

  # Insert some NAs.
  x[sample(seq_along(x), 20)] <- NA

  roll_test <- roll_mean(x, win, na.rm = TRUE, ema = FALSE)

  expect_equal(
    mean(x[1:win], na.rm = TRUE),
    roll_test[win]
  )

  # 10 random windows to check; ignoring initial zeros.
  i  <- seq_along(x)[-(1:win)] %>% sample(10)

  roll_checks <- purrr::map_dbl(i, ~mean(x[seq(.x - win + 1, .x)], na.rm = TRUE))

  expect_equal(
    roll_checks,
    roll_test[i]
  )
})

test_that("roll_mean: na.rm = TRUE, ema = TRUE", {
  win <- sample(30:60, 1)
  x <- runif(100, 100, 500)

  # Insert some NAs.
  x[sample(seq_along(x), 20)] <- NA

  ema_mean <- function(x, na.rm = TRUE) {
    len <- length(x[!is.na(x)])
    sum(x[!is.na(x)] * EMA_WEIGHTS(len))
  }

  roll_test <- roll_mean(x, win, na.rm = TRUE, ema = TRUE)

  expect_equal(
    ema_mean(x[1:win]),
    roll_test[win]
  )

  # 10 random windows to check; ignoring initial zeros.
  i  <- seq_along(x)[-(1:win)] %>% sample(10)

  roll_checks <- purrr::map_dbl(i, ~ema_mean(x[seq(.x - win + 1, .x)]))

  expect_equal(
    roll_checks,
    roll_test[i]
  )
})

# Time-window rolling ----------------------------------------------------
test_that("roll_mean.time: na.rm = FALSE", {

  # Create awkwardly sampled data.
  time <- sort(sample(1:1e3, 500))
  win <- sample(30:60, 1)
  x <- runif(500, 100, 500)

  roll_test <- roll_mean.time(x, time, win, na.rm = FALSE)

  first_i <- rev(which(time <= win))[1]

  expect_equal(
    mean(x[1:first_i], na.rm = FALSE),
    roll_test[first_i]
  )

  roll_checks <- replicate(100, {
    i <- sample(time[time > win], 1) # Don't search leading zeros.
    this_win <- time > (i - win) & time <= i
    all.equal(
      mean(x[this_win], na.rm = FALSE),
      roll_test[match(i, time)]
    )
  })

  expect_true(all(roll_checks))
})

test_that("roll_mean.time: na.rm = TRUE", {

  # Create awkwardly sampled data.
  time <- sort(sample(1:1e3, 500))
  win <- sample(30:60, 1)
  x <- runif(500, 100, 500)

  # Add NAs
  x[sample(seq_along(x), 50)] <- NA

  roll_test <- roll_mean.time(x, time, win, na.rm = TRUE)

  first_i <- rev(which(time <= win))[1]

  expect_equal(
    mean(x[1:first_i], na.rm = TRUE),
    roll_test[first_i]
  )

  roll_checks <- replicate(100, {
    i <- sample(time[time > win], 1) # Don't search leading zeros.
    this_win <- time > (i - win) & time <= i
    all.equal(
      mean(x[this_win], na.rm = TRUE),
      roll_test[match(i, time)]
    )
  })

  expect_true(all(roll_checks))
})
