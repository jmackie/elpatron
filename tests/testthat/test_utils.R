context("Testing Rcpp utils")

test_that("Diff", {
  checks <- replicate(1e3, {
    x <- runif(100, 100, 500)
    all.equal(diff(x), Diff(x)[-1])
  })
  expect_true(all(checks))

  # With some NAs.
  x <- runif(100, 100, 500)
  x[sample(seq_along(x), 10)] <- NA

  expect_equal(diff(x), Diff(x)[-1])

})

test_that("Cumsum", {
  checks <- replicate(1e3, {
    x <- runif(100, 100, 500)
    all.equal(cumsum(x), Cumsum(x))
  })
  expect_true(all(checks))

  # Check *treatment* of NAs.
  x <- runif(100, 100, 500)
  x[sample(seq_along(x), 10)] <- NA
  x[1] <- rnorm(1, 300, 50)  # Can't start with an NA.

  inset <- .Primitive("[<-")

  expect_equal(
    x %>% inset(is.na(.), 0) %>% cumsum,
    Cumsum(x)
  )
})
