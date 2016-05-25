context("Testing W' balance")

test_that("Algorithm", {
  # Toy case.
  time.s <- seq_len(500)
  power.W <- c(runif(250, 301, 600),  # Work (250-sec).
               runif(250, 50, 299))   # Recovery (250-sec).
  CP <- 300

  # Calculate target by "hand".

  ## Work section.
  Wexp <- numeric(500)
  deltat <- Diff(time.s)  # To illustrate.
  for (i in 2:250) {
    Wexp[i] <- Wexp[i - 1] + (power.W[i] - CP) * deltat[i]
  }
  ## Recovery section.
  DCP <- CP - power.W[-(1:250)]
  tau <- 546 * exp(-0.01 * DCP) + 316
  Wexp[251:500] <- max(Wexp) * exp(-(1:250) / tau)


  expect_equivalent(
    Wbalance(time.s, power.W, CP, smooth = FALSE),
    Wexp / 1000
  )
})
