test_that("palette log density gradient matches finite differences", {
  set.seed(1)
  init <- farver::convert_colour(255 * matrix(runif(9), ncol = 3), "rgb", "oklab")
  fixed <- rep(FALSE, 3)
  lg <- function(u) palette_log_density(u, init, fixed, beta = 25)
  u <- c(0.2, -0.5, 1.1, 0.8, -1.4, 0.3, -0.9, 0.6, 1.5)
  fg <- lg(u)
  expect_false(is.na(fg$value))
  h <- 1e-6
  for (idx in c(1, 4, 7)) {
    up <- u; up[idx] <- up[idx] + h
    um <- u; um[idx] <- um[idx] - h
    fd <- (lg(up)$value - lg(um)$value) / (2 * h)
    expect_lt(abs(fg$gradient[idx] - fd), 1e-4 + 0.01 * abs(fd))
  }
})

test_that("beta = 0 posterior is uniform over the OKLAB box", {
  # With beta = 0 the density is the Jacobian of the logit transform,
  # so x is uniform: mean at box centre, sd = width / sqrt(12)
  set.seed(2)
  init <- farver::convert_colour(255 * matrix(runif(9), ncol = 3), "rgb", "oklab")
  fit <- sample_palette_posterior(
    init, rep(FALSE, 3),
    chains = 1, warmup = 100, iter = 250, beta = 0, seed = 5
  )
  x <- fit$draws[, 1, , ]
  expect_equal(mean(x[, , 1]), 0.5, tolerance = 0.05)
  expect_equal(sd(x[, , 1]), 0.998 / sqrt(12), tolerance = 0.03)
  expect_equal(sd(x[, , 3]), 0.8 / sqrt(12), tolerance = 0.03)
})

test_that("palette posterior concentrates on better palettes at higher beta", {
  set.seed(3)
  init <- farver::convert_colour(
    255 * matrix(c(0.1, 0.1, 0.1, 0.9, 0.9, 0.9, 0.9, 0.1, 0.1),
      ncol = 3, byrow = TRUE),
    "rgb", "oklab"
  )
  low <- sample_palette_posterior(
    init, rep(FALSE, 3), chains = 1, warmup = 80, iter = 150,
    beta = 5, seed = 11
  )
  high <- sample_palette_posterior(
    init, rep(FALSE, 3), chains = 1, warmup = 80, iter = 150,
    beta = 40, seed = 11
  )
  m_low <- mean(vapply(
    seq_len(dim(low$draws)[1]),
    function(d) objective_min_perceptual_dist(low$draws[d, 1, , ]),
    numeric(1)
  ))
  m_high <- mean(vapply(
    seq_len(dim(high$draws)[1]),
    function(d) objective_min_perceptual_dist(high$draws[d, 1, , ]),
    numeric(1)
  ))
  expect_gt(m_high, m_low)
})

test_that("draw metrics and weighted ranking work", {
  set.seed(4)
  init <- farver::convert_colour(255 * matrix(runif(9), ncol = 3), "rgb", "oklab")
  fit <- sample_palette_posterior(
    init, rep(FALSE, 3), chains = 2, warmup = 80, iter = 100,
    beta = 20, seed = 3
  )
  met <- palette_draw_metrics(fit)
  expect_equal(nrow(met), 200)
  expect_true(all(met$min_dist >= 0))
  expect_true(all(met$mean_chroma >= 0))

  ranked <- rank_palette_draws(met, c(cvd_safe = 2, min_dist = 1))
  expect_true(all(diff(ranked$score) <= 1e-12))
  # with mixed weights the top draw maximizes the combined score, so its
  # cvd safety is high but not necessarily the maximum ...
  expect_gt(ranked$cvd_safe[1], stats::quantile(met$cvd_safe, 0.75))
  # ... while a pure weight must select the maximum exactly
  ranked_cvd_only <- rank_palette_draws(met, c(cvd_safe = 1))
  expect_equal(ranked_cvd_only$cvd_safe[1], max(met$cvd_safe))

  summ <- summarize_palette_posterior(fit, met)
  expect_true(all(c("mean", "ess", "rhat") %in% names(summ)))
  expect_true(all(summ$rhat < 1.2 | is.na(summ$rhat)))
})

test_that("ESS estimator behaves on known series", {
  set.seed(5)
  iid <- rnorm(4000)
  expect_gt(.ess_initial_positive(iid), 3000)
  # AR(1) with rho = 0.9: ESS/n ~ 1 - rho ~ 0.19
  ar <- stats::arima.sim(list(ar = 0.9), n = 4000)
  ess <- .ess_initial_positive(as.numeric(ar))
  expect_lt(ess, 1500)
  expect_gt(ess, 200)
})

test_that("split-Rhat detects unsampled chains vs identical chains", {
  set.seed(6)
  a <- matrix(rnorm(500), ncol = 2)
  expect_lt(.split_rhat(a), 1.05)
  b <- a
  b[, 2] <- b[, 2] + 10
  expect_gt(.split_rhat(b), 1.5)
})
