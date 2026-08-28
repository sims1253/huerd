test_that("soft-min objective reproduces the package's hard minimax metric", {
  set.seed(1)
  # In-gamut colors: the differentiable chain agrees with the farver +
  # colorspace pipeline the package's metric uses (absolute tolerance:
  # the values are small, so waldo's default relative scale is too loose)
  lab <- farver::convert_colour(
    255 * matrix(runif(12), ncol = 3), "rgb", "oklab"
  )
  fg <- cvd_smooth_objective_gradient(lab, k = 60, cvd_safe = TRUE)
  expect_lt(
    abs(fg$min_distance - objective_min_cvd_safe_dist(lab)),
    5e-5
  )

  fg2 <- cvd_smooth_objective_gradient(lab, k = 60, cvd_safe = FALSE)
  expect_equal(fg2$min_distance, objective_min_perceptual_dist(lab))
})

test_that("analytic gradient matches finite differences", {
  # Fixed, well-separated colors pulled toward mid gray keep every
  # gamut/simulation clip inactive, so the objective is smooth and the
  # central difference is exact. (At saturated colors the clips are
  # active by design: values clip, gradients are gated, and a central
  # difference across that kink legitimately disagrees.) Note farver's
  # "rgb" space is 0-255.
  corners <- matrix(
    c(
      0.05, 0.05, 0.05, 0.95, 0.95, 0.95, 0.9, 0.05, 0.05, 0.05, 0.9, 0.9,
      0.05, 0.9, 0.05, 0.05, 0.05, 0.9
    ),
    ncol = 3, byrow = TRUE
  )
  lab <- farver::convert_colour(255 * (0.40 * corners + 0.60 * 0.5), "rgb", "oklab")
  expect_gt(min(stats::dist(lab)), 0.08)
  for (cvd_safe in c(TRUE, FALSE)) {
    fg <- cvd_smooth_objective_gradient(lab, k = 20, cvd_safe = cvd_safe,
      gamut_penalty = 1e3)
    h <- 1e-6
    for (i in 1:4) {
      for (cc in 1:3) {
        lab_p <- lab; lab_p[i, cc] <- lab_p[i, cc] + h
        lab_m <- lab; lab_m[i, cc] <- lab_m[i, cc] - h
        fd <- (cvd_smooth_objective_gradient(lab_p, k = 20,
          cvd_safe = cvd_safe, gamut_penalty = 1e3)$value -
          cvd_smooth_objective_gradient(lab_m, k = 20,
            cvd_safe = cvd_safe, gamut_penalty = 1e3)$value) / (2 * h)
        expect_lt(abs(fg$gradient[i, cc] - fd), 1e-4 + 0.01 * abs(fd))
      }
    }
  }
})

test_that("two-loop recursion approximates the inverse Hessian", {
  # On a quadratic with Hessian H, L-BFGS with a full history converges
  # to the optimum (classic sanity check)
  H <- rbind(c(4, 1, 0), c(1, 3, 0.5), c(0, 0.5, 2))
  b <- c(1, -1, 2)
  f_grad <- function(x) list(
    value = as.numeric(0.5 * x %*% H %*% x - b %*% x),
    gradient = as.numeric(x %*% H - b)
  )
  res <- stan_lbfgs(f_grad, c(3, -2, 5), max_iterations = 50L)
  expect_true(res$converged)
  expect_equal(res$par, solve(H, b), tolerance = 1e-6)
})

test_that("stan_lbfgs respects box constraints and improves the objective", {
  set.seed(3)
  init <- farver::convert_colour(255 * matrix(runif(9), ncol = 3), "rgb", "oklab")
  res <- optimize_colors_stan_lbfgs(init, rep(FALSE, 3), 300L,
    cvd_safe = FALSE, k = 30, gamut_penalty = 1e3)
  expect_true(all(res$palette >= cbind(rep(0.001, 3), rep(-0.4, 3), rep(-0.4, 3))))
  expect_true(all(res$palette <= cbind(rep(0.999, 3), rep(0.4, 3), rep(0.4, 3))))
  expect_gt(
    objective_min_perceptual_dist(res$palette),
    objective_min_perceptual_dist(init)
  )
  expect_gte(as.integer(res$details$iterations), 0)
})
