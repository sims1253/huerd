# Palette posterior sampling (proof of concept)
#
# The Stan-style optimization PoC finds a single best palette (the MAP
# analogue). This module implements the *sampling* side of the Stan
# idea: draw from a Boltzmann posterior
#
#   pi(palette) ∝ exp(beta * Q(palette))
#
# where Q is a differentiable quality (soft-min perceptual separation
# under normal and CVD-simulated vision). Each posterior draw is one
# candidate palette; all package metrics are computed per draw as
# derived quantities, and an interactive chooser re-ranks draws under
# user-supplied metric weights without re-running the sampler.
#
# Stan parallels, deliberately:
#
# * box constraints are handled by Stan's lower/upper transform to
#   unconstrained space (scaled logit), and -- as the Stan Reference
#   Manual's optimization chapter stresses for Laplace approximation --
#   the log-Jacobian of the transform is INCLUDED, which for sampling is
#   not optional: log|dx/du| = log(width) + log(s(1-s)) also keeps the
#   chain off the box boundary in u-space;
# * chains are initialized uniform(-2, 2) on the unconstrained scale,
#   exactly Stan's default initialization;
# * warmup uses Stan's dual-averaging step-size adaptation (Hoffman &
#   Gelman 2014 constants: gamma = 0.05, t0 = 10, kappa = 0.75, target
#   acceptance 0.8) with the same initial-step-size heuristic;
# * divergent transitions (energy error above Stan's ~1000 threshold)
#   are counted and reported;
# * draws are summarized with split-Rhat and an initial-positive-
#   sequence effective sample size, in the spirit of Stan's summaries.
#
# Note: like mixture-model posteriors in Stan, this posterior is
# invariant under permutation of the palette's colors (n! symmetric
# modes). That is harmless here because every derived metric is
# permutation-invariant.

#' Log density and gradient of the palette posterior in unconstrained space
#'
#' The quality Q is a weighted sum of the soft-minimum separation under
#' normal vision and under the three CVD simulations, plus optional
#' lightness-spread and mean-chroma terms. Box bounds use the same OKLAB
#' box as `.run_optimization()`.
#'
#' @param u Numeric vector, unconstrained free-color parameters
#'   (row-major packing of the n_free x 3 logit coordinates).
#' @param initial_colors_oklab Full color matrix (fixed colors constant).
#' @param fixed_mask Logical vector, which rows are fixed.
#' @param beta Inverse temperature; larger values concentrate the
#'   posterior near high-quality palettes.
#' @param quality_weights Named numeric with elements "normal", "cvd",
#'   "lightness_spread", "mean_chroma" (defaults 1, 1, 0, 0). The
#'   soft-min terms are maximized; the aesthetic terms are scaled to
#'   roughly [0, 1]. Additionally accepts `chroma_target` / `l_target`
#'   (numeric targets for mean OKLAB chroma / lightness) and
#'   `target_weight` (default 8): soft quadratic penalties that pull
#'   palettes toward a desired saturation/brightness instead of letting
#'   separation push every color to the gamut edge.
#' @param k Soft-min temperature passed to the objective.
#' @return List with `value` (log density) and `gradient` (wrt u).
#' @noRd
palette_log_density <- function(
  u,
  initial_colors_oklab,
  fixed_mask,
  beta = 25,
  quality_weights = list(normal = 1, cvd = 1, lightness_spread = 0, mean_chroma = 0),
  k = 40
) {
  n_free <- sum(!fixed_mask)
  lb <- c(0.001, -0.4, -0.4)
  ub <- c(0.999, 0.4, 0.4)
  LB <- matrix(rep(lb, n_free), ncol = 3, byrow = TRUE)
  UB <- matrix(rep(ub, n_free), ncol = 3, byrow = TRUE)
  width <- UB - LB

  s <- matrix(stats::plogis(u), ncol = 3, byrow = TRUE)
  x <- LB + width * s

  full <- initial_colors_oklab
  full[!fixed_mask, ] <- x

  qw <- modifyList(
    list(
      normal = 1, cvd = 1, lightness_spread = 0, mean_chroma = 0,
      chroma_target = NULL, l_target = NULL, target_weight = 8
    ),
    quality_weights
  )
  quality <- 0
  dquality <- matrix(0, nrow = nrow(full), ncol = 3)

  if (qw$normal != 0) {
    fg <- cvd_smooth_objective_gradient(full, k = k, cvd_safe = FALSE)
    quality <- quality - qw$normal * fg$value # value is -(soft min)
    dquality <- dquality - qw$normal * fg$gradient
  }
  if (qw$cvd != 0) {
    fg <- cvd_smooth_objective_gradient(full, k = k, cvd_safe = TRUE)
    quality <- quality - qw$cvd * fg$value
    dquality <- dquality - qw$cvd * fg$gradient
  }
  if (qw$lightness_spread != 0) {
    L <- full[, 1]
    spread <- (max(L) - min(L)) / 0.998
    quality <- quality + qw$lightness_spread * spread
    # gradient only through free colors at the extremes
    i_hi <- which.max(L)
    i_lo <- which.min(L)
    if (!fixed_mask[i_hi]) dquality[i_hi, 1] <- dquality[i_hi, 1] + qw$lightness_spread / 0.998
    if (!fixed_mask[i_lo]) dquality[i_lo, 1] <- dquality[i_lo, 1] - qw$lightness_spread / 0.998
  }
  if (qw$mean_chroma != 0) {
    chroma <- sqrt(full[, 2]^2 + full[, 3]^2)
    quality <- quality + qw$mean_chroma * mean(chroma) / 0.4
    grad_chroma <- cbind(0, full[, 2] / chroma, full[, 3] / chroma) / (0.4 * nrow(full))
    grad_chroma[chroma == 0, ] <- 0
    dquality <- dquality + qw$mean_chroma * grad_chroma
  }
  if (!is.null(qw$chroma_target)) {
    # soft saturation target: -w * ((mean chroma - t) / 0.4)^2
    chroma <- sqrt(full[, 2]^2 + full[, 3]^2)
    dev <- (mean(chroma) - qw$chroma_target) / 0.4
    quality <- quality - qw$target_weight * dev^2
    grad_t <- cbind(0, full[, 2] / chroma, full[, 3] / chroma) / nrow(full)
    grad_t[chroma == 0, ] <- 0
    dquality <- dquality - 2 * qw$target_weight * dev / 0.4 * grad_t
  }
  if (!is.null(qw$l_target)) {
    # soft brightness target on mean OKLAB lightness
    dev <- (mean(full[, 1]) - qw$l_target) / 0.998
    quality <- quality - qw$target_weight * dev^2
    dquality[, 1] <- dquality[, 1] -
      2 * qw$target_weight * dev / 0.998 / nrow(full)
  }

  # log density: beta * Q(u) + log |dx/du| (the Stan sampling rule:
  # always include the Jacobian of the constraining transform)
  log_jac <- sum(log(width) + log(s) + log(1 - s))
  value <- beta * quality + log_jac

  gx <- dquality * beta # d quality / d x for free colors
  chain <- width * s * (1 - s) # dx/du, elementwise
  g_free <- gx[!fixed_mask, , drop = FALSE] * chain + 1 - 2 * s
  gradient <- as.vector(t(g_free))

  list(value = value, gradient = gradient)
}

#' Leapfrog integrator for palette HMC
#' @noRd
.leapfrog <- function(lgdl, u, momentum, step, n_steps) {
  g <- lgdl(u)$gradient
  u_new <- u
  p_new <- momentum + 0.5 * step * g
  for (i in seq_len(n_steps)) {
    u_new <- u_new + step * p_new
    lp <- lgdl(u_new)
    g <- lp$gradient
    if (i < n_steps) {
      p_new <- p_new + step * g
    }
  }
  p_new <- p_new + 0.5 * step * g
  list(u = u_new, p = p_new, logp = lp$value, gradient = g)
}

#' Heuristic initial step size (Stan's approach: double/halve until the
#' one-step acceptance probability crosses 0.5)
#' @noRd
.find_initial_step <- function(lgdl, u) {
  step <- 1
  lp0 <- lgdl(u)
  p0 <- stats::rnorm(length(u))
  h0 <- lp0$value - 0.5 * sum(p0^2)
  lf <- .leapfrog(lgdl, u, p0, step, 1)
  h1 <- lf$logp - 0.5 * sum(lf$p^2)
  ratio <- exp(h1 - h0)
  direction <- ifelse(is.finite(ratio) && ratio > 0.5, 1, -1)
  for (i in 1:50) {
    step <- step * 2^direction
    lf <- .leapfrog(lgdl, u, p0, step, 1)
    h1 <- lf$logp - 0.5 * sum(lf$p^2)
    ratio <- exp(h1 - h0)
    if (direction == 1 && !(is.finite(ratio) && ratio > 0.5)) break
    if (direction == -1 && (is.finite(ratio) && ratio > 0.5)) break
  }
  step
}

#' Sample palettes from the Boltzmann posterior with adaptive HMC
#'
#' @param initial_colors_oklab Full initial color matrix in OKLAB space.
#' @param fixed_mask Logical vector of fixed colors.
#' @param chains Number of chains.
#' @param warmup Warmup iterations per chain (dual-averaging adapts the
#'   step size during warmup).
#' @param iter Sampling iterations per chain after warmup.
#' @param beta,quality_weights,k Passed to [palette_log_density()].
#' @param sim_time Total integration time per trajectory; the number of
#'   leapfrog steps is randomized around sim_time / step.
#' @param max_steps Cap on leapfrog steps per trajectory (the static-HMC
#'   analogue of Stan's max treedepth; without it a small adapted step
#'   size makes trajectories -- and runtime -- explode).
#' @param target_accept Target acceptance statistic for adaptation
#'   (Stan's `adapt_delta`, default 0.8).
#' @param seed Integer seed.
#' @return List with `draws` (iter x chains x n_free x 3 OKLAB array),
#'   `unconstrained` (same shape on the u scale), `diagnostics` (step
#'   size, acceptance, divergences per chain), and `seed`.
#' @noRd
sample_palette_posterior <- function(
  initial_colors_oklab,
  fixed_mask,
  chains = 4,
  warmup = 300,
  iter = 300,
  beta = 25,
  quality_weights = list(
    normal = 1, cvd = 1, lightness_spread = 0, mean_chroma = 0
  ),
  k = 40,
  sim_time = 1,
  max_steps = 64L,
  target_accept = 0.8,
  seed = 1234
) {
  n_free <- sum(!fixed_mask)
  n_total <- nrow(initial_colors_oklab)
  set.seed(seed)

  lgdl <- function(u) {
    palette_log_density(
      u,
      initial_colors_oklab,
      fixed_mask,
      beta = beta,
      quality_weights = quality_weights,
      k = k
    )
  }

  draws <- array(NA_real_, dim = c(iter, chains, n_free, 3))
  u_draws <- array(NA_real_, dim = c(iter, chains, n_free, 3))
  diagnostics <- list()

  lb <- c(0.001, -0.4, -0.4)
  ub <- c(0.999, 0.4, 0.4)
  LB <- matrix(rep(lb, n_free), ncol = 3, byrow = TRUE)
  UB <- matrix(rep(ub, n_free), ncol = 3, byrow = TRUE)
  width <- UB - LB

  for (ch in seq_len(chains)) {
    # Stan default initialization: uniform(-2, 2) on the unconstrained scale
    u <- stats::runif(3 * n_free, -2, 2)
    lp <- lgdl(u)
    h_bar <- 0
    log_step_bar <- 0
    mu <- log(10 * .find_initial_step(lgdl, u))
    step <- exp(mu)
    gamma <- 0.05
    t0 <- 10
    kappa <- 0.75
    accept_sum <- 0
    divergences <- 0
    steps_sum <- 0

    total <- warmup + iter
    for (m in seq_len(total)) {
      sampling_phase <- m > warmup
      p0 <- stats::rnorm(length(u))
      h0 <- lp$value - 0.5 * sum(p0^2)
      n_steps <- min(
        max_steps,
        max(1L, round(sim_time / step * stats::runif(1, 0.7, 1.3)))
      )
      lf <- .leapfrog(lgdl, u, p0, step, n_steps)
      h1 <- lf$logp - 0.5 * sum(lf$p^2)
      delta_h <- h1 - h0
      accept_prob <- ifelse(delta_h > 0, 1, exp(delta_h))
      accept_prob <- min(1, accept_prob)
      divergent <- !is.finite(delta_h) || delta_h < -1000

      if (divergent) {
        divergences <- divergences + 1
        accept_prob <- 0
        accepted <- FALSE
      } else if (stats::runif(1) < accept_prob) {
        u <- lf$u
        lp <- list(value = lf$logp, gradient = lf$gradient)
        accepted <- TRUE
      }

      if (!sampling_phase) {
        # Stan's dual averaging
        w <- 1 / (m + t0)
        h_bar <- (1 - w) * h_bar + w * (target_accept - accept_prob)
        log_step <- mu - sqrt(m) / gamma * h_bar
        step <- exp(log_step)
        eta <- m^-kappa
        log_step_bar <- eta * log_step + (1 - eta) * log_step_bar
      } else {
        step <- exp(log_step_bar)
        accept_sum <- accept_sum + accept_prob
        steps_sum <- steps_sum + n_steps
        u_draw_iter <- (m - warmup)
        s <- matrix(stats::plogis(u), ncol = 3, byrow = TRUE)
        x <- LB + width * s
        u_draws[u_draw_iter, ch, , ] <- matrix(u, ncol = 3, byrow = TRUE)
        draws[u_draw_iter, ch, , ] <- x
      }
    }
    diagnostics[[ch]] <- list(
      step_size = exp(log_step_bar),
      mean_accept = accept_sum / iter,
      mean_steps = steps_sum / iter,
      divergences = divergences
    )
  }

  list(
    draws = draws,
    unconstrained = u_draws,
    diagnostics = diagnostics,
    fixed_mask = fixed_mask,
    initial_colors_oklab = initial_colors_oklab,
    seed = seed
  )
}

#' Derived metrics for every posterior draw
#'
#' Computes, per draw: the hard minimax metrics from the package
#' (perceptual and CVD-safe minimum distance), per-condition CVD
#' minima (via the verified differentiable chain), mean chroma, and
#' lightness spread. These are the "derived quantities" the chooser
#' weights.
#'
#' @param fit Output of [sample_palette_posterior()].
#' @return Data frame with one row per draw and chain, columns
#'   `.chain`, `.draw`, `palette` (list column of n x 3 matrices), and
#'   one column per metric.
#' @noRd
palette_draw_metrics <- function(fit) {
  dm <- dim(fit$draws)
  n_free <- dm[3]
  mats <- .cvd_condition_matrices()
  rows <- vector("list", dm[1] * dm[2])
  idx <- 0L
  for (ch in seq_len(dm[2])) {
    for (d in seq_len(dm[1])) {
      idx <- idx + 1L
      full <- fit$initial_colors_oklab
      full[!fit$fixed_mask, ] <- fit$draws[d, ch, , ]
      # per-condition CVD minima via the verified chain
      root <- pmax(full %*% t(.STAN_LBFGS_LMS_FROM_LAB), 0)
      lin <- root^3 %*% t(.STAN_LBFGS_LIN_FROM_LMS)
      gam <- pmin(pmax(lin, 0), 1)
      cvd_mins <- vapply(
        c("deutan", "protan", "tritan"),
        function(nm) {
          sim <- pmin(pmax(gam %*% t(mats[[nm]]), 0), 1)
          labc <- (pmax(sim %*% t(.STAN_LBFGS_LMS_FROM_LIN), 0)^(1 / 3)) %*%
            t(.STAN_LBFGS_LAB_FROM_LMS)
          min(as.matrix(stats::dist(labc))[upper.tri(diag(nrow(labc)))])
        },
        numeric(1)
      )
      chroma <- sqrt(full[, 2]^2 + full[, 3]^2)
      rows[[idx]] <- data.frame(
        .chain = ch,
        .draw = d,
        palette = I(list(full)),
        min_dist = objective_min_perceptual_dist(full),
        cvd_safe = objective_min_cvd_safe_dist(full),
        min_deutan = cvd_mins[["deutan"]],
        min_protan = cvd_mins[["protan"]],
        min_tritan = cvd_mins[["tritan"]],
        mean_chroma = mean(chroma),
        lightness_spread = diff(range(full[, 1])),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

#' Rank draws under user-supplied metric weights
#'
#' Metrics are min-max scaled across draws (higher is better for all
#' listed metrics) and combined into a single weighted score. The
#' ranking never re-runs the sampler: it is a pure post-processing of
#' the derived quantities, so interactive weight changes are instant.
#'
#' @param metrics Data frame from [palette_draw_metrics()].
#' @param weights Named numeric vector over metric columns, e.g.
#'   `c(cvd_safe = 3, min_dist = 1, mean_chroma = 0.5)`.
#' @param metrics_cols Character vector of metric columns to scale.
#' @return The metrics data frame with added `score` and row order of
#'   decreasing score.
#' @noRd
rank_palette_draws <- function(metrics, weights, metrics_cols = intersect(
  names(weights),
  c("min_dist", "cvd_safe", "min_deutan", "min_protan", "min_tritan",
    "mean_chroma", "lightness_spread")
)) {
  z <- as.data.frame(lapply(metrics[metrics_cols], function(x) {
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) < 1e-12) rep(0.5, length(x)) else (x - rng[1]) / diff(rng)
  }))
  score <- rep(0, nrow(z))
  for (nm in names(weights)) {
    if (nm %in% metrics_cols) {
      score <- score + weights[[nm]] * z[[nm]]
    }
  }
  metrics$score <- score
  metrics[order(-metrics$score), ]
}

#' Effective sample size via Geyer's initial positive sequence
#' @noRd
.ess_initial_positive <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)
  x <- x - mean(x)
  rho <- as.numeric(stats::acf(x, lag.max = floor(n / 2), plot = FALSE)$acf)
  # pair sums rho[2k] + rho[2k+1] must be positive
  tau <- 1
  k <- 1
  while (2 * k + 1 <= n) {
    pair <- rho[2 * k] + rho[2 * k + 1]
    if (is.na(pair) || pair <= 0) break
    tau <- tau + 2 * pair
    k <- k + 1
  }
  n / tau
}

#' Split-Rhat for one scalar quantity across chains
#' @noRd
.split_rhat <- function(x_matrix) {
  # x_matrix: iter x chains
  half <- floor(nrow(x_matrix) / 2)
  if (half < 2) return(NA_real_)
  xs <- rbind(x_matrix[seq_len(half), ], x_matrix[half + seq_len(half), ])
  m <- ncol(xs)
  chain_means <- colMeans(xs)
  chain_vars <- apply(xs, 2, stats::var)
  W <- mean(chain_vars)
  B <- half * stats::var(chain_means)
  if (W <= 0) return(NA_real_)
  sqrt((half - 1) / half + B / (half * W))
}

#' Stan-style summary of the palette posterior
#'
#' @param fit Output of [sample_palette_posterior()].
#' @param metrics Data frame from [palette_draw_metrics()].
#' @return Data frame with mean, sd, ESS, split-Rhat per metric.
#' @noRd
summarize_palette_posterior <- function(fit, metrics) {
  metric_cols <- setdiff(
    names(metrics),
    c(".chain", ".draw", "palette", "score")
  )
  out <- do.call(rbind, lapply(metric_cols, function(nm) {
    wide <- vapply(
      seq_len(dim(fit$draws)[2]),
      function(ch) metrics[metrics$.chain == ch, nm],
      numeric(dim(fit$draws)[1])
    )
    flat <- as.vector(wide)
    data.frame(
      metric = nm,
      mean = mean(flat),
      sd = stats::sd(flat),
      ess = .ess_initial_positive(flat),
      rhat = .split_rhat(wide),
      row.names = NULL
    )
  }))
  divergences <- sum(vapply(fit$diagnostics, function(d) d$divergences, numeric(1)))
  attr(out, "divergences") <- divergences
  out
}
