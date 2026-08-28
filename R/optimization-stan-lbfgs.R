# Differentiable CVD-safe objective + Stan-style L-BFGS palette optimizer
# (proof of concept)
#
# The existing minimax objective (`objective_min_cvd_safe_dist`) is a hard
# minimum over pairwise OKLAB distances after simulating three CVD
# conditions, which is non-smooth, so it can only be used with
# derivative-free optimizers (COBYLA, Nelder-Mead, SANN). The existing
# gradient path (`objective_smooth_repulsion` / `objective_smooth_logsumexp`)
# is smooth but ignores the CVD simulations entirely.
#
# This module closes that gap the "Stan way": the whole pipeline
#
#   OKLAB -> (cube) LMS -> linear sRGB -> 3x3 CVD matrix -> clamp ->
#   (cube root) LMS -> OKLAB -> pairwise distance
#
# is differentiable (the colorspaces `deutan`/`protan`/`tritan`
# simulations are fixed Machado et al. (2009) matrices in linear RGB),
# so a log-sum-exp soft-minimum over all pairwise distances across CVD
# conditions has an analytic gradient, computed here by a hand-rolled
# reverse-mode pass over the vectorized forward computation.

#' Ottosson OKLAB <-> LMS / linear-sRGB transform matrices
#' @noRd
.STAN_LBFGS_LMS_FROM_LAB <- rbind(
  c(1, 0.3963377774, 0.2158037573),
  c(1, -0.1055613458, -0.0638541728),
  c(1, -0.0894841775, -1.2914855480)
)
.STAN_LBFGS_LIN_FROM_LMS <- rbind(
  c(+4.0767416621, -3.3077115913, +0.2309699292),
  c(-1.2684380046, +2.6097574011, -0.3413193965),
  c(-0.0041960863, -0.7034186147, +1.7076147010)
)
.STAN_LBFGS_LAB_FROM_LMS <- solve(.STAN_LBFGS_LMS_FROM_LAB)
.STAN_LBFGS_LMS_FROM_LIN <- solve(.STAN_LBFGS_LIN_FROM_LMS)

#' CVD simulation matrices (linear RGB), extracted from colorspace
#'
#' `colorspace::deutan()` & co. apply a fixed severity-1 matrix per
#' condition in linear RGB space (Machado, Oliveira & Fernandes 2009),
#' so they can be composed into the differentiable pipeline.
#' @noRd
.cvd_condition_matrices <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <- list(
        normal = diag(3),
        deutan = colorspace::interpolate_cvd_transform(
          getFromNamespace("deutanomaly_cvd", "colorspace"), 1
        ),
        protan = colorspace::interpolate_cvd_transform(
          getFromNamespace("protanomaly_cvd", "colorspace"), 1
        ),
        tritan = colorspace::interpolate_cvd_transform(
          getFromNamespace("tritanomaly_cvd", "colorspace"), 1
        )
      )
    }
    cached
  }
})

#' Scatter-add pair contributions back onto color rows
#'
#' Like `rowsum()` but always returns exactly `n` rows, zero-filled for
#' colors that never appear in `idx` (rowsum drops empty groups).
#' @noRd
.splat_rows <- function(U, idx, n) {
  out <- matrix(0, nrow = n, ncol = ncol(U))
  if (nrow(U) > 0) {
    rs <- rowsum(U, group = idx, reorder = FALSE)
    out[as.integer(rownames(rs)), ] <- rs
  }
  out
}

#' Value and analytic gradient of the smooth CVD-safe soft-min objective
#'
#' Objective (to minimize): `(1/k) * logsumexp(-k * d_pq)` over all
#' pairwise OKLAB distances `d_pq` of the palette after each CVD
#' simulation (or plain OKLAB distances when `cvd_safe = FALSE`, which
#' is exactly what `objective_min_perceptual_dist()` measures). As
#' `k -> Inf` this converges to the hard worst-case minimum distance.
#'
#' Hard constraints are handled the Stan way: instead of clamping (which
#' zeroes gradients exactly where the optimum lives, on the gamut
#' boundary), out-of-gamut linear-RGB values incur quadratic penalties
#' and the back-transform uses signed cube roots, so the gradient stays
#' alive through the boundary. A further quadratic penalty keeps the
#' colors in the region where the OKLAB -> LMS transform is valid
#' (non-negative pre-root LMS).
#'
#' @param colors_oklab Matrix of colors in OKLAB space (n x 3).
#' @param k Soft-min temperature; larger values track the hard minimum
#'   more closely at the cost of a stiffer landscape.
#' @param cvd_safe Logical; use the three CVD simulations (as the
#'   package's minimax objective does) or plain OKLAB distances.
#' @param penalty Weight of the out-of-validity / out-of-gamut
#'   penalties.
#' @return List with `value`, `gradient` (n x 3 matrix), `min_distance`
#'   (hard minimum across conditions), and `n_conditions`.
#' @noRd
cvd_smooth_objective_gradient <- function(
  colors_oklab,
  k = 30,
  cvd_safe = TRUE,
  penalty = 1e4,
  gamut_penalty = 0
) {
  lab <- colors_oklab
  n <- nrow(lab)

  ut <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
  i_idx <- ut[, 1]
  j_idx <- ut[, 2]
  P <- length(i_idx)

  softmin_from_labc_list <- function(labc_list) {
    # returns value (soft min), weights, dists
    C <- length(labc_list)
    dists <- matrix(0, nrow = P, ncol = C)
    for (c in seq_len(C)) {
      dm <- as.matrix(stats::dist(labc_list[[c]]))
      dists[, c] <- dm[upper.tri(dm)]
    }
    z <- -k * dists
    zmax <- max(z)
    lse <- zmax + log(sum(exp(z - zmax)))
    w <- exp(z - zmax)
    w <- w / sum(w)
    list(dists = dists, zmax = zmax, soft_min = lse / k, w = w)
    # note: soft_min here equals -(soft minimum of the distances); the
    # objective to MINIMIZE is the negated soft minimum, i.e. maximize
    # the minimum distance
  }
  pair_scatter <- function(labc, wc) {
    # Extract distances in the same pair order as i_idx/j_idx (column-
    # major upper triangle). Note stats::dist() enumerates the lower
    # triangle, whose order only coincides with this for n <= 3.
    dm <- as.matrix(stats::dist(labc))
    d <- dm[upper.tri(dm)]
    diff_ij <- labc[i_idx, , drop = FALSE] - labc[j_idx, , drop = FALSE]
    u <- if (P > 0) diff_ij / d else diff_ij * 0
    u[!is.finite(u)] <- 0
    # dF/dd_pq = -w_pq for the soft minimum F = (1/k) LSE(-k d)
    U <- -u * wc
    .splat_rows(U, i_idx, n) - .splat_rows(U, j_idx, n)
  }
  if (!cvd_safe) {
    # Faithful smooth surrogate of objective_min_perceptual_dist():
    # plain OKLAB distances, no color-space transform at all.
    sm <- softmin_from_labc_list(list(lab))
    gradient <- pair_scatter(lab, sm$w[, 1])
    return(list(
      value = sm$soft_min,
      gradient = gradient,
      min_distance = min(sm$dists),
      neg_soft_min = sm$soft_min,
      n_conditions = 1L
    ))
  }

  mats <- .cvd_condition_matrices()[c("deutan", "protan", "tritan")]
  C <- length(mats)

  # Forward pass, shared prefix: lab -> rooted LMS -> linear RGB
  root_raw <- lab %*% t(.STAN_LBFGS_LMS_FROM_LAB)
  neg_root <- pmin(root_raw, 0)
  root <- pmax(root_raw, 0)
  lms <- root^3
  lin <- lms %*% t(.STAN_LBFGS_LIN_FROM_LMS)

  # Gamut handling, mirroring the package's measurement pipeline: the
  # original color is clipped into the sRGB gamut (as farver's oklab ->
  # rgb conversion does), then simulated (colorspace's fixed matrices in
  # linear RGB) and the result clipped again. Values follow that
  # geometry; gradients pass through both clips as if they were the
  # identity (straight-through estimator), which keeps them alive on the
  # gamut boundary where the optimum lives.
  gam_raw <- lin
  gam <- pmin(pmax(gam_raw, 0), 1)
  gam_mask <- gam_raw > 0 & gam_raw < 1
  gam_viol <- pmin(gam_raw, 0) + pmax(gam_raw - 1, 0)
  labc_list <- list()
  rootc_list <- list()
  mask_list <- list()
  lmsc_mask_list <- list()
  sim_viol_list <- list()
  for (c in seq_len(C)) {
    sim_raw <- gam %*% t(mats[[c]])
    mask <- sim_raw > 0 & sim_raw < 1
    sim <- pmin(pmax(sim_raw, 0), 1)
    sim_viol_list[[c]] <- pmin(sim_raw, 0) + pmax(sim_raw - 1, 0)
    lmsc_raw <- sim %*% t(.STAN_LBFGS_LMS_FROM_LIN)
    # Negative cone responses (possible even in gamut) clamp to zero in
    # the value; the gradient must be gated accordingly
    lmsc_mask_list[[c]] <- lmsc_raw > 0
    rootc <- pmax(lmsc_raw, 0)^(1 / 3)
    labc_list[[c]] <- rootc %*% t(.STAN_LBFGS_LAB_FROM_LMS)
    rootc_list[[c]] <- rootc
    mask_list[[c]] <- mask
  }

  sm <- softmin_from_labc_list(labc_list)
  value <- sm$soft_min + penalty * sum(neg_root^2) +
    gamut_penalty * (sum(gam_viol^2) +
      sum(Reduce(`+`, lapply(sim_viol_list, function(v) v^2))))

  # Reverse pass
  dlin <- matrix(0, nrow = n, ncol = 3)
  for (c in seq_len(C)) {
    Gc <- pair_scatter(labc_list[[c]], sm$w[, c])
    drootc <- Gc %*% .STAN_LBFGS_LAB_FROM_LMS
    dlmsc <- drootc / (3 * pmax(rootc_list[[c]], 1e-12)^2) * lmsc_mask_list[[c]]
    dsimc <- (dlmsc %*% .STAN_LBFGS_LMS_FROM_LIN) * mask_list[[c]] +
      2 * gamut_penalty * sim_viol_list[[c]]
    dlin <- dlin + (dsimc %*% mats[[c]]) * gam_mask
  }
  dlin <- dlin + 2 * gamut_penalty * gam_viol
  dlms <- dlin %*% .STAN_LBFGS_LIN_FROM_LMS
  droot <- dlms * 3 * root^2 + 2 * penalty * neg_root
  gradient <- droot %*% .STAN_LBFGS_LMS_FROM_LAB

  list(
    value = value,
    gradient = gradient,
    min_distance = min(sm$dists),
    neg_soft_min = sm$soft_min,
    n_conditions = C
  )
}

#' Optimize a palette with the Stan-style L-BFGS on the smooth CVD objective
#'
#' Unlike `optimize_colors_lbfgs()` (NLopt L-BFGS on a repulsion
#' surrogate that ignores CVD), this optimizes the same quantity the
#' package's minimax objective measures, through a differentiable
#' soft-minimum. Box constraints use the same OKLAB bounds as
#' `.run_optimization()`, enforced by projection during the line search.
#'
#' @param initial_colors_oklab Matrix of all colors (fixed and initial
#'   free) in OKLAB space.
#' @param fixed_mask Logical vector indicating which rows are fixed.
#' @param max_iterations Maximum L-BFGS iterations (not evaluations).
#' @param cvd_safe Logical; optimize the worst case across CVD
#'   simulations, or plain OKLAB separation.
#' @param k Soft-min temperature.
#' @param init_alpha,history_size,tol_obj,tol_rel_obj,tol_grad,tol_rel_grad,tol_param
#'   Passed to [stan_lbfgs()]; defaults follow Stan.
#' @param verbose Print Stan-style progress lines.
#' @return Same shape as the other `optimize_colors_*()` functions:
#'   list with `palette` and `details`.
#' @noRd
optimize_colors_stan_lbfgs <- function(
  initial_colors_oklab,
  fixed_mask,
  max_iterations = 1000L,
  cvd_safe = TRUE,
  k = 30,
  gamut_penalty = 0,
  init_alpha = 0.001,
  history_size = 5L,
  tol_obj = 1e-12,
  tol_rel_obj = 1e8,
  tol_grad = 1e-8,
  tol_rel_grad = 1e7,
  tol_param = 1e-8,
  verbose = FALSE
) {
  n_free <- sum(!fixed_mask)
  if (n_free == 0) {
    return(.make_list_result(
      as.matrix(initial_colors_oklab),
      list(
        algorithm = "Stan-style L-BFGS (PoC)",
        iterations = as.integer(0),
        f_evals = as.integer(0),
        converged = NA,
        status_message = "No free colors to optimize",
        final_objective_value = NA_real_
      )
    ))
  }

  # Same OKLAB box as .run_optimization()
  lb <- c(0.001, -0.4, -0.4)
  ub <- c(0.999, 0.4, 0.4)
  lower_bounds <- rep(lb, n_free)
  upper_bounds <- rep(ub, n_free)

  x0 <- as.vector(t(initial_colors_oklab[!fixed_mask, , drop = FALSE]))

  f_grad <- function(free_params_vec) {
    full <- initial_colors_oklab
    full[!fixed_mask, ] <- matrix(free_params_vec, ncol = 3, byrow = TRUE)
    fg <- cvd_smooth_objective_gradient(
      full, k = k, cvd_safe = cvd_safe, gamut_penalty = gamut_penalty
    )
    list(value = fg$value, gradient = as.vector(t(fg$gradient[!fixed_mask, , drop = FALSE])))
  }

  res <- stan_lbfgs(
    f_grad = f_grad,
    par = x0,
    lower = lower_bounds,
    upper = upper_bounds,
    max_iterations = as.integer(max_iterations),
    init_alpha = init_alpha,
    history_size = history_size,
    tol_obj = tol_obj,
    tol_rel_obj = tol_rel_obj,
    tol_grad = tol_grad,
    tol_rel_grad = tol_rel_grad,
    tol_param = tol_param,
    verbose = verbose
  )

  x <- matrix(res$par, ncol = 3, byrow = TRUE)
  x[, 1] <- .clamp_to_bounds(x[, 1], lb[1], ub[1])
  x[, 2] <- .clamp_to_bounds(x[, 2], lb[2], ub[2])
  x[, 3] <- .clamp_to_bounds(x[, 3], lb[3], ub[3])
  final <- initial_colors_oklab
  final[!fixed_mask, ] <- x

  details <- list(
    algorithm = "Stan-style L-BFGS (PoC)",
    iterations = as.integer(res$iterations),
    f_evals = as.integer(res$f_evals),
    converged = res$converged,
    convergence_test = res$convergence_test,
    status_message = res$status_message,
    final_objective_value = res$value
  )
  .make_list_result(as.matrix(final), details)
}
