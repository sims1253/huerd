# Stan-style L-BFGS optimizer (proof of concept)
#
# A pure-R port of the L-BFGS optimizer described in the Stan Reference
# Manual (https://mc-stan.org/docs/reference-manual/optimization.html),
# following Nocedal & Wright (2006) as implemented in Stan Math:
#
# * search direction from the limited-memory two-loop recursion
#   (`history_size` curvature pairs, default 5 as recommended in the
#   Stan manual)
# * Wolfe-condition line search (Nocedal & Wright Algorithm 3.5/3.6,
#   c1 = 1e-4, c2 = 0.9) with bisection zoom
# * the five Stan convergence tests: parameter, density, relative
#   density, gradient, and relative gradient (the relative tests are
#   scaled by machine epsilon, mirroring the Stan manual's table)
# * `init_alpha` initial step size (default 0.001, as in Stan) with the
#   last accepted step reused as the next initial guess
# * Hessian-approximation reset when curvature pairs degenerate or the
#   line search fails, mirroring Stan's "resetting Hessian
#   approximation" recovery before giving up
#
# Box constraints are handled by projection (as NLopt's L-BFGS does)
# rather than by an unconstraining transform: the Stan manual's caveat
# about boundary optima under transforms is real for this problem --
# the OKLAB-box corners are exactly where palette optima live, and a
# logit transform prematurely zeros the gradient there. The gradient
# convergence tests therefore use the projected gradient
# x - proj(x - g), the standard projected-method criterion.

#' Machine epsilon used to scale the relative convergence tests
#' @noRd
.STAN_LBFGS_EPS <- .Machine$double.eps

#' Apply the L-BFGS inverse-Hessian approximation via the two-loop recursion
#'
#' @param gradient Numeric vector, gradient at the current point.
#' @param s_list,y_list,rho_list Curvature history (parallel lists of
#'   vectors s_k, y_k and scalars rho_k = 1 / (y_k' s_k)), oldest first.
#' @return Numeric vector, the product of the implicit inverse-Hessian
#'   approximation with `gradient`.
#' @noRd
.stan_lbfgs_apply_inv_hessian <- function(gradient, s_list, y_list, rho_list) {
  q <- gradient
  k <- length(s_list)
  if (k == 0) {
    return(q)
  }
  alpha <- numeric(k)

  # First loop, newest pair to oldest
  for (i in seq(k, 1)) {
    alpha[i] <- rho_list[[i]] * sum(s_list[[i]] * q)
    q <- q - alpha[i] * y_list[[i]]
  }

  # Scale by gamma = (y's) / (y'y) of the most recent pair
  gamma <- (1 / rho_list[[k]]) / sum(y_list[[k]]^2)
  q <- gamma * q

  # Second loop, oldest pair to newest
  for (i in seq_len(k)) {
    beta <- rho_list[[i]] * sum(y_list[[i]] * q)
    q <- q + s_list[[i]] * (alpha[i] - beta)
  }
  q
}

#' Wolfe-conditions line search (Nocedal & Wright, Algorithm 3.5 + zoom 3.6)
#'
#' Trial points are projected onto `[lower, upper]`, so the objective is
#' evaluated on the feasible box throughout. Phase one brackets the step
#' size by extrapolation; the zoom bisects the bracket. Strict Wolfe
#' points are preferred, but when the bracket collapses (e.g. in exactly
#' flat regions created by the projection), the search falls back to the
#' best Armijo-satisfying point seen, and only signals failure
#' (alpha = 0) if none exists.
#'
#' @param f_grad Function(par) returning list(value, gradient).
#' @param par Current parameter vector (feasible).
#' @param direction Search direction (a descent direction).
#' @param f0,grad0 Objective value and gradient at `par`.
#' @param alpha0 Initial step-size guess (Stan's `init_alpha` semantics).
#' @param lower,upper Box bounds (recycled).
#' @param c1,c2 Armijo and curvature constants.
#' @param max_evals Maximum objective evaluations in the line search.
#' @param min_alpha Bracket width below which strict bisection gives up.
#' @return List with elements `alpha` (0 signals failure), `value`,
#'   `gradient` (at the accepted point when successful), `par` (the
#'   accepted, projected point), and `n_evals`.
#' @noRd
.stan_lbfgs_wolfe_line_search <- function(
  f_grad,
  par,
  direction,
  f0,
  grad0,
  alpha0,
  lower,
  upper,
  c1 = 1e-4,
  c2 = 0.9,
  max_evals = 100L,
  min_alpha = 1e-16
) {
  dphi0 <- sum(grad0 * direction) # < 0 for a descent direction
  n_evals <- 0L
  best <- NULL # best Armijo-satisfying point seen

  eval_point <- function(alpha) {
    x <- pmin(pmax(par + alpha * direction, lower), upper)
    fg <- f_grad(x)
    n_evals <<- n_evals + 1L  # value/gradient used as returned
    list(
      alpha = alpha,
      par = x,
      fg = fg,
      phi = fg$value,
      dphi = sum(as.numeric(fg$gradient) * direction)
    )
  }
  armijo <- function(pt) pt$phi <= f0 + c1 * pt$alpha * dphi0
  curvature <- function(pt) abs(pt$dphi) <= -c2 * dphi0
  note <- function(pt) {
    if (armijo(pt) && (is.null(best) || pt$phi < best$phi)) {
      best <<- pt
    }
  }
  as_result <- function(pt) {
    list(
      alpha = pt$alpha,
      par = pt$par,
      value = pt$fg$value,
      gradient = as.numeric(pt$fg$gradient),
      n_evals = n_evals
    )
  }
  fail <- function() {
    list(
      alpha = 0,
      par = par,
      value = f0,
      gradient = grad0,
      n_evals = n_evals
    )
  }

  zoom <- function(alpha_lo, alpha_hi, pt_lo) {
    repeat {
      if (n_evals >= max_evals || abs(alpha_hi - alpha_lo) < min_alpha) {
        return(NULL)
      }
      alpha_j <- 0.5 * (alpha_lo + alpha_hi)
      if (is.na(alpha_j) || alpha_j == alpha_lo || alpha_j == alpha_hi) {
        return(NULL)
      }
      pt <- eval_point(alpha_j)
      note(pt)
      if (!armijo(pt) || pt$phi >= pt_lo$phi) {
        alpha_hi <- alpha_j
      } else {
        if (curvature(pt)) {
          return(pt)
        }
        if (pt$dphi * (alpha_hi - alpha_lo) >= 0) {
          alpha_hi <- alpha_lo
        }
        alpha_lo <- alpha_j
        pt_lo <- pt
      }
    }
  }

  alpha_prev <- 0
  pt_prev <- list(alpha = 0, phi = f0)
  alpha <- alpha0
  result <- NULL
  repeat {
    pt <- eval_point(alpha)
    note(pt)
    if (!armijo(pt) || pt$phi >= pt_prev$phi) {
      result <- zoom(alpha_prev, alpha, pt_prev)
      break
    }
    if (curvature(pt)) {
      result <- pt
      break
    }
    if (pt$dphi >= 0) {
      result <- zoom(alpha, alpha_prev, pt)
      break
    }
    alpha_prev <- alpha
    pt_prev <- pt
    alpha <- 2 * alpha
    if (n_evals >= max_evals) {
      break
    }
  }

  if (is.null(result)) {
    result <- best
  }
  if (is.null(result)) {
    fail()
  } else {
    as_result(result)
  }
}

#' Minimize a box-constrained objective with the Stan-style L-BFGS algorithm
#'
#' @param f_grad Function(par) returning a list with elements `value`
#'   (the objective to minimize) and `gradient` (its gradient), like
#'   Stan's combined value-and-gradient evaluation.
#' @param par Initial parameter vector (projected onto the box first).
#' @param lower,upper Box bounds (recycled); default unrestricted.
#' @param max_iterations Maximum iterations (Stan default 2000).
#' @param init_alpha Initial step size (Stan default 0.001).
#' @param history_size Number of stored curvature pairs (Stan default 5;
#'   the manual suggests 5-10 and below the parameter dimension).
#' @param tol_obj,tol_rel_obj,tol_grad,tol_rel_grad,tol_param Convergence
#'   tolerances, following the Stan Reference Manual. The relative tests
#'   are compared against `tol * .Machine$double.eps`. A tolerance of 0
#'   disables the corresponding test. The gradient-based tests use the
#'   projected gradient (x - proj(x - g)) because optima often sit on
#'   the box boundary.
#' @param verbose Print Stan-style progress lines.
#' @return List with elements `par`, `value`, `gradient`, `iterations`,
#'   `f_evals` (combined value+gradient evaluations), `converged`
#'   (logical), `status_message`, and `convergence_test` (which of the
#'   Stan termination criteria fired, or NA).
#' @noRd
stan_lbfgs <- function(
  f_grad,
  par,
  lower = -Inf,
  upper = Inf,
  max_iterations = 2000L,
  init_alpha = 0.001,
  history_size = 5L,
  tol_obj = 1e-12,
  tol_rel_obj = 1e8,
  tol_grad = 1e-8,
  tol_rel_grad = 1e7,
  tol_param = 1e-8,
  verbose = FALSE
) {
  par <- as.numeric(par)
  if (history_size < 1L) {
    stop("history_size must be >= 1")
  }
  lower <- rep_len(lower, length(par))
  upper <- rep_len(upper, length(par))
  par <- pmin(pmax(par, lower), upper)

  fg <- f_grad(par)
  f_evals <- 1L
  f <- fg$value
  g <- as.numeric(fg$gradient)

  projected_gradient_norm <- function(x, g) {
    max(abs(pmin(pmax(x - g, lower), upper) - x))
  }

  s_list <- list()
  y_list <- list()
  rho_list <- list()

  direction <- -g
  alpha <- init_alpha
  converged <- FALSE
  convergence_test <- NA_character_
  status_message <- "Maximum iterations reached"
  iterations <- 0L

  if (verbose) {
    cat(sprintf("%9s %12s %12s %9s\n", "iter", "f(x)", "||grad||inf", "alpha"))
    cat(sprintf("%9d %12.6f %12.4e\n", iterations, f, max(abs(g))))
  }

  # Convergence tests, evaluated at the top of each iteration as in Stan
  # services: the parameter/density tests compare against the previous
  # iteration (so they cannot fire on iteration 1), while the gradient
  # tests use only the current state.
  f_prev <- Inf
  par_prev <- par
  while (iterations < max_iterations) {
    if (tol_grad > 0 && projected_gradient_norm(par, g) < tol_grad) {
      converged <- TRUE
      convergence_test <- "gradient"
      status_message <- "Converged: gradient tolerance reached"
      break
    }
    if (tol_rel_grad > 0 && length(s_list) > 0) {
      hinv_g <- .stan_lbfgs_apply_inv_hessian(g, s_list, y_list, rho_list)
      rel_grad <- sum(g * hinv_g) / max(abs(f), 1)
      if (rel_grad < tol_rel_grad * .STAN_LBFGS_EPS) {
        converged <- TRUE
        convergence_test <- "relative gradient"
        status_message <- "Converged: relative gradient tolerance reached"
        break
      }
    }
    if (iterations > 0) {
      if (tol_obj > 0 && abs(f - f_prev) < tol_obj) {
        converged <- TRUE
        convergence_test <- "objective"
        status_message <- "Converged: objective tolerance reached"
        break
      }
      if (tol_rel_obj > 0 &&
        abs(f - f_prev) / max(abs(f), abs(f_prev), 1) <
          tol_rel_obj * .STAN_LBFGS_EPS) {
        converged <- TRUE
        convergence_test <- "relative objective"
        status_message <- "Converged: relative objective tolerance reached"
        break
      }
      if (tol_param > 0 && max(abs(par - par_prev)) < tol_param) {
        converged <- TRUE
        convergence_test <- "parameter"
        status_message <- "Converged: parameter tolerance reached"
        break
      }
    }

    # Nocedal & Wright: after the first (steepest-descent) iteration,
    # always try the unit step first -- the quasi-Newton direction
    # approximates Newton, and reusing the last accepted alpha instead
    # accepts a short step forever under the weak curvature condition
    # (linear instead of superlinear convergence).
    ls <- .stan_lbfgs_wolfe_line_search(
      f_grad,
      par,
      direction,
      f0 = f,
      grad0 = g,
      alpha0 = if (iterations == 0L || length(s_list) == 0) init_alpha else 1,
      lower = lower,
      upper = upper
    )
    f_evals <- f_evals + ls$n_evals

    if (ls$alpha <= 0) {
      # Line search failed: reset the Hessian approximation and retry
      # from steepest descent, as Stan does; give up if that fails too.
      s_list <- list()
      y_list <- list()
      rho_list <- list()
      direction <- -g
      ls <- .stan_lbfgs_wolfe_line_search(
        f_grad,
        par,
        direction,
        f0 = f,
        grad0 = g,
        alpha0 = init_alpha,
        lower = lower,
        upper = upper
      )
      f_evals <- f_evals + ls$n_evals
      if (ls$alpha <= 0) {
        status_message <- "Line search failed after Hessian approximation reset"
        break
      }
    }

    par_prev <- par
    f_prev <- f
    g_prev <- g
    par <- ls$par
    f <- ls$value
    g <- ls$gradient
    alpha <- ls$alpha
    iterations <- iterations + 1L

    # Update the curvature history; a non-positive y's invalidates the
    # approximation, so reset it (Stan's lbfgs_update behavior).
    s <- par - par_prev
    y <- g - g_prev
    yts <- sum(s * y)
    if (is.finite(yts) && yts > 0) {
      s_list <- c(s_list, list(s))
      y_list <- c(y_list, list(y))
      rho_list <- c(rho_list, list(1 / yts))
      if (length(s_list) > history_size) {
        s_list <- s_list[-1]
        y_list <- y_list[-1]
        rho_list <- rho_list[-1]
      }
    } else {
      s_list <- list()
      y_list <- list()
      rho_list <- list()
    }
    direction <- -.stan_lbfgs_apply_inv_hessian(g, s_list, y_list, rho_list)

    if (verbose) {
      cat(sprintf(
        "%9d %12.6f %12.4e %9.3g\n",
        iterations, f, max(abs(g)), alpha
      ))
    }
  }

  list(
    par = par,
    value = f,
    gradient = g,
    iterations = iterations,
    f_evals = f_evals,
    converged = converged,
    convergence_test = convergence_test,
    status_message = status_message
  )
}
