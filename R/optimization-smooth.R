#' Smooth Differentiable Objective Function for Color Optimization
#'
#' Uses inverse square sum as a differentiable proxy for max-min distance optimization.
#' This function heavily penalizes small distances while remaining smooth and differentiable.
#'
#' @param colors_oklab Matrix of colors in OKLAB space (n x 3)
#' @param epsilon Small constant to prevent division by zero (default: 1e-8)
#' @return Single numeric value representing the objective score
#' @details
#' The function computes: sum(1 / (distance^2 + epsilon)) where distance
#' is the Euclidean distance between all pairs of colors in OKLAB space.
#' Lower values indicate better color separation.
#' @noRd
objective_smooth_repulsion <- function(colors_oklab, epsilon = 1e-8) {
  # Validate inputs
  if (!is.matrix(colors_oklab) || ncol(colors_oklab) != 3) {
    stop("colors_oklab must be a matrix with 3 columns")
  }

  n_colors <- nrow(colors_oklab)
  if (n_colors < 2) {
    return(0) # No repulsion needed for single color
  }

  # Calculate pairwise distances efficiently
  distances <- as.matrix(dist(colors_oklab))

  # Remove diagonal (distance to self = 0)
  distances[diag(n_colors)] <- NA

  # Calculate inverse square sum (excluding diagonal)
  objective_value <- sum(1 / (distances^2 + epsilon), na.rm = TRUE)

  return(objective_value)
}

#' Log-Sum-Exp Smooth Objective Function for Color Optimization
#'
#' Uses log-sum-exp as a smooth approximation to the max function for
#' distance optimization. Provides an alternative optimization landscape.
#'
#' @param colors_oklab Matrix of colors in OKLAB space (n x 3)
#' @param k Temperature parameter controlling approximation quality (default: 10)
#' @return Single numeric value representing the objective score
#' @details
#' The function computes: log(sum(exp(-k * distance))) / k where distance
#' is the Euclidean distance between all pairs of colors in OKLAB space.
#' Lower values indicate better color separation.
#' @noRd
objective_smooth_logsumexp <- function(colors_oklab, k = 10) {
  # Validate inputs
  if (!is.matrix(colors_oklab) || ncol(colors_oklab) != 3) {
    stop("colors_oklab must be a matrix with 3 columns")
  }

  n_colors <- nrow(colors_oklab)
  if (n_colors < 2) {
    return(0) # No optimization needed for single color
  }

  # Calculate pairwise distances efficiently
  distances <- as.matrix(dist(colors_oklab))

  # Remove diagonal (distance to self = 0)
  distances[diag(n_colors)] <- NA

  # Calculate log-sum-exp (excluding diagonal)
  exp_values <- exp(-k * distances)
  objective_value <- log(sum(exp_values, na.rm = TRUE)) / k

  return(objective_value)
}

#' Analytical Gradient for Smooth Repulsion Objective
#'
#' Computes the analytical gradient of the smooth repulsion objective function
#' with respect to each color component in OKLAB space.
#'
#' @param colors_oklab Matrix of colors in OKLAB space (n x 3)
#' @param epsilon Small constant to prevent division by zero (default: 1e-8)
#' @return Matrix of gradients (n x 3) for each color component
#' @details
#' For each color c_k, the gradient is:
#' ∇_k f = -2 * Σ_i≠k (1/(d_ik^2 + ε)^2) * (c_k - c_i) / d_ik
#' where d_ik is the Euclidean distance between colors k and i.
#' @noRd
gradient_smooth_repulsion <- function(colors_oklab, epsilon = 1e-8) {
  n_colors <- nrow(colors_oklab)
  n_dims <- ncol(colors_oklab)

  # Initialize gradient matrix
  gradient <- matrix(0, nrow = n_colors, ncol = n_dims)

  # Calculate gradients for each color
  for (k in 1:n_colors) {
    for (i in 1:n_colors) {
      if (i != k) {
        # Distance between colors k and i
        diff_vector <- colors_oklab[k, ] - colors_oklab[i, ]
        distance <- sqrt(sum(diff_vector^2))

        # Gradient contribution from pair (k, i)
        if (distance > epsilon) {
          gradient_factor <- -2 / ((distance^2 + epsilon)^2) / distance
          gradient[k, ] <- gradient[k, ] + gradient_factor * diff_vector
        }
      }
    }
  }

  return(gradient)
}

#' Analytical Gradient for Log-Sum-Exp Objective
#'
#' Computes the analytical gradient of the log-sum-exp objective function
#' with respect to each color component in OKLAB space.
#'
#' @param colors_oklab Matrix of colors in OKLAB space (n x 3)
#' @param k Temperature parameter (default: 10)
#' @return Matrix of gradients (n x 3) for each color component
#' @noRd
gradient_smooth_logsumexp <- function(colors_oklab, k = 10) {
  n_colors <- nrow(colors_oklab)
  n_dims <- ncol(colors_oklab)

  # Initialize gradient matrix
  gradient <- matrix(0, nrow = n_colors, ncol = n_dims)

  # Calculate distances and weights
  distances <- as.matrix(dist(colors_oklab))
  exp_values <- exp(-k * distances)
  sum_exp <- sum(exp_values, na.rm = TRUE)

  # Calculate gradients for each color
  for (m in 1:n_colors) {
    for (n in 1:n_colors) {
      if (m != n) {
        diff_vector <- colors_oklab[m, ] - colors_oklab[n, ]
        distance <- distances[m, n]

        if (distance > 1e-8) {
          weight <- exp_values[m, n] / sum_exp
          gradient_factor <- -weight * (-k) / distance
          gradient[m, ] <- gradient[m, ] + gradient_factor * diff_vector
        }
      }
    }
  }

  return(gradient)
}
