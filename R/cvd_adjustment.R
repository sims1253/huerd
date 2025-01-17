#' Adjust Colors for Color Vision Deficiency (CVD)
#'
#' This function optimizes a set of colors to be more distinguishable for individuals
#' with various types of color vision deficiency (color blindness) while maintaining
#' the visual appeal for those with normal color vision.
#'
#' @param lab_colors A matrix with 3 columns representing colors in LAB color space,
#'        where each row is a color with L*, a*, and b* values
#' @param base_colors Optional matrix of LAB colors that should remain unchanged during
#'        the optimization process. Must have the same format as lab_colors
#' @param return_states Logical; if TRUE, returns a list of intermediate color states
#'        during optimization. Default is FALSE
#' @param save_every Integer specifying how often (in iterations) to save intermediate
#'        states. Only relevant if return_states is TRUE. Default is 5
#' @param convergence_threshold Integer specifying how many consecutive non-improving
#'        iterations to allow before stopping. Default is 10
#' @param min_iterations Minimum number of iterations to run before allowing
#'        convergence. Default is 100
#' @param max_iterations Maximum number of iterations to run before forcing
#'        termination. Default is 5000
#'
#' @return If return_states is FALSE (default), returns a matrix of optimized colors
#'         in LAB color space. If return_states is TRUE, returns a list of matrices
#'         showing the optimization process at intervals specified by save_every.
#'
#' @details The function uses a stochastic optimization approach to gradually adjust
#' colors to improve their distinguishability for different types of color vision
#' deficiency. It maintains color validity within LAB color space boundaries and
#' allows for certain colors to remain fixed during optimization.
#'
#' @examples
#' # Basic usage with 5 colors in LAB space
#' lab_colors <- matrix(c(
#'   50, 0, 0,
#'   60, 20, 30,
#'   70, -20, 40,
#'   40, 10, -30,
#'   80, -10, 20
#' ), ncol = 3, byrow = TRUE)
#' optimized_colors <- adjust_for_cvd(lab_colors)
#'
#' # Using base colors that won't be modified
#' base_colors <- matrix(c(50, 0, 0), ncol = 3)
#' optimized_with_base <- adjust_for_cvd(lab_colors, base_colors = base_colors)
#'
#' @export
#' @importFrom stats rnorm
adjust_for_cvd <- function(lab_colors,
                           base_colors = NULL,
                           return_states = FALSE,
                           save_every = 5,
                           convergence_threshold = 10,
                           min_iterations = 100,
                           max_iterations = 5000) {
  # Input validation
  if (save_every < 1) stop("save_every must be >= 1")
  if (min_iterations < 1) stop("min_iterations must be >= 1")
  if (max_iterations < 1) stop("max_iterations must be >= 1")
  if (convergence_threshold <= 0) {
    stop("convergence_threshold must be positive")
  }

  if (return_states) {
    states <- vector(mode = "list", length = floor(max_iterations / save_every))
  }

  boundaries <- get_lab_boundaries(base_colors = base_colors)



  # Find indices of base colors if provided
  if (!is.null(base_colors)) base_indices <- seq(1, length(base_colors))
  modify_indices <- setdiff(1:nrow(lab_colors), base_indices)

  # Initialize convergence tracking
  last_change <- 0
  last_scores <- lapply(
    calculate_all_distances(lab_colors),
    mean_bottom_quartile
  )

  for (i in 1:max_iterations) {
    prev_points <- lab_colors

    # Save state if needed
    if (i %% save_every == 0 && return_states) {
      states[[i / save_every]] <- prev_points
    }

    # Create noise matrix
    noise <- matrix(0, nrow = nrow(lab_colors), ncol = 3)
    noise[modify_indices, 1] <- matrix(
      rnorm(
        length(modify_indices),
        0,
        0.01 * mean(abs(lab_colors[, 1]))
      ),
      ncol = 1
    )

    noise[modify_indices, 2] <- matrix(
      rnorm(
        length(modify_indices),
        0,
        0.01 * mean(abs(lab_colors[, 2]))
      ),
      ncol = 1
    )

    noise[modify_indices, 3] <- matrix(
      rnorm(
        length(modify_indices),
        0,
        0.01 * mean(abs(lab_colors[, 3]))
      ),
      ncol = 1
    )

    new_points <- lab_colors + noise

    # Constrain to valid LAB space
    new_points <- enforce_boundaries(
      new_points,
      boundaries = get_lab_boundaries()
    )

    new_scores <- lapply(
      calculate_all_distances(new_points),
      mean_bottom_quartile
    )

    score_eval <- evaluate_cvd_change(
      new_scores = new_scores,
      old_scores = last_scores
    )

    if (score_eval) {
      last_scores <- new_scores
      lab_colors <- new_points
      last_change <- 0
    } else {
      last_change <- last_change + 1
    }


    # Calculate convergence metric (root mean square movement)
    if (i >= min_iterations && last_change > convergence_threshold) {
      break
    }
  }

  # Always save final state
  if (max_iterations %% save_every != 0) {
    states[[floor(i / save_every + 1)]] <- prev_colors
  }
  if (return_states) {
    return(states[!sapply(states, is.null)])
  } else {
    return(lab_colors)
  
  }
}

#' Evaluate Changes in Color Vision Deficiency (CVD) Scores
#' 
#' This function compares two sets of CVD scores and determines if the changes represent
#' an improvement. It uses a weighted approach that gives more importance to improvements
#' in scores that were initially closer to the minimum score.
#'
#' @param new_scores A numeric vector or list containing the new CVD scores
#' @param old_scores A numeric vector or list containing the original CVD scores
#'                   Must be the same length as new_scores
#'
#' @return A logical value indicating whether the changes represent an overall improvement
#'         (TRUE) or not (FALSE)
#'
#' @details
#' The function calculates relative improvements between old and new scores and weights
#' them based on their proximity to the minimum score in the original set. Scores closer
#' to the minimum receive higher weights in the final calculation.
#'
#' The weighted improvement is considered positive if the weighted sum of relative
#' improvements is greater than 0.
#'
#' @examples
#' old_scores <- list(10, 15, 20)
#' new_scores <- list(8, 14, 19)
#' evaluate_cvd_change(new_scores, old_scores)
#'
#' @export
evaluate_cvd_change <- function(new_scores, old_scores) {
  # Get minimum scores for scaling
  min_old <- min(unlist(old_scores))
  min_new <- min(unlist(new_scores))

  # Calculate relative improvements
  improvements <- mapply(function(new, old) {
    # Use relative improvement to handle different scales
    (new - old) / old
  }, new_scores, old_scores)

  # Calculate weights based on how close each score is to the minimum
  weights <- mapply(function(score) {
    1 / (1 + (score / min_old - 1)^2)
  }, old_scores)

  # Normalize weights and calculate weighted improvement
  weights <- weights / sum(weights)
  weighted_improvements <- sum(improvements * weights)

  # Return both the score and diagnostics
  return(
    accept = weighted_improvements > 0
  )
}
