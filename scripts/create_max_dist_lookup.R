## code to prepare `MAX_MIN_DIST_ESTIMATES` dataset goes here

# This script is for the package developer. It is run once to create
# R/sysdata.rda, which contains the MAX_MIN_DIST_ESTIMATES lookup table.
# This object is then available internally to all package functions.
#
# This process can be time-consuming (minutes to hours depending on parameters).

# Make sure devtools/usethis are installed for the final step.
# And your package's dependencies (farver) are available.
# It's best to run this after loading your package functions, e.g., with devtools::load_all()

# --- 1. Generate a Large Pool of In-Gamut OKLAB Colors ---

# The quality of the estimate depends on the density of this pool.
# 500,000 is a good number for decent accuracy.
# 1,000,000+ would be better but slower.
n_pool <- 1000000
cat("Generating a candidate pool of", n_pool, "points...\n")

# Generate points in a box that fully contains the sRGB gamut in OKLAB space.
# OKLAB L is [0,1]. OKLAB a/b for sRGB is roughly [-0.25, 0.25] for L>0.1
# and can reach +/- 0.4 near black. We sample a slightly larger box.
l_pool <- stats::runif(n_pool, 0, 1)
a_pool <- stats::runif(n_pool, -0.4, 0.4)
b_pool <- stats::runif(n_pool, -0.4, 0.4)
oklab_pool <- cbind(L = l_pool, a = a_pool, b = b_pool)

cat("Filtering for in-gamut sRGB colors...\n")
# Since farver automatically clamps out-of-gamut colors to valid RGB values,
# we need a different approach to detect which OKLAB colors are truly in-gamut.
# We'll do a round-trip conversion and check if the OKLAB values remain the same.

# Convert OKLAB -> RGB -> OKLAB
rgb_pool <- farver::convert_colour(oklab_pool, from = "oklab", to = "rgb")
oklab_roundtrip <- farver::convert_colour(rgb_pool, from = "rgb", to = "oklab")

# A color is in-gamut if the round-trip conversion preserves the original OKLAB values
# (within a small tolerance for floating-point precision)
tolerance <- 1e-6
in_gamut_indices <- which(
  !is.na(rgb_pool[, 1]) &
    !is.na(rgb_pool[, 2]) &
    !is.na(rgb_pool[, 3]) &
    abs(oklab_pool[, 1] - oklab_roundtrip[, 1]) < tolerance &
    abs(oklab_pool[, 2] - oklab_roundtrip[, 2]) < tolerance &
    abs(oklab_pool[, 3] - oklab_roundtrip[, 3]) < tolerance
)
in_gamut_oklab_pool <- oklab_pool[in_gamut_indices, ]
cat("Found", nrow(in_gamut_oklab_pool), "in-gamut candidate colors.\n")


# --- 2. Greedy Farthest Point Sampling Algorithm ---
# This function estimates the max-min distance for a given n by running a
# greedy algorithm multiple times with different random starting points.
estimate_max_min_dist <- function(n, pool, n_restarts = 25) {
  best_min_dist_found <- 0

  for (i in 1:n_restarts) {
    # Start with a single random point from the pool
    centers <- matrix(pool[sample.int(nrow(pool), 1), ], nrow = 1)

    if (n > 1) {
      for (j in 2:n) {
        # Find the point in the pool that is furthest from any existing center
        # This is computationally the most expensive part.
        min_dists_to_centers <- apply(pool, 1, function(p) {
          # Calculate Euclidean distance from p to all rows in centers
          distances_to_p <- sqrt(colSums((t(centers) - p)^2))
          # Return the distance to the *nearest* center
          min(distances_to_p)
        })
        # The point we want to add is the one with the largest minimum distance
        next_point_idx <- which.max(min_dists_to_centers)
        centers <- rbind(centers, pool[next_point_idx, ])
      }
    }

    # Calculate the minimum distance for this generated set of n centers
    # stats::dist() is efficient for this
    final_min_dist_for_set <- min(stats::dist(centers))

    # Update our best-so-far if this run was better
    if (final_min_dist_for_set > best_min_dist_found) {
      best_min_dist_found <- final_min_dist_for_set
    }
  }
  return(best_min_dist_found)
}

# --- 3. Compute for a Range of N and Store Results ---

# Define the range of palette sizes (n) to compute estimates for.
# 2 to 40 is a very reasonable range for qualitative palettes.
n_values <- 2:40
cat(
  "Estimating max-min distances for n =",
  min(n_values),
  "to",
  max(n_values),
  "...\n"
)

# Use a progress bar for this long process
pb <- utils::txtProgressBar(min = 0, max = length(n_values), style = 3)
estimated_max_dists <- sapply(n_values, function(n) {
  res <- estimate_max_min_dist(n, pool = in_gamut_oklab_pool, n_restarts = 15)
  utils::setTxtProgressBar(pb, which(n_values == n))
  return(res)
})
close(pb)
cat("\nDone estimating.\n")

# Store the results as a named vector for easy lookup
# Names will be "2", "3", ... "40"
MAX_MIN_DIST_ESTIMATES <- stats::setNames(estimated_max_dists, n_values)

# Let's look at the result
print(MAX_MIN_DIST_ESTIMATES)

# --- 4. Save to the Package's Internal Data ---

# This is the crucial step. It saves the `MAX_MIN_DIST_ESTIMATES` object
# into a special file `R/sysdata.rda`. This file is automatically loaded
# when your package is loaded, making the object available to your functions.
# Ensure you have the `usethis` package installed.
# Run this from the root of your package project.
usethis::use_data(MAX_MIN_DIST_ESTIMATES, internal = TRUE, overwrite = TRUE)

# After running this, you will see a message like:
# ✔ Saving `MAX_MIN_DIST_ESTIMATES` to `R/sysdata.rda`
