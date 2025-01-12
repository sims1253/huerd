#' Calculate Mean of Bottom Quartile
#'
#' Helper function to calculate the mean of the lowest 25% of values in a
#' vector.
#'
#' @param distances Numeric vector of distances
#' @return Mean value of the bottom quartile
mean_bottom_quartile <- function(distances) {
  sorted_distances <- sort(distances)
  quartile_size <- length(distances) %/% 4
  mean(sorted_distances[1:quartile_size])
}

#' Calculate Reference Distances for Color Palettes
#'
#' Generates reference color palettes and calculates their color distances in
#' both original and CVD simulations to provide benchmarks for palette
#' comparison.
#'
#' @param n_colors Number of colors to generate in each palette
#' @return A list containing reference distances for three palettes (batlow,
#'   brewer, and tableau). Each palette contains mean bottom quartile distances
#'   for original colors and three CVD simulations.
#'
#' @importFrom scico scico
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales hue_pal
get_reference_distances <- function(n_colors) {
  # Generate reference palettes
  palettes <- list(
    batlow = scico::scico(n_colors, palette = "batlow", categorical = TRUE),
    brewer = RColorBrewer::brewer.pal(min(8, n_colors), "Set2"),
    tableau = scales::hue_pal()(n_colors)
  )

  # Calculate distances for each palette
  distances <- lapply(
    palettes,
    function(colors) {
      calculate_all_distances(
        farver::convert_colour(
          t(grDevices::col2rgb(colors)),
          from = "rgb", to = "xyz"
        )
      )
    }
  )

  # Calculate mean bottom quartile for each palette and CVD version
  lapply(distances, function(dist) {
    list(
      original = mean_bottom_quartile(dist$original),
      deutan = mean_bottom_quartile(dist$deutan),
      protan = mean_bottom_quartile(dist$protan),
      tritan = mean_bottom_quartile(dist$tritan)
    )
  })
}

#' Calculate distances between colors
#'
#' This function computes perceptual distances between all color pairs in a
#' given set of colors. It converts LAB coordinates to RGB, checks for valid
#' RGB colors, and calculates the color distance using the color_distance
#' function.
#'
#' @param points A matrix of LAB coordinates where each row represents a color
#'               and columns represent L, A, and B values respectively.
#'
#' @return A list containing two elements:
#'         \item{avg}{The average perceptual distance between all valid
#'         color pairs.}
#'         \item{min}{The minimum perceptual distance found between any two
#'         valid color pairs.}
#'
#' @details The function skips calculations for invalid RGB colors (values
#'          outside 0-255 range) and assigns a distance of 0 to such pairs.
#'          It also excludes fixed base colors from the calculations.
#'
#' @keywords internal
calculate_color_distances <- function(points) {
  distances <- farver::compare_colour(
    points,
    from_space = "lab",
    method = "cie2000"
  )

  list(
    mean = mean(distances, na.rm = TRUE),
    median = median(distances, na.rm = TRUE),
    min = min(distances, na.rm = TRUE),
    max = max(distances, na.rm = TRUE),
    sd = sd(distances, na.rm = TRUE)
  )
}

#' Calculate Color Distances for CVD Simulations
#'
#' Calculates color distances between pairs of colors in both original and
#' simulated color vision deficiency (CVD) spaces.
#'
#' @param points A matrix or data frame of LAB color values (n × 3), where each
#'               row represents a color and columns are L, a, and b values.
#' @return A named list of distance vectors:
#' \describe{
#'   \item{original}{Pairwise distances between original colors}
#'   \item{deutan}{Pairwise distances in deuteranomaly simulation}
#'   \item{protan}{Pairwise distances in protanomaly simulation}
#'   \item{tritan}{Pairwise distances in tritanomaly simulation}
#' }
#' @importFrom farver convert_colour compare_colour
#' @importFrom colorspace deutan protan tritan
#' @export
calculate_all_distances <- function(points) {
  # Convert LAB to RGB for CVD simulation
  rgb_colors <- farver::convert_colour(points, from = "lab", to = "rgb")

  # Calculate distances in original LAB space
  original_distances <- farver::compare_colour(
    points,
    from_space = "lab",
    method = "cie2000"
  )

  # Simulate CVD versions and convert back to LAB
  cvd_colors <- list(
    deutan = colorspace::deutan(rgb_colors),
    protan = colorspace::protan(rgb_colors),
    tritan = colorspace::tritan(rgb_colors)
  )

  # Convert simulated colors back to LAB and calculate distances
  cvd_distances <- lapply(cvd_colors, function(rgb) {
    lab <- farver::convert_colour(rgb, from = "rgb", to = "lab")
    farver::compare_colour(lab, from_space = "lab", method = "cie2000")
  })

  # Extract upper triangular part of distance matrices
  list(
    original = original_distances[upper.tri(original_distances)],
    deutan = cvd_distances$deutan[upper.tri(cvd_distances$deutan)],
    protan = cvd_distances$protan[upper.tri(cvd_distances$protan)],
    tritan = cvd_distances$tritan[upper.tri(cvd_distances$tritan)]
  )
}
