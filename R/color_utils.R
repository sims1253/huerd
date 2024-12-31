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

#' Calculate Color Distances for a Palette
#'
#' Calculates pairwise color distances for a palette in both original and
#' simulated color vision deficiency (CVD) spaces.
#'
#' @param colors A character vector of color names or hex codes.
#' @return A list containing four vectors of pairwise color distances:
#'   \item{original}{Distances in original color space}
#'   \item{deutan}{Distances in deuteranomaly simulation}
#'   \item{protan}{Distances in protanomaly simulation}
#'   \item{tritan}{Distances in tritanomaly simulation}
#'
#' @importFrom grDevices col2rgb
#' @importFrom farver convert_colour
calculate_palette_distances <- function(colors) {
  # Convert colors to LAB space
  rgb_vals <- t(grDevices::col2rgb(colors))
  lab_vals <- farver::convert_colour(rgb_vals, from = "rgb", to = "lab")
  calculate_all_distances(lab_vals)
}

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
  distances <- lapply(palettes, calculate_palette_distances)

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
