#' Calculate Color Distances for Original and Color Vision Deficiency (CVD) Simulations
#'
#' This function calculates color distances between pairs of colors in the original
#' color space and in simulated color vision deficiency (CVD) spaces for deuteranomaly,
#' protanomaly, and tritanomaly.
#'
#' @param points A matrix or data frame of LAB color values, where each row represents
#'   a color and columns represent L, A, and B values respectively.
#'
#' @return A list containing four vectors of color distances:
#'   \item{original}{Distances between original color pairs}
#'   \item{deutan}{Distances between color pairs as perceived by individuals with deuteranomaly}
#'   \item{protan}{Distances between color pairs as perceived by individuals with protanomaly}
#'   \item{tritan}{Distances between color pairs as perceived by individuals with tritanomaly}
#'   Each vector contains the upper triangular part of the distance matrix.
#'
#' @importFrom farver convert_colour compare_colour
#' @importFrom colorspace deutan protan tritan
#'
calculate_all_distances <- function(points) {
  # Convert LAB to RGB
  rgb_colors <- farver::convert_colour(points, from = "lab", to = "rgb")

  # Calculate distances for original colors
  original_distances <- farver::compare_colour(
    points,
    from_space = "lab",
    method = "cie2000"
  )

  # Simulate CVD versions using colorspace package
  deutan_rgb <- colorspace::deutan(rgb_colors)
  protan_rgb <- colorspace::protan(rgb_colors)
  tritan_rgb <- colorspace::tritan(rgb_colors)

  # Convert back to LAB for distance calculations
  deutan_lab <- farver::convert_colour(deutan_rgb, from = "rgb", to = "lab")
  protan_lab <- farver::convert_colour(protan_rgb, from = "rgb", to = "lab")
  tritan_lab <- farver::convert_colour(tritan_rgb, from = "rgb", to = "lab")

  # Calculate distances for CVD versions
  deutan_distances <- farver::compare_colour(
    deutan_lab,
    from_space = "lab",
    method = "cie2000"
  )
  protan_distances <- farver::compare_colour(
    protan_lab,
    from_space = "lab",
    method = "cie2000"
  )
  tritan_distances <- farver::compare_colour(
    tritan_lab,
    from_space = "lab",
    method = "cie2000"
  )

  list(
    original = original_distances[upper.tri(original_distances)],
    deutan = deutan_distances[upper.tri(deutan_distances)],
    protan = protan_distances[upper.tri(protan_distances)],
    tritan = tritan_distances[upper.tri(tritan_distances)]
  )
}

#' Calculate Color Distances for a Palette
#'
#' This function calculates color distances between pairs of colors in a given palette,
#' both in the original color space and in simulated color vision deficiency (CVD) spaces.
#'
#' @param colors A vector of color names or hex codes representing the palette.
#'
#' @return A list containing four vectors of color distances:
#'   \item{original}{Distances between original color pairs}
#'   \item{deutan}{Distances between color pairs as perceived by individuals with deuteranomaly}
#'   \item{protan}{Distances between color pairs as perceived by individuals with protanomaly}
#'   \item{tritan}{Distances between color pairs as perceived by individuals with tritanomaly}
#'   Each vector contains the upper triangular part of the distance matrix.
#'
#' @importFrom grDevices col2rgb
#' @importFrom farver convert_colour
#'
calculate_palette_distances <- function(colors) {
  # Convert to LAB
  rgb_vals <- t(col2rgb(colors))
  lab_vals <- farver::convert_colour(rgb_vals, from = "rgb", to = "lab")

  # Calculate all distances
  calculate_all_distances(lab_vals)
}

#' Calculate Mean of Bottom Quartile
#'
#' This function calculates the mean of the bottom quartile of a given set of distances.
#'
#' @param distances A numeric vector of distances.
#'
#' @return A numeric value representing the mean of the bottom quartile of the input distances.
#'
#' @details
#' The function sorts the input distances, determines the size of the bottom quartile,
#' extracts the bottom quartile, and calculates its mean.
#'
mean_bottom_quartile <- function(distances) {
  sorted_distances <- sort(distances)
  quartile_size <- length(distances) %/% 4 # integer division
  bottom_quartile <- sorted_distances[1:quartile_size]
  mean(bottom_quartile)
}

#' Calculate Reference Distances for Color Palettes
#'
#' This function generates reference color palettes and calculates their color distances
#' for both original and color vision deficiency (CVD) simulations. It provides a benchmark
#' for comparing other color palettes.
#'
#' @param n_colors An integer specifying the number of colors to generate in each palette.
#'
#' @return A list containing reference distances for three color palettes (batlow, brewer, and tableau).
#'   Each palette's data is a nested list with four elements:
#'   \itemize{
#'     \item original: Mean of bottom quartile distances for original colors
#'     \item deutan: Mean of bottom quartile distances for deuteranomaly simulation
#'     \item protan: Mean of bottom quartile distances for protanomaly simulation
#'     \item tritan: Mean of bottom quartile distances for tritanomaly simulation
#'   }
#'
#' @importFrom scico scico
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales hue_pal
#'
get_reference_distances <- function(n_colors) {
  # Get palettes using built-in functions
  batlow <- scico::scico(n_colors, palette = "batlow", categorical = TRUE)
  brewer_set2 <- RColorBrewer::brewer.pal(min(8, n_colors), "Set2")
  tableau <- scales::hue_pal()(n_colors)

  # Get distances for each palette
  batlow_dist <- calculate_palette_distances(batlow)
  brewer_dist <- calculate_palette_distances(brewer_set2)
  tableau_dist <- calculate_palette_distances(tableau)

  # Return means for each palette and CVD version
  list(
    batlow = list(
      original = mean_bottom_quartile(batlow_dist$original),
      deutan = mean_bottom_quartile(batlow_dist$deutan),
      protan = mean_bottom_quartile(batlow_dist$protan),
      tritan = mean_bottom_quartile(batlow_dist$tritan)
    ),
    brewer = list(
      original = mean_bottom_quartile(brewer_dist$original),
      deutan = mean_bottom_quartile(brewer_dist$deutan),
      protan = mean_bottom_quartile(brewer_dist$protan),
      tritan = mean_bottom_quartile(brewer_dist$tritan)
    ),
    tableau = list(
      original = mean_bottom_quartile(tableau_dist$original),
      deutan = mean_bottom_quartile(tableau_dist$deutan),
      protan = mean_bottom_quartile(tableau_dist$protan),
      tritan = mean_bottom_quartile(tableau_dist$tritan)
    )
  )
}
