#' Calculate color distances using CAM16-UCS
#'
#' Calculates color distances in CAM16-UCS space for both original and
#' CVD-simulated colors. Input colors should be in LAB space.
#'
#' @param colors Matrix of xyz color values (n × 3)
#' @param viewing_conditions List with viewing condition parameters
#' @return List of distance vectors for original and CVD-simulated colors
#' @importFrom farver convert_colour
#' @importFrom colorspace deutan protan tritan
#' @import methods
#' @export
calculate_all_distances <- function(colors,
                                    viewing_conditions = list(
                                      L_A = 100, # cd/m²
                                      Y_b = 20, # relative luminance
                                      surround = "average"
                                    )) {
  # Validate input

  colors <- validate_color_input(colors, "xyz")

  # Validate viewing conditions
  if (!is.list(viewing_conditions)) {
    stop("viewing_conditions must be a list")
  }

  required_conditions <- c("L_A", "Y_b", "surround")
  missing_conditions <- setdiff(required_conditions, names(viewing_conditions))
  if (length(missing_conditions) > 0) {
    stop(
      "Missing required viewing conditions: ",
      paste(missing_conditions, collapse = ", ")
    )
  }

  if (!viewing_conditions$surround %in% c("average", "dim", "dark")) {
    stop("surround must be one of: average, dim, dark")
  }

  if (!is.numeric(viewing_conditions$L_A) || viewing_conditions$L_A <= 0) {
    stop("L_A must be a positive number")
  }

  if (!is.numeric(viewing_conditions$Y_b) ||
    viewing_conditions$Y_b < 0 ||
    viewing_conditions$Y_b > 100) {
    stop("Y_b must be between 0 and 100")
  }

  tryCatch(
    {
      # Convert to CAM16-UCS
      cam16_colors <- xyz_to_CIECAM16(
        colors,
        L_A = viewing_conditions$L_A,
        Y_b = viewing_conditions$Y_b,
        surround = viewing_conditions$surround,
        validate = FALSE
      )

      # Calculate original distances
      original_distances <- CIECAM16_distance_matrix(cam16_colors)

      # Convert to RGB for CVD simulation
      rgb_colors <- farver::convert_colour(colors, from = "xyz", to = "rgb")

      # Simulate CVD versions and calculate their distances
      cvd_distances <- list()
      for (cvd_type in c("deutan", "protan", "tritan")) {
        # Simulate CVD using colorspace package
        cvd_rgb <- switch(cvd_type,
          "deutan" = colorspace::deutan(rgb_colors),
          "protan" = colorspace::protan(rgb_colors),
          "tritan" = colorspace::tritan(rgb_colors)
        )

        # Convert back to XYZ
        cvd_xyz <- farver::convert_colour(cvd_rgb, from = "rgb", to = "xyz")

        # Convert to CAM16-UCS
        cvd_cam16 <- xyz_to_CIECAM16(
          cvd_xyz,
          L_A = viewing_conditions$L_A,
          Y_b = viewing_conditions$Y_b,
          surround = viewing_conditions$surround
        )

        # Calculate distances
        cvd_distances[[cvd_type]] <- CIECAM16_distance_matrix(cvd_cam16)
      }

      # Extract upper triangular part of distance matrices
      list(
        original = original_distances[upper.tri(original_distances)],
        deutan = cvd_distances$deutan[upper.tri(cvd_distances$deutan)],
        protan = cvd_distances$protan[upper.tri(cvd_distances$protan)],
        tritan = cvd_distances$tritan[upper.tri(cvd_distances$tritan)]
      )
    },
    error = function(e) {
      stop("Error calculating distances: ", e$message)
    }
  )
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
