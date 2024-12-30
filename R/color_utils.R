# Helper function to calculate distances for original and CVD versions
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
