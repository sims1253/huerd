# huerd: Herding hues
A discrete color palette generation tool that creates perceptually distinct colors optimized for both normal and color vision deficient viewers.

## Installation

```R
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("sims1253/huerd")
```

## Usage

### Basic Color Palette Generation

Generate a palette of perceptually distinct colors using inverse square law repulsion:

```R
library(huerd)

# Generate 8 colors
colors <- sqrt_repulsion(
  n_colors = 8,
  max_iterations = 1000,
  color_force_factor = 200,
  boundary_force_factor = 400,
  center_force_factor = 150
)

# Convert LAB colors to hex format
hex_colors <- farver::encode_colour(colors, from = "lab")
```

### Color Vision Deficiency (CVD) Optimization

Take the generated palette and optimize it for color vision deficiency:

```R
# Optimize colors for CVD viewers
cvd_optimized <- adjust_for_cvd(
  lab_colors = colors,
  max_iterations = 1000,
  convergence_threshold = 10,
  min_iterations = 100
)

# Convert optimized colors to hex
hex_optimized <- farver::encode_colour(cvd_optimized, from = "lab")
```

### Animating the Process

Create an animated visualization of the color optimization process:

```R
# Generate animation states
states <- sqrt_repulsion(
  n_colors = 8,
  max_iterations = 1000,
  return_states = TRUE,
  save_every = 5
)

# Create animation
animate_repulsion(
  states = states,
  n_colors = 8,
  filename = "color_optimization.gif",
  show_force_field = TRUE
)
```

### Complete Example

Here's a complete example that combines all three features:

```R
library(huerd)

# 1. Generate initial palette with animation states
repulsion_states <- sqrt_repulsion(
  n_colors = 8,
  max_iterations = 1000,
  base_colors = c("#FF0000", "#00FF00"), # Optional fixed colors
  return_states = TRUE,
  save_every = 5,
  color_force_factor = 200,
  boundary_force_factor = 400,
  center_force_factor = 150
)

# 2. Get final colors and optimize for CVD
cvd_adjustment_states <- adjust_for_cvd(
  lab_colors = tail(states, 1)[[1]],
  max_iterations = 1000,
  convergence_threshold = 10,
  min_iterations = 100
)
cvd_optimized_color <- tail(cvd_adjustment_states, 1)[[1]]

# 3. Create animation of the process
animate_repulsion(
  states = c(repulsion_states, cvd_adjustment_states),
  n_colors = 8,
  base_colors = c("#FF0000", "#00FF00"),
  filename = "palette_generation.gif",
  show_force_field = TRUE
)

# Convert final colors to hex format
hex_colors <- farver::encode_colour(cvd_optimized_color, from = "lab")
colorspace::swatchplot(hex_colors)
print(hex_colors)
```
