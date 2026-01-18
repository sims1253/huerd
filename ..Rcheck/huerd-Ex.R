pkgname <- "huerd"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('huerd')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("brand_palette")
### * brand_palette

flush(stderr()); flush(stdout())

### Name: brand_palette
### Title: Create a palette with brand colors
### Aliases: brand_palette

### ** Examples

# Corporate palette with 2 brand colors expanded to 6
brand_palette(
  brand_colors = c("#003366", "#FF6600"),
  n_total = 6
)




cleanEx()
nameEx("evaluate_palette")
### * evaluate_palette

flush(stderr()); flush(stdout())

### Name: evaluate_palette
### Title: Evaluate Palette Quality
### Aliases: evaluate_palette

### ** Examples

pal <- generate_palette(5, progress = FALSE)
metrics <- evaluate_palette(pal)
print(metrics) # Uses custom print method

# The performance_ratio compares the achieved min distance to an estimated maximum
# metrics$distances$performance_ratio



cleanEx()
nameEx("export_palette")
### * export_palette

flush(stderr()); flush(stdout())

### Name: export_palette
### Title: Export palette to various formats
### Aliases: export_palette

### ** Examples

pal <- generate_palette(5, progress = FALSE)

# Get as hex vector (default)
export_palette(pal)

# CSS custom properties
export_palette(pal, format = "css")

# With custom names
export_palette(pal, format = "css",
               names = c("primary", "secondary", "accent", "bg", "text"))

# JSON for web applications
export_palette(pal, format = "json")




cleanEx()
nameEx("generate_palette")
### * generate_palette

flush(stderr()); flush(stdout())

### Name: generate_palette
### Title: Generate Optimal Color Palette using Pure Minimax Optimization
### Aliases: generate_palette

### ** Examples

# Simple optimal palette
palette <- generate_palette(5, progress = FALSE)
print(palette)

# Brand-constrained palette
brand_palette <- generate_palette(
  n = 6,
  include_colors = c("#4A6B8A", "#E5A04C"),
  progress = FALSE
)

# Using specific optimizer (deterministic)
optimizer_palette <- generate_palette(
  n = 4,
  optimizer = "nloptr_cobyla",
  progress = FALSE
)

# Using simulated annealing (stochastic, excellent quality)
set.seed(42)  # For reproducibility
sann_palette <- generate_palette(
  n = 4,
  optimizer = "sann",
  progress = FALSE
)

# Using DIRECT algorithm (deterministic global, best for scientific reproducibility)
direct_palette <- generate_palette(
  n = 4,
  optimizer = "nlopt_direct",
  progress = FALSE
)

# Using Nelder-Mead algorithm (derivative-free local, good alternative to COBYLA)
neldermead_palette <- generate_palette(
  n = 4,
  optimizer = "nlopt_neldermead",
  progress = FALSE
)

# Using smooth optimization with L-BFGS (efficient for larger palettes)
smooth_palette <- generate_palette(
  n = 12,
  weights = c(smooth_repulsion = 1),
  optimizer = "nlopt_lbfgs",
  progress = FALSE
)

# Using alternative smooth objective
logsumexp_palette <- generate_palette(
  n = 6,
  weights = c(smooth_logsumexp = 1),
  optimizer = "nlopt_lbfgs",
  progress = FALSE
)

# Evaluate quality
evaluation <- evaluate_palette(brand_palette)
cat("Min distance:", evaluation$distances$min, "\n")
cat("Performance:", evaluation$distances$performance_ratio * 100, "%\n")

# Comprehensive analysis
plot_palette_analysis(brand_palette)




cleanEx()
nameEx("interpret_palette_quality")
### * interpret_palette_quality

flush(stderr()); flush(stdout())

### Name: interpret_palette_quality
### Title: Interpret palette quality in plain language
### Aliases: interpret_palette_quality

### ** Examples

pal <- generate_palette(6, progress = FALSE)
interpret_palette_quality(pal)




cleanEx()
nameEx("plot.huerd_palette")
### * plot.huerd_palette

flush(stderr()); flush(stdout())

### Name: plot.huerd_palette
### Title: Plot method for huerd palettes
### Aliases: plot.huerd_palette

### ** Examples

pal <- generate_palette(6, progress = FALSE)

# Simple swatch display
plot(pal)

# Full analysis dashboard
plot(pal, type = "analysis")




cleanEx()
nameEx("plot_palette_analysis")
### * plot_palette_analysis

flush(stderr()); flush(stdout())

### Name: plot_palette_analysis
### Title: Comprehensive Palette Analysis Dashboard
### Aliases: plot_palette_analysis

### ** Examples

colors <- c("#ff0000", "#00ff00", "#0000ff")
plot_palette_analysis(colors)



cleanEx()
nameEx("quick_palette")
### * quick_palette

flush(stderr()); flush(stdout())

### Name: quick_palette
### Title: Quick palette generation with sensible defaults
### Aliases: quick_palette

### ** Examples

# Simple 5-color palette
quick_palette(5)

# Include brand colors
quick_palette(6, brand_colors = c("#1f77b4", "#ff7f0e"))

# Fast generation for exploration
quick_palette(8, quality = "fast")

# Light colors for dark backgrounds
quick_palette(5, lightness = "light")




cleanEx()
nameEx("reproduce_palette")
### * reproduce_palette

flush(stderr()); flush(stdout())

### Name: reproduce_palette
### Title: Reproduce Palette from Existing huerd_palette Object
### Aliases: reproduce_palette

### ** Examples

## Not run: 
##D # Create a reproducible palette
##D set.seed(42)
##D original_palette <- generate_palette(
##D   n = 5,
##D   include_colors = c("#FF0000"),
##D   optimizer = "nlopt_direct",
##D   progress = FALSE
##D )
##D 
##D # Reproduce the exact same palette
##D reproduced_palette <- reproduce_palette(original_palette)
##D 
##D # Verify they are identical
##D identical(original_palette, reproduced_palette)
##D 
##D # Examine generation metadata
##D metadata <- attr(original_palette, "generation_metadata")
##D str(metadata)
## End(Not run)




cleanEx()
nameEx("scale_color_huerd")
### * scale_color_huerd

flush(stderr()); flush(stdout())

### Name: scale_color_huerd
### Title: Discrete color scale using huerd palettes
### Aliases: scale_color_huerd scale_colour_huerd scale_fill_huerd

### ** Examples

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Basic usage - automatic palette generation
  ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
    geom_point(size = 3) +
    scale_color_huerd()

  # With brand colors
  ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
    geom_point(size = 3) +
    scale_color_huerd(brand_colors = c("#1f77b4", "#ff7f0e"))

  # Using a pre-generated palette
  my_palette <- generate_palette(5, progress = FALSE)
  ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
    geom_point(size = 3) +
    scale_color_huerd(palette = my_palette)

  # Fill scale for bar charts
  ggplot(mpg, aes(class, fill = class)) +
    geom_bar() +
    scale_fill_huerd() +
    theme(legend.position = "none")
}




cleanEx()
nameEx("simulate_palette_cvd")
### * simulate_palette_cvd

flush(stderr()); flush(stdout())

### Name: simulate_palette_cvd
### Title: Simulate Palette Under Color Vision Deficiency
### Aliases: simulate_palette_cvd

### ** Examples

palette_ex <- generate_palette(6, progress = FALSE)

# Simulate complete deuteranopia
deutan_palette <- simulate_palette_cvd(palette_ex, "deutan", severity = 1.0)
print(deutan_palette)

# See all CVD types and plot them
if (interactive() && length(palette_ex) > 0) {
  all_cvd <- simulate_palette_cvd(palette_ex, "all", plot = TRUE)
}




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
