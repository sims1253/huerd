# Simulate Palette Under Color Vision Deficiency

Shows how a color palette appears to individuals with different types of
color vision deficiency using physiologically accurate models from
`farver`.

## Usage

``` r
simulate_palette_cvd(
  colors,
  cvd_type = c("all", "protan", "deutan", "tritan"),
  severity = 1,
  plot = FALSE,
  ...
)
```

## Arguments

- colors:

  Character vector of hex colors.

- cvd_type:

  Character. Type of CVD: "protan", "deutan", "tritan", or "all".
  Default is "all".

- severity:

  Numeric. Severity of CVD from 0 (none) to 1 (complete). Default is
  1.0.

- plot:

  Logical. Whether to plot a comparison using base R graphics. Default
  is FALSE.

- ...:

  Additional arguments reserved for future use.

## Value

A list with simulated palettes. If `cvd_type` is "all", the list
contains elements `original`, `protan`, `deutan`, and `tritan`. For a
single CVD type, the list contains a single element named after that
type (e.g., `protan`). The output object also inherits from
`huerd_simulation_result`.

## Examples

``` r
palette_ex <- generate_palette(6, progress = FALSE)

# Simulate complete deuteranopia
deutan_palette <- simulate_palette_cvd(palette_ex, "deutan", severity = 1.0)
print(deutan_palette)
#> 
#> -- huerd CVD Simulation Result (Type: deutan, Severity: 1.00) --
#> Palette for: deutan
#>   [ 1] #443B00
#>   [ 2] #0038CC
#>   [ 3] #4D5874
#>   [ 4] #807753
#>   [ 5] #689BFA
#>   [ 6] #B3A249

# See all CVD types and plot them
if (interactive() && length(palette_ex) > 0) {
  all_cvd <- simulate_palette_cvd(palette_ex, "all", plot = TRUE)
}
```
