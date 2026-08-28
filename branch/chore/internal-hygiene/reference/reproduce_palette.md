# Reproduce Palette from Existing huerd_palette Object

Recreates an identical color palette from a previously generated
huerd_palette object using stored generation metadata.

## Usage

``` r
reproduce_palette(palette, progress = NULL, ...)
```

## Arguments

- palette:

  A huerd_palette object (result from
  [`generate_palette()`](https://sims1253.github.io/huerd/branch/chore/internal-hygiene/reference/generate_palette.md))
  containing generation metadata.

- progress:

  Logical. Show progress messages. Default is
  [`interactive()`](https://rdrr.io/r/base/interactive.html). If NULL,
  uses the progress setting from the original generation.

- ...:

  Additional arguments reserved for future use.

## Value

A character vector of hex colors with class `huerd_palette`, identical
to the input palette when reproduction is successful.

## Details

This function reads the generation metadata stored in the
`generation_metadata` attribute of a huerd_palette object and re-runs
[`generate_palette()`](https://sims1253.github.io/huerd/branch/chore/internal-hygiene/reference/generate_palette.md)
with the exact same parameters.

Reproducibility depends on the optimizer used:

- **Deterministic optimizers** ("nloptr_cobyla", "nlopt_neldermead",
  "nlopt_lbfgs"): Reproduction is always identical regardless of the
  random seed, as these algorithms produce the same results for the same
  inputs.

- **Stochastic optimizers** ("sann"): Reproduction requires restoring
  the random seed captured during the original generation. The seed is
  scoped using
  [`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html)
  to avoid mutating global state.

The function validates that the input object contains the necessary
metadata and provides informative error messages if reproduction fails.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a reproducible palette
set.seed(42)
original_palette <- generate_palette(
  n = 5,
  include_colors = c("#FF0000"),
  optimizer = "nloptr_cobyla",
  progress = FALSE
)

# Reproduce the exact same palette
reproduced_palette <- reproduce_palette(original_palette)

# Verify they are identical
identical(original_palette, reproduced_palette)

# Examine generation metadata
metadata <- attr(original_palette, "generation_metadata")
str(metadata)
} # }
```
