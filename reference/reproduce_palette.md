# Reproduce Palette from Existing huerd_palette Object

Recreates an identical color palette from a previously generated
huerd_palette object using the stored generation metadata. This function
enables perfect scientific reproducibility by reusing the exact same
parameters and random seed that were used in the original generation.

## Usage

``` r
reproduce_palette(palette, progress = NULL)
```

## Arguments

- palette:

  A huerd_palette object (result from
  [`generate_palette()`](https://sims1253.github.io/huerd/reference/generate_palette.md))
  containing generation metadata.

- progress:

  Logical. Show progress messages. Default is
  [`interactive()`](https://rdrr.io/r/base/interactive.html). If NULL,
  uses the progress setting from the original generation.

## Value

A character vector of hex colors with class `huerd_palette`, identical
to the input palette when reproduction is successful.

## Details

This function reads the generation metadata stored in the
`generation_metadata` attribute of a huerd_palette object and re-runs
[`generate_palette()`](https://sims1253.github.io/huerd/reference/generate_palette.md)
with the exact same parameters. When a random seed was captured during
original generation, the reproduction will be identical. For
deterministic optimizers like "nlopt_direct", reproduction should always
be identical regardless of random seed.

The function validates that the input object contains the necessary
metadata and provides informative error messages if reproduction fails
due to missing metadata or package version incompatibilities.

For scientific reproducibility, it's recommended to use deterministic
optimizers like "nlopt_direct" when reproducibility is critical.

## Compatibility

- Reproduction is most reliable within the same package version

- Package version changes may affect reproducibility

- Missing or incomplete metadata will prevent reproduction

- The function handles backward compatibility with older huerd_palette
  objects that may lack metadata

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a reproducible palette
set.seed(42)
original_palette <- generate_palette(
  n = 5,
  include_colors = c("#FF0000"),
  optimizer = "nlopt_direct",
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
