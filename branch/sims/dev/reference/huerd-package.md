# huerd: Constrained Color Palette Generation with Aesthetic Guidance

The huerd package provides tools for generating perceptually distinct
color palettes with constraints. The main feature is the ability to
specify fixed colors that must be included in the final palette
unchanged. Generated colors are optimized for distinctness and CVD
safety, and can optionally adhere to the aesthetic "vibe" (Lightness and
Chroma profile) of the fixed colors.

## Main Functions

- [`generate_palette()`](https://sims1253.github.io/huerd/branch/sims/dev/reference/generate_palette.md):

  Generate a color palette with constraints.

- [`evaluate_palette()`](https://sims1253.github.io/huerd/branch/sims/dev/reference/evaluate_palette.md):

  Evaluate the quality of a color palette.

- [`simulate_palette_cvd()`](https://sims1253.github.io/huerd/branch/sims/dev/reference/simulate_palette_cvd.md):

  Simulate how a palette appears under CVD.

- [`is_cvd_safe()`](https://sims1253.github.io/huerd/branch/sims/dev/reference/is_cvd_safe.md):

  Quick check for CVD safety against a threshold.

## Color Spaces

The package uses the OK LAB color space by default for its perceptual
uniformity. Euclidean distance in OK LAB corresponds well to perceived
color differences. A Just Noticeable Difference (JND) in OK LAB is
approximately 0.02.

## Optimization Method

`huerd` treats palette generation as a box-constrained optimization
problem, handled by the `nloptr` package. The goal is to find an
arrangement of colors that maximizes a score based on perceptual
distance, CVD safety, and optional penalties for aesthetic or gamut
deviations.

## See also

Useful links:

- <https://github.com/sims1253/huerd>

- Report bugs at <https://github.com/sims1253/huerd/issues>

## Author

**Maintainer**: Maximilian Scholz <dev.scholz@mailbox.org>

Authors:

- Maximilian Scholz <dev.scholz@mailbox.org>
