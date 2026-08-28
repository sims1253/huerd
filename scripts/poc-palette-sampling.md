# PoC 2: Palette posterior sampling + interactive chooser (Stan-style)

Branch: `poc/stan-lbfgs`. Companion to `scripts/poc-stan-lbfgs.md`
(the optimization PoC). Question: can the *sampling* half of the Stan
idea complement the optimizer -- a posterior draw as one candidate
palette, derived metrics for every draw, an interactive chooser that
re-ranks draws under user weights, and palettes visualized in
metric space?

## Architecture

```
differentiable quality Q(palette)          [PoC 1]
        |
        v  pi(palette) ∝ exp(beta * Q)     Boltzmann posterior
HMC in logit-transformed OKLAB box         Stan's constraining transform,
  + log-Jacobian (always, when sampling)     as the manual's MAP-vs-Laplace
  + U(-2, 2) unconstrained init              discussion prescribes
  + dual-averaging step-size warmup          Hoffman & Gelman, as in Stan
  + max-treedepth-style step cap             static-HMC analogue
        |
        v
per-draw derived quantities                package metrics + per-condition
        |                                   CVD minima + aesthetics
        v
weighted ranking (instant, no re-sampling)  the "chooser"
        +
metric-space visualization                  Pareto fronts, palette families
```

* **Sampler** (`R/palette-sampling.R::sample_palette_posterior`): HMC
  with Stan's adaptation constants (gamma = 0.05, t0 = 10, kappa = 0.75,
  adapt_delta = 0.8 default), Stan's U(-2, 2) init, divergent-transition
  accounting, 4 chains. A `max_steps` cap (64) plays the role of Stan's
  max treedepth -- without it, a small adapted step size makes
  `sim_time / step` leapfrog steps and the runtime explodes.
* **Density**: `palette_log_density` -- weighted soft-min separation
  (normal + CVD) at inverse temperature beta, plus optional
  lightness-spread / mean-chroma terms, in logit space with the full
  log-Jacobian. Gradient verified against finite differences (1e-9).
* **Derived quantities**: `palette_draw_metrics` -- hard package metrics
  per draw (`objective_min_perceptual_dist`,
  `objective_min_cvd_safe_dist`), per-condition CVD minima via the
  verified differentiable chain, mean chroma, lightness spread.
* **Diagnostics**: `summarize_palette_posterior` -- mean/sd/ESS/split-Rhat
  per metric, Geyer initial-positive-sequence ESS, divergence count.
* **Chooser**: `rank_palette_draws` -- min-max-scaled metrics, weighted
  score, instant re-ranking. The sampler never re-runs.

## Demo run (n = 6 free colors, 4 chains, 300 + 300, beta = 25)

~167 s sampling in pure R; 7 divergences total; step size ~0.008
(the soft-min density is stiff; k = 40).

```
metric             mean      sd     ess    rhat
min_dist          0.371   0.079    72.7   1.002
cvd_safe          0.087   0.043   110.8   1.053
min_deutan        0.119   0.050   113.5   1.047
min_protan        0.119   0.052   148.4   1.026
min_tritan        0.116   0.044   270.8   1.016
mean_chroma       0.329   0.040    54.3   1.053
lightness_spread  0.766   0.124    42.1   1.126
```

What the metric space shows (artifacts in `scripts/palette-posterior/`):

* `metric-space-frontier.png` -- one dense cloud with an empty
  upper-right corner: normal-vision separation and CVD-safe separation
  are in genuine tension, with a visible Pareto arc. Choosing weights
  is choosing where on that arc to sit.
* `metric-space-families.png` + `families-representatives.png` --
  k-means over the metric vector separates recognizable palette
  families (e.g. saturated-hue-spread vs lightness-ladder types).
* `metric-space-pca.png` -- PC1/PC2 of the 7-metric matrix; the
  dominant axis is overall separation quality, the second trades
  chroma against lightness spread.
* `top9-cvd-first.png` -- the top-9 draws under CVD-first weights are
  visually distinct, usable categorical palettes (not 9 jittered
  copies of one optimum): the posterior gives a *space* of solutions.

Weight profiles matter, exactly as hoped:

```
CVD-first  (cvd 3 : dist 1 : chroma 0.5): top draw cvd_safe 0.199, min_dist 0.488
Balanced   (1 : 1 : 1 : 1):               top draw cvd_safe 0.154, min_dist 0.617
```

## The WebGL manifold explorer

For a more fluid experience than Shiny can offer,
`scripts/build-palette-manifold.R` generates
`scripts/palette-manifold.html`: a **single self-contained file**
(three.js r128 inlined, ~1 MB, opens offline in any browser) with all
1200 draws embedded. Everything runs client-side at 60 fps:

* glowing additive-blended particle cloud over a live-computed
  wireframe **density carpet** (client-side 2D histogram + blur over
  the current X/Z axes),
* weight sliders recolor/rescale points and re-rank the top-8 palette
  cards instantly -- no server round-trip, unlike the Shiny app,
* X/Y/Z axis dropdowns over every metric and PC1-3 with eased
  position-lerp transitions between axis choices,
* orbit/zoom/pan (auto-spinning until first interaction), hover
  tooltips with palette previews, click-to-pin detail cards with hex
  codes, and a `?selftest` mode that asserts the data, scoring,
  ranking, carpet, reweighting and axis-switch logic inside a real
  browser (verified in headless Chrome; screenshot at
  `scripts/palette-posterior/manifold-explorer.png`).

## Interactive chooser

`scripts/palette-chooser-app.R` (shiny): weight sliders over the four
headline metrics, a metric-space scatter colored by the current
weighted score (click any point to inspect that palette), the top-N
palettes as swatch rows with hex labels, and a stats hint line.
Verified headless via `shiny::testServer` and served over HTTP.
Launch:

```r
source("scripts/palette-chooser-app.R")   # builds `app` from the RDS
shiny::runApp(app)
```

## Findings

1. **The Jacobian rule from the manual is exactly what makes sampling
   work here.** Including log|dx/du| = log(width) + log(s(1-s)) both
   targets the right distribution and (as a bonus) keeps chains off
   the box boundary in u-space. The beta = 0 sanity test (density =
   Jacobian only => x uniform in the box) passes to Monte Carlo error,
   a clean end-to-end check of transform + leapfrog + Metropolis.
2. **Stiffness is the cost of sharp soft-minima.** Adapted step ~0.008
   at k = 40, beta = 25 means trajectories hit the 64-step cap --
   acceptance is high (0.96+) but ESS/iteration is modest (42-271 over
   1200 draws). Options if this graduates: lower k while sampling,
   Stan-style diagonal mass adaptation, or sample at lower beta and
   importance-reweight. The ESS/s lever the package author knows from
   Stan applies directly.
3. **Permutation symmetry = mixture-model label switching.** The
   posterior is invariant under relabeling the n colors (n! modes).
   Harmless here (all derived metrics are permutation-invariant), but
   worth documenting; the same phenomenon as label switching in Stan
   mixture models.
4. **Sampling and optimization compose**: the optimizer (PoC 1) finds
   the best single palette in 0.1-0.2 s; the sampler (PoC 2) maps the
   solution *space* around and beyond it. Initializing the density at
   the optimizer's solution with lower beta would concentrate draws
   where they matter (Stan's warm-start/Laplace intuition from the
   same manual chapter).

## Reproduce

```sh
Rscript scripts/poc-palette-sampling.R    # ~3 min; reuses the RDS if present
Rscript -e 'testthat::test_local(".", filter = "palette-sampling")'
Rscript -e 'source("scripts/palette-chooser-app.R"); shiny::runApp(app)'
```
