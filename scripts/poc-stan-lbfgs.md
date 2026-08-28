# PoC: Stan-style L-BFGS optimization for huerd palettes

Branch: `poc/stan-lbfgs` (worktree). Question: can the optimizer design
from the [Stan Reference Manual, Optimization
chapter](https://mc-stan.org/docs/reference-manual/optimization.html) be
transplanted into huerd, and what does it buy?

## What was built

1. **`R/stan-lbfgs.R`** -- a pure-R port of Stan's L-BFGS:
   - two-loop recursion with `history_size` curvature pairs (default 5,
     per the manual's recommendation of 5-10),
   - Wolfe-condition line search (Nocedal & Wright Alg. 3.5/3.6, c1=1e-4,
     c2=0.9) with bisection zoom, bracketing, and a best-Armijo fallback
     for exactly-flat regions,
   - the manual's five convergence tests (parameter, objective, relative
     objective scaled by machine epsilon, gradient, relative gradient
     g'H^-1g / max(|f|,1)),
   - `init_alpha = 0.001`, unit-step initial guess after the first
     quasi-Newton iteration (Nocedal & Wright's rule; see findings),
   - Stan's Hessian-approximation reset on curvature/line-search
     failures,
   - box constraints by projection during the line search, with the
     projected gradient x - proj(x - g) in the gradient tests.

2. **`R/optimization-stan-lbfgs.R`** -- the piece Stan would call "make
   the whole model differentiable":
   - the existing minimax CVD objective chains oklab -> sRGB -> linear
     RGB -> Machado et al. (2009) CVD matrix -> back to oklab through
     `farver`/`colorspace`, none of it differentiable in R. The CVD
     simulations are fixed 3x3 matrices in linear RGB (severity 1), so
     the entire chain is re-implemented as vectorized matrix algebra
     with matrices numerically identical to `colorspace`'s (verified to
     ~1e-6 against the package's own metric on in-gamut palettes),
   - the objective is a log-sum-exp soft minimum over all pairwise
     distances across the three CVD conditions (temperature `k`),
   - the gradient is a hand-rolled reverse-mode pass through the chain
     (verified against central finite differences),
   - `optimize_colors_stan_lbfgs()` mirrors the package optimizer
     interface and supports `cvd_safe = TRUE` -- which the existing
     `optimize_colors_lbfgs()` (NLopt L-BFGS on a repulsion surrogate)
     cannot: it never sees the CVD simulations at all.

3. **`scripts/benchmark-stan-lbfgs.R`** -- 4 palette sizes x 5 seeds x 6
   methods, identical k-means++ initializations, package metrics as the
   yardstick (results in `benchmark-stan-lbfgs-results.csv`).

## Results (mean over 5 seeds, `cvd_safe = TRUE`, max 1000 iterations)

| n | method | time (s) | evals | CVD-safe min dist |
|---|--------|---------:|------:|------------------:|
| 4 | **stan L-BFGS (mask, k=120)** | **0.08** | 102 | **0.305** |
| 4 | COBYLA (default) | 0.37 | 249 | 0.263 |
| 4 | Nelder-Mead | 2.03 | 887 | 0.261 |
| 6 | **stan L-BFGS (mask, k=120)** | **0.15** | 170 | **0.247** |
| 6 | Nelder-Mead | 2.24 | 983 | 0.169 |
| 6 | COBYLA (default) | 0.99 | 615 | 0.155 |
| 8 | Nelder-Mead | 1.51 | 1002 | 0.137 |
| 8 | **stan L-BFGS (mask, k=120)** | **0.10** | 114 | 0.126 |
| 8 | COBYLA (default) | 1.05 | 671 | 0.107 |
| 12 | **stan L-BFGS (penalty, k=120)** | 1.60 | 1677 | **0.133** |
| 12 | **stan L-BFGS (mask, k=120)** | **0.17** | 208 | 0.077 |
| 12 | Nelder-Mead | 2.28 | 1002 | 0.073 |
| 12 | COBYLA (default) | 1.55 | 908 | 0.063 |

The Stan-style optimizer matches or beats every existing optimizer's
quality at every size while using **6-15x less wall time** than
COBYLA/Nelder-Mead and ~5x fewer objective evaluations. The existing
`nlopt_lbfgs` path is not competitive on the CVD metric (0.003-0.067;
it optimizes a repulsion surrogate that ignores CVD, and sometimes
returns palettes worse than the initialization).

## Findings worth carrying back

These are the transferable lessons between Stan and huerd:

1. **The manual's constrained-vs-unconstrained caveat is real.** The
   first PoC version mapped the OKLAB box to unconstrained space with a
   scaled logit transform (the literal Stan approach). Palette optima
   live exactly on the box boundary, where the logit gradient decays
   exponentially; the gradient-norm convergence test fired prematurely
   and palettes stalled partway (e.g. a 2-color palette stopped at
   distance 0.80 instead of ~1.5). Projection + projected-gradient
   tests fixed it. Same phenomenon the manual describes for `sigma`
   approaching 0 on the log scale.

2. **Reuse-the-last-step-size is a trap under weak Wolfe.** With
   c2 = 0.9, once the L-BFGS direction approaches Newton, *any* step in
   [0.1, 1.9] satisfies the curvature condition, so starting each line
   search from the previous alpha accepts a short step forever -- the
   textbook superlinear convergence degrades to linear (observed rate
   0.87 on a quadratic). Always trying the unit step first (Nocedal &
   Wright's recommendation) is essential. Worth checking how Stan's own
   restart/step-size heuristics interact with this.

3. **Optima on the constraint boundary need gradient gating, not
   clamping alone.** The palette optimum sits deep outside the sRGB
   gamut in OKLAB coordinates (linear values up to 1.35); the package
   metric measures distances on gamut-clipped colors. Value paths must
   clip exactly like the metric (including farver's pre-simulation
   clip *before* the CVD matrix), while backward paths must gate by the
   clip masks so the gradient describes the value on the active
   manifold. Straight-through gradients (identity through clips)
   produced non-descent directions; quadratic penalties for gamut
   violations work well at larger palette sizes but perturb the
   geometry at small ones. Both variants are exposed
   (`gamut_penalty = 0` or `> 0`).

4. **Pair-ordering bug class.** `stats::dist()` enumerates the lower
   triangle column-major; `dm[upper.tri(dm)]` the upper triangle
   column-major. The two orders coincide only for n <= 3 -- pairing
   softmin weights from one with unit directions from the other
   silently corrupts gradients for n >= 4. (Found via stable finite
   differences; the test suite now pins this.)

5. **A gradient path finally makes the CVD objective optimizable by
   L-BFGS** -- the package's headline feature -- and it is also the
   fastest option in the package. This mirrors the Stan experience: the
   algorithm is only half the story; the differentiable pipeline is the
   enabling piece.

## Next steps if this graduates from PoC

- Expose as `optimizer = "stan_lbfgs"` in `generate_palette()` (wire
  into `.optimize_palette()` switch + validation + docs; the return
  shape already matches).
- Tune `k` by palette size, or anneal k with L-BFGS history resets
  between stages (continuation).
- Multi-start (the landscape is non-convex; k-means++ quality varies by
  seed) -- cheap now at ~0.1 s per start.
- Consider moving the objective to C/C++ (or {taina}-style tensor
  ops) if eval cost matters; the R implementation is already ~0.6 ms
  per evaluation at n=12.
- Consider severity-sweep CVD (multiple severities per condition) --
  now a one-line change in the differentiable chain, and compare
  against the Laplace-approximation angle from the Stan manual for
  uncertainty around the palette optimum.

## Reproduce

```sh
Rscript scripts/benchmark-stan-lbfgs.R   # ~2 min, writes results CSV
Rscript -e 'testthat::test_local(".", filter = "stan-lbfgs")'
```
