# PoC demo: palette posterior with saturation/brightness targets ("pastel" variant)
#
# The default Boltzmann posterior concentrates at the gamut edge: maximizing
# perceptual separation rewards chroma, so every draw ends up maximally
# saturated (sample minimum mean chroma ~0.21). This script re-samples the
# posterior with soft targets -- mean OKLAB chroma ~0.09, mean lightness
# ~0.72 -- to show that "conditions on the proposed palettes" are priors on
# the same machinery, not a different algorithm.
#
# Run from the repo root of the poc/stan-lbfgs worktree:
#   Rscript scripts/poc-palette-sampling-pastel.R
#
# Artifacts are written to scripts/palette-posterior-pastel/ .

dir.create("scripts/palette-posterior-pastel", showWarnings = FALSE)
art <- "scripts/palette-posterior-pastel"

pkgload::load_all(".")

set.seed(11)
n_free <- 6
init <- farver::convert_colour(255 * matrix(runif(n_free * 3), ncol = 3), "rgb", "oklab")
fixed <- rep(FALSE, n_free)

t0 <- proc.time()
fit <- sample_palette_posterior(
  init, fixed,
  chains = 4, warmup = 300, iter = 300,
  beta = 25, target_accept = 0.9, seed = 1234,
  quality_weights = list(
    normal = 1, cvd = 1,
    chroma_target = 0.09, l_target = 0.72, target_weight = 8
  )
)
cat(sprintf("sampling: %.1fs for %d chains\n",
  (proc.time() - t0)[["elapsed"]], length(fit$diagnostics)))
for (ch in seq_along(fit$diagnostics)) {
  d <- fit$diagnostics[[ch]]
  cat(sprintf(
    "  chain %d: step %.4f, accept %.2f, mean leapfrog steps %.0f, divergences %d\n",
    ch, d$step_size, d$mean_accept, d$mean_steps, d$divergences
  ))
}

met <- palette_draw_metrics(fit)
summ <- summarize_palette_posterior(fit, met)
print(summ, row.names = FALSE)
cat("total divergences:", attr(summ, "divergences"), "\n")
cat("mean chroma range:", round(range(met$mean_chroma), 3), "\n")

mean_L <- vapply(met$palette, function(p) mean(p[, 1]), numeric(1))
met$mean_L <- mean_L
cat("mean lightness range:", round(range(mean_L), 3), "\n")

write.csv(
  met[, setdiff(names(met), "palette")],
  file.path(art, "draw-metrics.csv"),
  row.names = FALSE
)
saveRDS(list(fit = fit, met = met), file.path(art, "palette-posterior.rds"))
cat("artifacts written to", art, "\n")
