# PoC demo: palette posterior sampling + metric space + weighted chooser
#
# Run from the repo root of the poc/stan-lbfgs worktree:
#   Rscript scripts/poc-palette-sampling.R
#
# Artifacts are written to scripts/palette-posterior/ .

dir.create("scripts/palette-posterior", showWarnings = FALSE)
art <- "scripts/palette-posterior"

pkgload::load_all(".")
library(ggplot2)

set.seed(11)
n_free <- 6
init <- farver::convert_colour(255 * matrix(runif(n_free * 3), ncol = 3), "rgb", "oklab")
fixed <- rep(FALSE, n_free)

# ---------------------------------------------------------------------------
# 1. Sample the palette posterior (HMC, Stan-style adaptation)
# ---------------------------------------------------------------------------
rds_path <- file.path(art, "palette-posterior.rds")
if (file.exists(rds_path)) {
  cat("reusing existing posterior:", rds_path, "\n")
  cached <- readRDS(rds_path)
  fit <- cached$fit
} else {
  t0 <- proc.time()
  fit <- sample_palette_posterior(
    init, fixed,
    chains = 4, warmup = 300, iter = 300,
    beta = 25, target_accept = 0.9, seed = 1234
  )
  cat(sprintf("sampling: %.1fs for %d chains\n",
    (proc.time() - t0)[["elapsed"]], length(fit$diagnostics)))
}
for (ch in seq_along(fit$diagnostics)) {
  d <- fit$diagnostics[[ch]]
  cat(sprintf(
    "  chain %d: step %.4f, accept %.2f, mean leapfrog steps %.0f, divergences %d\n",
    ch, d$step_size, d$mean_accept, d$mean_steps, d$divergences
  ))
}

# ---------------------------------------------------------------------------
# 2. Derived quantities: package metrics per draw + Stan-style summary
# ---------------------------------------------------------------------------
met <- palette_draw_metrics(fit)
summ <- summarize_palette_posterior(fit, met)
cat("\nposterior summary (Stan-style: mean, sd, ESS, split-Rhat):\n")
print(summ, row.names = FALSE)
cat("total divergences:", attr(summ, "divergences"), "\n")

write.csv(
  met[, setdiff(names(met), "palette")],
  file.path(art, "draw-metrics.csv"),
  row.names = FALSE
)

# ---------------------------------------------------------------------------
# 3. Weighted chooser: two user profiles, instant re-ranking of draws
# ---------------------------------------------------------------------------
ranked_cvd <- rank_palette_draws(met, c(
  cvd_safe = 3, min_dist = 1, mean_chroma = 0.5
))
ranked_balanced <- rank_palette_draws(met, c(
  cvd_safe = 1, min_dist = 1, mean_chroma = 1, lightness_spread = 1
))
cat("\ntop draw under CVD-first weights:  cvd_safe =", round(ranked_cvd$cvd_safe[1], 4),
  " min_dist =", round(ranked_cvd$min_dist[1], 4), "\n")
cat("top draw under balanced weights:   cvd_safe =", round(ranked_balanced$cvd_safe[1], 4),
  " min_dist =", round(ranked_balanced$min_dist[1], 4), "\n")

palette_hex <- function(pal) {
  rgb <- farver::convert_colour(pal, "oklab", "rgb") / 255
  grDevices::rgb(rgb[, 1], rgb[, 2], rgb[, 3])
}

swatch_plot <- function(pal, title = "") {
  hex <- palette_hex(pal)
  df <- data.frame(
    x = factor(seq_len(nrow(pal)), levels = seq_len(nrow(pal))),
    hex = hex
  )
  ggplot(df, aes(x, 1, fill = hex)) +
    geom_tile(color = "white", linewidth = 2) +
    scale_fill_identity() +
    ggtitle(title) +
    theme_void() +
    theme(plot.title = element_text(size = 10))
}

top9_cvd <- do.call(gridExtra::grid.arrange, c(
  lapply(seq_len(9), function(i) {
    swatch_plot(ranked_cvd$palette[[i]],
      sprintf("cvd %.3f | dist %.3f", ranked_cvd$cvd_safe[i], ranked_cvd$min_dist[i]))
  }),
  ncol = 3,
  top = "Top 9 palettes, CVD-first weights"
))
ggplot2::ggsave(file.path(art, "top9-cvd-first.png"), top9_cvd,
  width = 7, height = 5, dpi = 150)

# ---------------------------------------------------------------------------
# 4. Metric space: quality trade-offs and palette families
# ---------------------------------------------------------------------------
p_frontier <- ggplot(met, aes(min_dist, cvd_safe)) +
  geom_point(aes(color = rank_palette_draws(met, c(
    cvd_safe = 1, min_dist = 1
  ))$score), alpha = 0.5) +
  scale_color_viridis_c(option = "magma", name = "balanced score") +
  # Pareto frontier (cvd vs dist): draws not dominated on both axes
  geom_step(
    data = {
      o <- met[order(-met$min_dist), ]
      keep <- o[FALSE, ]
      best <- -Inf
      for (i in seq_len(nrow(o))) {
        if (o$cvd_safe[i] > best) {
          keep <- rbind(keep, o[i, ])
          best <- o$cvd_safe[i]
        }
      }
      keep[order(keep$min_dist), ]
    },
    aes(min_dist, cvd_safe),
    direction = "vh", color = "white", linewidth = 0.7, linetype = "dashed"
  ) +
  labs(
    title = "Palette posterior in metric space",
    subtitle = "Pareto frontier between normal-vision and CVD-safe separation",
    x = "min perceptual distance (normal vision)",
    y = "min CVD-safe distance"
  ) +
  theme_minimal(base_size = 10)
print(p_frontier)
ggsave(file.path(art, "metric-space-frontier.png"), p_frontier,
  width = 6, height = 4.5, dpi = 150)

# families: cluster draws in metric space
balanced <- rank_palette_draws(met, c(cvd_safe = 1, min_dist = 1))
met$score <- balanced$score[
  match(paste0(met$.chain, "-", met$.draw),
    paste0(balanced$.chain, "-", balanced$.draw))
]
met$.family <- factor(
  stats::kmeans(
    scale(met[, c("min_dist", "cvd_safe", "mean_chroma", "lightness_spread")]),
    centers = 4, nstart = 5
  )$cluster
)
p_families <- ggplot(met, aes(min_dist, cvd_safe, color = .family)) +
  geom_point(alpha = 0.6, size = 1.2) +
  labs(
    title = "Palette families in metric space (k-means, k = 4)",
    x = "min perceptual distance", y = "min CVD-safe distance",
    color = "family"
  ) +
  theme_minimal(base_size = 10)
print(p_families)
ggsave(file.path(art, "metric-space-families.png"), p_families,
  width = 6, height = 4.5, dpi = 150)

# representative palette per family
fam_reps <- lapply(split(seq_len(nrow(met)), met$.family), function(idx) {
  met[idx[which.max(met$score[idx])], ]
})
fam_plot <- do.call(gridExtra::grid.arrange, c(
  lapply(names(fam_reps), function(f) {
    swatch_plot(fam_reps[[f]]$palette[[1]],
      sprintf("family %s (n = %d)", f, sum(met$.family == f)))
  }),
  ncol = length(fam_reps),
  top = "Representative palette per family (best scored within family)"
))
ggsave(file.path(art, "families-representatives.png"), fam_plot,
  width = 8, height = 2.6, dpi = 150)

# PCA of the full metric matrix: the "shape" of the solution space
met_cols <- c("min_dist", "cvd_safe", "min_deutan", "min_protan",
  "min_tritan", "mean_chroma", "lightness_spread")
pc <- stats::prcomp(scale(met[, met_cols]))
met$.pc1 <- predict(pc)[, 1]
met$.pc2 <- predict(pc)[, 2]
p_pca <- ggplot(met, aes(.pc1, .pc2, color = .family)) +
  geom_point(alpha = 0.6, size = 1.2) +
  labs(
    title = "PCA of per-draw metric vectors",
    subtitle = sprintf("PC1 %.0f%%, PC2 %.0f%% of metric variance",
      100 * pc$sdev[1]^2 / sum(pc$sdev^2),
      100 * pc$sdev[2]^2 / sum(pc$sdev^2)),
    color = "family"
  ) +
  theme_minimal(base_size = 10)
print(p_pca)
ggsave(file.path(art, "metric-space-pca.png"), p_pca,
  width = 6, height = 4.5, dpi = 150)

saveRDS(list(fit = fit, met = met), file.path(art, "palette-posterior.rds"))
cat("\nartifacts written to", art, "\n")
