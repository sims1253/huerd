# Benchmark: Stan-style L-BFGS on the differentiable CVD-safe objective
# vs the package's existing optimizers, from identical initializations.
#
# Run from the repo root of the poc/stan-lbfgs worktree:
#   Rscript scripts/benchmark-stan-lbfgs.R

start <- Sys.time()
pkgload::load_all(".")

sizes <- c(4, 6, 8, 12)
seeds <- 1:5

methods <- list(
  list(
    id = "cobyla",
    label = "COBYLA (package default)",
    run = function(init, iters) {
      optimize_colors_constrained(init, rep(FALSE, nrow(init)), iters, cvd_safe = TRUE)
    },
    evals = function(d) d$iterations
  ),
  list(
    id = "neldermead",
    label = "Nelder-Mead (nlopt)",
    run = function(init, iters) {
      optimize_colors_nlopt_neldermead(init, rep(FALSE, nrow(init)), iters, cvd_safe = TRUE)
    },
    evals = function(d) d$iterations
  ),
  list(
    id = "sann",
    label = "SANN",
    run = function(init, iters) {
      optimize_colors_sann(init, rep(FALSE, nrow(init)), iters, cvd_safe = TRUE)
    },
    evals = function(d) d$iterations
  ),
  list(
    id = "nlopt_lbfgs",
    label = "NLopt L-BFGS (repulsion, no CVD)",
    run = function(init, iters) {
      optimize_colors_lbfgs(init, rep(FALSE, nrow(init)), iters)
    },
    evals = function(d) d$iterations
  ),
  list(
    id = "stan_lbfgs_mask",
    label = "Stan L-BFGS, k=120, mask backward",
    run = function(init, iters) {
      optimize_colors_stan_lbfgs(init, rep(FALSE, nrow(init)), iters,
        cvd_safe = TRUE, k = 120, gamut_penalty = 0
      )
    },
    evals = function(d) d$f_evals
  ),
  list(
    id = "stan_lbfgs_pen",
    label = "Stan L-BFGS, k=120, gamut penalty 1e3",
    run = function(init, iters) {
      optimize_colors_stan_lbfgs(init, rep(FALSE, nrow(init)), iters,
        cvd_safe = TRUE, k = 120, gamut_penalty = 1e3
      )
    },
    evals = function(d) d$f_evals
  )
)

make_init <- function(n, seed) {
  set.seed(seed)
  initialize_colors(
    n, NULL, "k-means++",
    list(
      lightness_bounds = c(0.2, 0.9),
      kmeans_chroma_filter_params = list(apply_filter = FALSE)
    ),
    c(0.2, 0.9)
  )
}

rows <- list()
for (n in sizes) {
  for (seed in seeds) {
    init <- make_init(n, seed)
    init_cvd <- objective_min_cvd_safe_dist(init)
    init_dist <- objective_min_perceptual_dist(init)
    for (m in methods) {
      gc(verbose = FALSE)
      t0 <- proc.time()
      res <- m$run(init, 1000L)
      el <- (proc.time() - t0)[["elapsed"]]
      rows[[length(rows) + 1]] <- data.frame(
        n = n,
        seed = seed,
        method = m$id,
        label = m$label,
        time_s = el,
        evals = m$evals(res$details),
        cvd_safe = objective_min_cvd_safe_dist(res$palette),
        min_dist = objective_min_perceptual_dist(res$palette),
        init_cvd_safe = init_cvd,
        init_min_dist = init_dist,
        stringsAsFactors = FALSE
      )
    }
    cat(sprintf("done n=%d seed=%d (%s)\n", n, seed,
      format(Sys.time(), "%H:%M:%S")))
  }
}

results <- do.call(rbind, rows)
write.csv(results, "scripts/benchmark-stan-lbfgs-results.csv", row.names = FALSE)

cat("\n==== aggregate (mean over seeds) ====\n")
agg <- aggregate(
  cbind(time_s, evals, cvd_safe, min_dist) ~ method + label + n,
  data = results, FUN = mean
)
agg <- agg[order(agg$n, -agg$cvd_safe), ]
print(
  transform(agg,
    time_s = round(time_s, 2),
    evals = round(evals),
    cvd_safe = round(cvd_safe, 4),
    min_dist = round(min_dist, 4)
  ),
  row.names = FALSE
)

cat("\ntotal elapsed:", round(as.numeric(Sys.time() - start, units = "secs"), 1), "s\n")
