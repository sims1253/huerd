# Build the standalone WebGL palette-manifold explorer (proof of concept)
#
# Reads the posterior produced by scripts/poc-palette-sampling.R and
# writes scripts/palette-manifold.html: a single self-contained file
# (three.js inlined) with every draw embedded, so it opens offline in
# any browser. All interaction -- weight sliders, axis switches,
# hover/click inspection, the density carpet -- runs client-side at
# 60 fps; nothing re-runs the sampler.
#
# Run from the repo root of the poc/stan-lbfgs worktree:
#   Rscript scripts/build-palette-manifold.R [art_dir [out_html [variant_label]]]
#
# Defaults build the standard posterior page; e.g. the pastel variant:
#   Rscript scripts/build-palette-manifold.R scripts/palette-posterior-pastel \
#     scripts/palette-manifold-pastel.html " · chroma/L-targeted posterior"

args <- commandArgs(trailingOnly = TRUE)
art <- if (length(args) >= 1) args[1] else "scripts/palette-posterior"
out_html <- if (length(args) >= 2) args[2] else "scripts/palette-manifold.html"
variant <- if (length(args) >= 3) args[3] else ""

stopifnot(requireNamespace("jsonlite", quietly = TRUE))

post <- readRDS(file.path(art, "palette-posterior.rds"))
met <- post$met

metric_cols <- c("min_dist", "cvd_safe", "min_deutan", "min_protan",
  "min_tritan", "mean_chroma", "lightness_spread")

pc <- stats::prcomp(scale(met[, metric_cols]))
pcs <- predict(pc)[, 1:3]
colnames(pcs) <- c("pc1", "pc2", "pc3")

palette_hex <- function(pal) {
  rgb <- farver::convert_colour(pal, "oklab", "rgb") / 255
  toupper(grDevices::rgb(rgb[, 1], rgb[, 2], rgb[, 3]))
}

draws <- lapply(seq_len(nrow(met)), function(i) {
  pal <- met$palette[[i]]
  hex <- palette_hex(pal)
  row <- as.list(round(met[i, metric_cols], 5))
  row$mean_L <- round(mean(pal[, 1]), 5) # mean OKLAB lightness
  row$pc1 <- round(pcs[i, 1], 5)
  row$pc2 <- round(pcs[i, 2], 5)
  row$pc3 <- round(pcs[i, 3], 5)
  row$chain <- met$.chain[i]
  row$draw <- met$.draw[i]
  row$i <- i - 1L
  row$hex <- hex
  row
})

data_json <- jsonlite::toJSON(draws, auto_unbox = TRUE, digits = 5)

template <- readLines("scripts/palette-manifold-template.html", warn = FALSE)
template <- paste(template, collapse = "\n")

three <- readLines("/tmp/poc/web/three.min.js", warn = FALSE)
orbit <- readLines("/tmp/poc/web/OrbitControls.js", warn = FALSE)
if (length(three) == 0 || length(orbit) == 0) {
  stop("three.js sources not found; adjust the path in this script")
}

# split-and-paste injection: sub()/gsub() replacement strings are not
# reliably literal (three.min.js is full of backslashes)
inject <- function(html, marker, content) {
  parts <- strsplit(html, marker, fixed = TRUE)[[1]]
  stopifnot(length(parts) == 2)
  paste0(parts[1], content, parts[2])
}

html <- template
html <- inject(html, "/*__THREE__*/", paste(three, collapse = "\n"))
html <- inject(html, "/*__ORBIT__*/", paste(orbit, collapse = "\n"))
html <- inject(html, "/*__DATA__*/", data_json)
html <- inject(html, "__NDRAWS__", as.character(length(draws)))
html <- inject(html, "__VTITLE__", variant)
html <- inject(html, "__VARIANT__", variant)

writeLines(html, out_html)
cat(sprintf(
  "wrote %s (%.1f MB, %d draws)\n",
  out_html, file.size(out_html) / 1e6, length(draws)
))
