# Regression tests for gamut awareness across initialization, the minimax
# objective, and the sRGB-parameterized L-BFGS path (review finding HR-01).

describe("gamut helpers", {
  it(".oklab_in_gamut rejects colors that clip on conversion", {
    clipped <- rbind(
      c(0.5, 0.4, 0.4),
      c(0.6, 0.4, 0.4)
    )
    in_gamut <- rbind(
      c(0.5, 0.1, 0.0),
      c(0.8, -0.1, 0.1)
    )

    expect_equal(.oklab_in_gamut(clipped), c(FALSE, FALSE))
    expect_equal(.oklab_in_gamut(in_gamut), c(TRUE, TRUE))
    expect_equal(.oklab_in_gamut(matrix(numeric(0), ncol = 3)), logical(0))
  })

  it(".project_oklab_to_gamut maps clipped colors to their delivered values", {
    # Both points satisfy the optimizer's OKLAB box but clip to the same
    # saturated red on delivery
    clipped <- rbind(c(0.5, 0.4, 0.4), c(0.6, 0.4, 0.4))

    projected <- .project_oklab_to_gamut(clipped)
    hex <- .oklab_to_hex(projected)

    expect_equal(hex[1], hex[2])
    expect_equal(as.matrix(stats::dist(projected))[1, 2], 0)
  })

  it("objective_min_perceptual_dist scores delivered colors, not raw coordinates", {
    clipped_pair <- rbind(c(0.5, 0.4, 0.4), c(0.6, 0.4, 0.4))

    # Raw-coordinate distance is 0.1, but both colors deliver as the same
    # hex, so the objective must not award separation credit
    expect_equal(objective_min_perceptual_dist(clipped_pair), 0)

    # The in-gamut control keeps its raw distance
    ok_pair <- rbind(c(0.5, 0.1, 0.0), c(0.6, 0.1, 0.0))
    expect_gte(objective_min_perceptual_dist(ok_pair), 0.09)
  })

  it("objective_min_perceptual_dist returns 0 for non-finite inputs", {
    expect_equal(
      objective_min_perceptual_dist(
        matrix(c(0.5, 0, 0, Inf, Inf, Inf), nrow = 2, byrow = TRUE)
      ),
      0
    )
  })
})

describe("k-means++ initialization gamut filter", {
  it("returns only in-gamut centers on the primary path", {
    withr::local_seed(42)

    centers <- initialize_kmeans_plus_plus(
      n_free = 5,
      fixed_colors_oklab = NULL,
      lightness_bounds = c(0.2, 0.9),
      chroma_filter_params = list(apply_filter = FALSE),
      base_init_lightness_bounds = c(0.2, 0.9)
    )

    expect_equal(nrow(centers), 5)
    expect_true(all(.oklab_in_gamut(centers)))
  })

  it("still initializes when the lightness range barely overlaps the gamut", {
    # Near-black lightness range: almost every uniform candidate is out of
    # gamut, so strict filtering alone cannot supply enough candidates
    withr::local_seed(1)

    centers <- initialize_kmeans_plus_plus(
      n_free = 3,
      fixed_colors_oklab = NULL,
      lightness_bounds = c(0, 0.0001),
      chroma_filter_params = list(apply_filter = FALSE),
      base_init_lightness_bounds = c(0, 0.0001)
    )

    expect_gte(nrow(centers), 1)
    expect_true(all(.oklab_in_gamut(centers)))
  })
})

describe("analytic OKLAB <-> sRGB transforms", {
  it("matches farver's rgb -> oklab conversion to quantization precision", {
    set.seed(7)
    srgb <- matrix(runif(300), ncol = 3)

    analytic <- .srgb_to_oklab(srgb)
    via_farver <- farver::convert_colour(srgb * 255, from = "rgb", to = "oklab")

    # ~1e-4 level agreement: an order of magnitude below the OKLAB error of
    # 8-bit hex quantization
    expect_lte(max(abs(analytic - via_farver)), 2e-4)
  })

  it("maps the unit sRGB cube into the gamut", {
    set.seed(8)
    srgb <- matrix(runif(300), ncol = 3)

    expect_true(all(.oklab_in_gamut(.srgb_to_oklab(srgb))))
  })

  it("round-trips in-gamut colors through oklab -> srgb -> oklab", {
    set.seed(9)
    oklab <- .project_oklab_to_gamut(matrix(runif(300, 0, 1), ncol = 3))

    round_trip <- .srgb_to_oklab(.oklab_to_srgb(oklab))

    expect_lte(max(abs(round_trip - oklab)), 5e-4)
  })

  it("chains OKLAB gradients to sRGB consistently with finite differences", {
    set.seed(10)
    srgb <- matrix(c(0.2, 0.5, 0.8, 0.6, 0.1, 0.9), ncol = 3, byrow = TRUE)
    grad <- matrix(rnorm(6), ncol = 3)

    analytic <- as.vector(t(.oklab_grad_wrt_srgb(srgb, grad)))

    # Flatten row-major to match the byrow reshaping inside f()
    p0 <- as.vector(t(srgb))
    f <- function(p) {
      sum(grad * .srgb_to_oklab(matrix(p, ncol = 3, byrow = TRUE)))
    }
    for (i in seq_along(p0)) {
      h <- 1e-6
      p_plus <- p0
      p_minus <- p0
      p_plus[i] <- p_plus[i] + h
      p_minus[i] <- p_minus[i] - h
      numeric_grad <- (f(p_plus) - f(p_minus)) / (2 * h)
      expect_equal(analytic[i], numeric_grad, tolerance = 1e-4)
    }
  })

  it("keeps gradients finite at the black corner", {
    srgb <- matrix(c(0, 0, 0), nrow = 1)
    grad <- matrix(c(1, 1, 1), nrow = 1)

    expect_true(all(is.finite(.oklab_grad_wrt_srgb(srgb, grad))))
  })
})

describe("delivered palettes have distinct colors", {
  it("normal-vision minimax optimization yields distinct hex colors", {
    withr::local_seed(11)
    palette <- generate_palette(
      6,
      cvd_safe = FALSE,
      max_iterations = 600,
      progress = FALSE
    )

    expect_equal(length(unique(as.character(palette))), 6)
    expect_gte(attr(palette, "metrics")$distances$min, 0.05)
  })

  it("smooth repulsion (L-BFGS, sRGB parameterization) yields distinct hex colors", {
    withr::local_seed(12)
    palette <- generate_palette(
      6,
      weights = c(smooth_repulsion = 1),
      optimizer = "nlopt_lbfgs",
      max_iterations = 800,
      progress = FALSE
    )

    expect_equal(length(unique(as.character(palette))), 6)
    expect_true(
      is.finite(attr(palette, "optimization_details")$final_objective_value)
    )
  })

  it("smooth log-sum-exp (L-BFGS) yields distinct hex colors", {
    withr::local_seed(13)
    palette <- generate_palette(
      6,
      weights = c(smooth_logsumexp = 1),
      optimizer = "nlopt_lbfgs",
      max_iterations = 800,
      progress = FALSE
    )

    expect_equal(length(unique(as.character(palette))), 6)
  })

  it("L-BFGS optimizes the objective it reports", {
    withr::local_seed(14)
    palette <- generate_palette(
      4,
      weights = c(smooth_repulsion = 1),
      optimizer = "nlopt_lbfgs",
      max_iterations = 500,
      progress = FALSE
    )

    details <- attr(palette, "optimization_details")
    expect_equal(details$objective, "smooth_repulsion")
    expect_equal(
      attr(palette, "generation_metadata")$effective_objective,
      "smooth_repulsion"
    )
  })
})
