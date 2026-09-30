# Regression tests for input validation, solver feasibility, RNG capture,
# all-fixed ordering, and the weights selector contract (review findings
# HR-02, HR-03, HR-04, HR-09, HR-10).

describe("max_iterations validation (HR-02)", {
  it("rejects zero, negative, non-finite, fractional, vector, and wrong-type budgets", {
    invalid_budgets <- list(
      0,
      -1,
      NA_real_,
      NaN,
      Inf,
      -Inf,
      1.5,
      c(5, 10),
      "10",
      NULL
    )

    for (budget in invalid_budgets) {
      expect_error(
        generate_palette(3, max_iterations = budget, progress = FALSE),
        regexp = "max_iterations",
        info = paste0("budget: ", deparse(budget))
      )
    }
  })

  it("accepts a single positive integer", {
    expect_no_error(
      generate_palette(3, max_iterations = 10, progress = FALSE)
    )
  })

  it("never forwards a non-positive evaluation budget to NLopt", {
    seen <- numeric()

    # Spy on the shared solver driver: an invalid budget reaching this layer
    # (possible only from internal callers) must still be normalized to a
    # positive maxeval, because maxeval = 0 disables NLopt's budget limit
    testthat::with_mocked_bindings(
      {
        huerd:::optimize_colors_constrained(
          matrix(c(0.5, 0, 0, 0.6, 0.1, 0.1), nrow = 2, byrow = TRUE),
          c(FALSE, FALSE),
          max_iterations = 0
        )
        huerd:::optimize_colors_constrained(
          matrix(c(0.5, 0, 0, 0.6, 0.1, 0.1), nrow = 2, byrow = TRUE),
          c(FALSE, FALSE),
          max_iterations = NA_real_
        )
      },
      `.run_nloptr_solver` = function(
        initial_free_params,
        eval_f,
        eval_grad_f = NULL,
        lower_bounds,
        upper_bounds,
        opts,
        error_prefix,
        evaluate_initial_objective = TRUE
      ) {
        seen <<- c(seen, opts$maxeval)
        list(
          solution = initial_free_params,
          status = 1,
          message = "solver spy",
          objective = 0
        )
      },
      .package = "huerd"
    )

    expect_true(all(seen >= 1))
    expect_equal(length(seen), 2)
  })
})

describe("solver feasibility for accepted lightness bounds (HR-03)", {
  it("optimizes instead of silently falling back when init L is below the solver bound", {
    withr::local_seed(42)

    palette <- generate_palette(
      3,
      init_lightness_bounds = c(0, 0.0001),
      fixed_aesthetic_influence = 0,
      max_iterations = 25,
      progress = FALSE
    )

    details <- attr(palette, "optimization_details")

    # -999 is the swallowed-solver-error fallback status
    expect_false(identical(details$nloptr_status, -999))
    expect_gte(details$iterations, 1)
  })

  it("clamps out-of-box initial parameters into the feasible box", {
    # a/b beyond the solver box: the starting point must be projected in
    result <- optimize_colors_constrained(
      matrix(c(0.5, 3, -3, 0.6, -3, 3), nrow = 2, byrow = TRUE),
      c(FALSE, FALSE),
      max_iterations = 5
    )

    expect_true(all(result$palette[, 2] >= -0.4 & result$palette[, 2] <= 0.4))
    expect_true(all(result$palette[, 3] >= -0.4 & result$palette[, 3] <= 0.4))
    expect_false(identical(result$details$nloptr_status, -999))
  })

  it("reports a mocked solver exception as a warning, not silent success", {
    testthat::local_mocked_bindings(
      `.run_nloptr_solver` = function(
        initial_free_params,
        eval_f,
        eval_grad_f = NULL,
        lower_bounds,
        upper_bounds,
        opts,
        error_prefix,
        evaluate_initial_objective = TRUE
      ) {
        # Emulate the real error handler: a caught solver exception
        # normalized to the -999 fallback shape
        list(
          solution = initial_free_params,
          status = -999,
          message = paste0(error_prefix, "mocked solver exception"),
          objective = NA_real_
        )
      },
      .package = "huerd"
    )

    expect_warning(
      result <- optimize_colors_constrained(
        matrix(c(0.5, 0, 0, 0.6, 0.1, 0.1), nrow = 2, byrow = TRUE),
        c(FALSE, FALSE),
        max_iterations = 5
      ),
      regexp = "solver failed"
    )

    expect_equal(result$details$nloptr_status, -999)
    expect_match(result$details$status_message, "mocked solver exception")
  })
})

describe("RNG state capture on first generation (HR-04)", {
  it("stores a non-NULL seed in a fresh session and replays exactly", {
    # Model a fresh session by removing the global RNG state
    seed_existed <- exists(
      ".Random.seed",
      envir = globalenv(),
      inherits = FALSE
    )
    if (seed_existed) {
      old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
      rm(".Random.seed", envir = globalenv())
    }
    on.exit(
      if (seed_existed) {
        assign(".Random.seed", old_seed, envir = globalenv())
      },
      add = TRUE
    )

    palette <- generate_palette(6, max_iterations = 50, progress = FALSE)

    expect_false(is.null(attr(palette, "generation_metadata")$seed))

    reproduced <- reproduce_palette(palette, progress = FALSE)
    expect_identical(as.character(reproduced), as.character(palette))
  })

  it("warns and preserves the caller RNG when reproducing legacy NULL-seed metadata", {
    set.seed(123)
    palette <- generate_palette(4, max_iterations = 10, progress = FALSE)
    legacy <- palette
    attr(legacy, "generation_metadata") <- utils::modifyList(
      attr(palette, "generation_metadata"),
      list(seed = NULL)
    )

    set.seed(321)
    before <- .Random.seed

    expect_warning(
      {
        reproduced <- reproduce_palette(legacy, progress = FALSE)
      },
      regexp = "no stored RNG state"
    )

    expect_true(inherits(reproduced, "huerd_palette"))
    expect_identical(.Random.seed, before)
  })
})

describe("all-fixed palettes follow the documented brightness sort (HR-09)", {
  it("sorts a reverse-brightness all-fixed palette ascending", {
    palette <- generate_palette(
      2,
      include_colors = c("#FFFFFF", "#000000"),
      progress = FALSE
    )

    expect_equal(as.character(palette), c("#000000", "#FFFFFF"))

    lightness <- farver::decode_colour(palette, to = "oklab")[, 1]
    expect_true(all(diff(lightness) >= 0))
  })

  it("preserves the fixed hex values, class, metrics, and metadata", {
    fixed <- c("#123456", "#FEDCBA", "#0A0A0A")
    palette <- generate_palette(3, include_colors = fixed, progress = FALSE)

    expect_true(all(fixed %in% as.character(palette)))
    expect_true(inherits(palette, "huerd_palette"))
    expect_false(is.null(attr(palette, "metrics")))
    expect_false(is.null(attr(palette, "generation_metadata")))
    expect_match(
      attr(palette, "optimization_details")$status_message,
      "All colors fixed"
    )
  })

  it("handles empty and single-color all-fixed palettes", {
    expect_length(generate_palette(0, progress = FALSE), 0)
    expect_equal(
      as.character(generate_palette(
        1,
        include_colors = "#FF0000",
        progress = FALSE
      )),
      "#FF0000"
    )
  })

  it("brand_palette() with n_total equal to the brand count is sorted too", {
    palette <- brand_palette(
      brand_colors = c("#FFFFFF", "#000000"),
      n_total = 2
    )

    expect_equal(as.character(palette), c("#000000", "#FFFFFF"))
  })
})

describe("weights selector contract (HR-10)", {
  it("rejects non-finite weights and duplicate objective names", {
    expect_error(
      generate_palette(3, weights = c(distance = NA_real_), progress = FALSE),
      regexp = "finite"
    )
    expect_error(
      generate_palette(
        3,
        weights = c(distance = 1, distance = 2),
        progress = FALSE
      ),
      regexp = "duplicate"
    )
  })

  it("warns when smooth objectives are requested with a minimax optimizer", {
    expect_warning(
      {
        palette <- generate_palette(
          4,
          weights = c(smooth_repulsion = 1),
          max_iterations = 10,
          progress = FALSE
        )
      },
      regexp = "require"
    )

    expect_equal(
      attr(palette, "generation_metadata")$effective_objective,
      "minimax_cvd"
    )
    expect_equal(attr(palette, "optimization_details")$objective, "minimax_cvd")
  })

  it("warns on mixed weights and selects a single objective", {
    expect_warning(
      {
        palette <- generate_palette(
          4,
          weights = c(smooth_repulsion = 1000, smooth_logsumexp = 0.001),
          optimizer = "nlopt_lbfgs",
          max_iterations = 10,
          progress = FALSE
        )
      },
      regexp = "single objective"
    )

    # Selection rule: any positive log-sum-exp weight selects log-sum-exp
    expect_equal(
      attr(palette, "generation_metadata")$effective_objective,
      "smooth_logsumexp"
    )
  })

  it("distance weights with minimax optimizers run without warnings", {
    expect_no_warning({
      palette <- generate_palette(
        4,
        weights = c(distance = 2.5),
        max_iterations = 10,
        progress = FALSE
      )
    })

    expect_equal(
      attr(palette, "generation_metadata")$effective_objective,
      "minimax_cvd"
    )
  })

  it("records the cvd_safe objective distinction in details", {
    withr::local_seed(99)

    plain <- generate_palette(
      4,
      cvd_safe = FALSE,
      max_iterations = 10,
      progress = FALSE
    )

    expect_equal(
      attr(plain, "optimization_details")$objective,
      "minimax_distance"
    )
  })
})
