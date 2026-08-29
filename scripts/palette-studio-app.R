# Palette studio: the WebGL manifold as a live sampling UI (PoC)
#
# The static manifold pages visualize one pre-computed posterior. This app
# wraps the SAME page in Shiny so the sampler can be re-run from the
# browser: a sampling panel (beta, per-color chroma/lightness targets,
# chains/warmup/iter, seed, number of free colors, and locked hex colors
# that every proposed palette must include) sends parameters to R, which
# runs sample_palette_posterior() and streams the replacement draws back
# into the canvas via a custom message -- the page's loadData() rebuilds
# buffers, Pareto front, constraint ranges and the ranked list in place.
#
# Launch (from the poc/stan-lbfgs worktree root):
#   Rscript -e 'source("scripts/palette-studio-app.R");
#               app <- build_palette_studio();
#               shiny::runApp(app, host = "127.0.0.1", port = 7638)'

library(shiny)

# split-and-paste injection (sub() replacement strings are not literal for
# JS-heavy content); marker must occur exactly once
.inject <- function(html, marker, content) {
  parts <- strsplit(html, marker, fixed = TRUE)[[1]]
  stopifnot(length(parts) == 2)
  paste0(parts[1], content, parts[2])
}

.palette_hex <- function(pal) {
  rgb <- farver::convert_colour(pal, "oklab", "rgb") / 255
  toupper(grDevices::rgb(rgb[, 1], rgb[, 2], rgb[, 3]))
}

# per-draw JSON in exactly the shape the manifold page's DATA expects
draws_to_json <- function(met) {
  metric_cols <- c("min_dist", "cvd_safe", "min_deutan", "min_protan",
    "min_tritan", "mean_chroma", "lightness_spread")
  pcs <- stats::predict(stats::prcomp(scale(met[, metric_cols])))[, 1:3]
  colnames(pcs) <- c("pc1", "pc2", "pc3")
  draws <- lapply(seq_len(nrow(met)), function(i) {
    pal <- met$palette[[i]]
    row <- as.list(round(met[i, metric_cols], 5))
    row$mean_L <- round(mean(pal[, 1]), 5)
    row$pc1 <- round(pcs[i, 1], 5)
    row$pc2 <- round(pcs[i, 2], 5)
    row$pc3 <- round(pcs[i, 3], 5)
    row$chain <- met$.chain[i]
    row$draw <- met$.draw[i]
    row$i <- i - 1L
    row$hex <- .palette_hex(pal)
    row
  })
  jsonlite::toJSON(draws, auto_unbox = TRUE, digits = 5)
}

#' Parse locked colors ("#0B7285, #FFE066") to an OKLAB matrix (0 rows if
#' empty). Invalid input raises an error with a user-facing message.
parse_locked_hex <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(matrix(numeric(0), ncol = 3))
  }
  parts <- trimws(strsplit(text, ",")[[1]])
  parts <- parts[nzchar(parts)]
  if (!all(grepl("^#[0-9A-Fa-f]{6}$", parts))) {
    stop("locked colors must be comma-separated #RRGGBB hex values")
  }
  farver::convert_colour(t(grDevices::col2rgb(parts)), "rgb", "oklab")
}

#' Sample a posterior from user parameters and package it for the client.
#' Returns list(payload = JSON string, subtitle = controls-panel subtitle).
run_sampling_payload <- function(locked_oklab, n_free, beta, chroma_target,
                                 l_target, target_weight, chains, warmup,
                                 iter, seed) {
  set.seed(seed)
  init_free <- farver::convert_colour(
    255 * matrix(stats::runif(n_free * 3), ncol = 3), "rgb", "oklab")
  init_full <- rbind(locked_oklab, init_free)
  fixed_mask <- c(rep(TRUE, nrow(locked_oklab)), rep(FALSE, n_free))

  qw <- list(normal = 1, cvd = 1, target_weight = target_weight)
  if (!is.null(chroma_target)) qw$chroma_target <- chroma_target
  if (!is.null(l_target)) qw$l_target <- l_target

  fit <- sample_palette_posterior(
    init_full, fixed_mask,
    chains = chains, warmup = warmup, iter = iter,
    beta = beta, target_accept = 0.9, seed = seed, quality_weights = qw
  )
  met <- palette_draw_metrics(fit)

  fmt <- function(x) if (is.null(x)) "off" else format(x)
  subtitle <- paste0(
    nrow(met), " posterior draws \u00b7 \u03b2 ", beta,
    " \u00b7 chroma\u2192", fmt(chroma_target),
    " \u00b7 L\u2192", fmt(l_target),
    " \u00b7 strength ", target_weight,
    if (nrow(locked_oklab) > 0) {
      paste0(" \u00b7 ", nrow(locked_oklab), " locked")
    } else {
      ""
    }
  )
  list(payload = draws_to_json(met), subtitle = subtitle)
}

.studio_css <- "
#studio label { color: #aebccd; font-size: 11px; margin-bottom: 2px;
  font-weight: 400; }
#studio .form-control { background: #151b27; color: #dfe6f1;
  border: 1px solid #2c3950; font-size: 12px; height: 28px; padding: 3px 8px; }
#studio .btn { background: #1b2433; color: #ffd166; border: 1px solid #3a4a66;
  width: 100%; font-size: 12px; }
#studio .btn:hover { border-color: #7cc4ff; color: #ffd166; }
#studio .irs-line { border-color: #2c3950; background: #1b2433; }
#studio .irs-bar { background: #7cc4ff; }
#studio .irs-from, #studio .irs-to, #studio .irs-single { background: #1b2433; }
#studio .checkbox-inline { color: #aebccd; font-size: 11px; }
#studio .shiny-input-container { width: 100%; }
#studio-head { display: flex; align-items: center; gap: 8px; cursor: pointer;
  user-select: none; padding: 2px 0; }
#studio-head h2 { margin: 0; flex: 1; font-size: 12px; color: #aebccd;
  letter-spacing: 0.4px; font-weight: 600; }
#studio-head:hover h2 { color: #dfe6f1; }
#studio-chev { color: #7cc4ff; width: 12px; text-align: center; }
#studio .studio-status { font-size: 11px; color: #7ce8c9;
  font-variant-numeric: tabular-nums; max-width: 45%; overflow: hidden;
  text-overflow: ellipsis; white-space: nowrap; }
#studio-locks { font-family: ui-monospace, monospace; font-size: 11px;
  margin: 2px 0 8px; min-height: 15px; letter-spacing: 0.2px; }
.studio-help { font-size: 10.5px; color: #8b98ad; line-height: 1.55;
  margin-top: 10px; border-top: 1px solid rgba(120, 150, 200, 0.18);
  padding-top: 8px; }
.studio-help b { color: #aebccd; font-weight: 600; }
"

build_palette_studio <- function(repo_path = ".") {
  pkgload::load_all(repo_path, quiet = TRUE)

  tpl <- paste(readLines(
    file.path(repo_path, "scripts/palette-manifold-template.html"),
    warn = FALSE), collapse = "\n")

  post <- readRDS(file.path(
    repo_path, "scripts/palette-posterior/palette-posterior.rds"))
  initial_json <- draws_to_json(post$met)

  three <- paste(readLines("/tmp/poc/web/three.min.js", warn = FALSE), collapse = "\n")
  orbit <- paste(readLines("/tmp/poc/web/OrbitControls.js", warn = FALSE), collapse = "\n")

  html <- tpl
  html <- .inject(html, "/*__THREE__*/", three)
  html <- .inject(html, "/*__ORBIT__*/", orbit)
  html <- .inject(html, "/*__DATA__*/", initial_json)
  html <- .inject(html, "__NDRAWS__", as.character(nrow(post$met)))
  html <- .inject(html, "__VTITLE__", " studio")
  html <- .inject(html, "__VARIANT__", " \u00b7 studio (re-sample from the panel)")
  # whisker-style mounts: htmlTemplate evaluates {{ }} as R expressions;
  # headContent() is provided by Shiny itself (serving swaps in its deps)
  html <- sub("<head>", "<head>{{ headContent() }}", html, fixed = TRUE)
  html <- .inject(html, '<div class="panel" id="detail"></div>',
    '<div class="panel" id="detail"></div>\n{{ studio_panel }}')
  tpl_file <- tempfile(fileext = ".html")
  writeLines(html, tpl_file)

  studio_panel <- tags$div(
    class = "panel", id = "studio",
    style = paste0("top:16px;left:50%;transform:translateX(-50%);",
      "width:400px;max-height:calc(100vh - 32px);overflow-y:auto"),
    tags$style(HTML(.studio_css)),
    # clickable header: folds the body away so the panel collapses to a
    # slim bar (status stays visible while folded)
    tags$div(
      id = "studio-head",
      tags$span(id = "studio-chev", "\u25b8"),
      h2("sampling parameters"),
      tags$span(id = "studio-status", "idle", class = "studio-status")
    ),
    tags$div(
      id = "studio-body",
      fluidRow(
        column(6, tags$div(title = paste("How many colors the sampler proposes.",
          "Final palette size = locked + free colors."),
          numericInput("n_free", "free colors", 6,
            min = 2, max = 8, step = 1, width = "100%"))),
        column(6, tags$div(title = paste("RNG seed. Same seed and same",
          "parameters reproduce the exact posterior."),
          numericInput("seed", "seed", 1234,
            min = 1, step = 1, width = "100%")))
      ),
      tags$div(title = paste("Hex colors kept verbatim in every proposed",
        "palette (fixed parameters); only the other colors are sampled."),
        textInput("locked", "locked colors (hex, comma-separated)",
          placeholder = "#0B7285, #FFE066", width = "100%")),
      div(id = "studio-locks"),
      tags$div(title = paste("Posterior concentration: low beta keeps a",
        "diverse, exploratory sample; high beta focuses tightly on the",
        "best palettes."),
        sliderInput("beta", "\u03b2 (concentration)", 5, 60, 25,
          step = 1, width = "100%")),
      tags$div(title = paste("Soft per-color saturation goal in OKLAB units",
        "(~0 = gray, 0.4 = vivid). Unchecked, separation pushes colors to",
        "the gamut edge -- maximum chroma."),
        checkboxInput("use_ct", "chroma target (per color)", TRUE,
          width = "100%")),
      conditionalPanel("input.use_ct",
        sliderInput("ct", NULL, 0.05, 0.45, 0.09, step = 0.01,
          width = "100%")),
      tags$div(title = paste("Soft per-color brightness goal (OKLAB L,",
        "0 = black, 1 = white). Also prevents near-black anchor colors."),
        checkboxInput("use_lt", "lightness target (per color)", TRUE,
          width = "100%")),
      conditionalPanel("input.use_lt",
        sliderInput("lt", NULL, 0.3, 0.95, 0.72, step = 0.01,
          width = "100%")),
      tags$div(title = paste("How hard palettes are pulled toward the",
        "targets, traded off against perceptual separation."),
        sliderInput("tw", "target strength", 1, 30, 8, step = 1,
          width = "100%")),
      fluidRow(
        column(4, tags$div(title = paste("Independent samplers whose draws",
          "are pooled (also used for split-Rhat)."),
          numericInput("chains", "chains", 4,
            min = 1, max = 8, width = "100%"))),
        column(4, tags$div(title = paste("Stan-style step-size adaptation,",
          "discarded -- these draws never reach the canvas."),
          numericInput("warmup", "warmup", 300,
            min = 50, max = 1000, step = 50, width = "100%"))),
        column(4, tags$div(title = "Kept draws per chain (draws = chains x iter).",
          numericInput("iter", "iter", 300,
            min = 50, max = 1000, step = 50, width = "100%")))
      ),
      actionButton("btn_sample", "sample posterior", width = "100%",
        onclick = paste0(
          "document.getElementById('studio-status').textContent=",
          "'sampling... R is busy until it finishes (defaults take ~1 min)'")),
      tags$div(class = "studio-help",
        HTML(paste0(
          "<b>\u03b2</b> concentrates the posterior: low = diverse candidates, ",
          "high = sharp focus on top palettes. ",
          "<b>Targets</b> are per-color soft goals (chroma 0\u20130.4, L 0\u20131); ",
          "unchecked, separation drives colors to the gamut edge. ",
          "<b>Strength</b> trades targets against separation. ",
          "<b>Warmup</b> adapts step sizes and is discarded. ",
          "Hover any control for details."))
      ),
    ),
    tags$script(HTML(paste0(
      "(function(){",
      "var head=document.getElementById('studio-head'),",
      "body=document.getElementById('studio-body'),",
      "chev=document.getElementById('studio-chev'),",
      "open=false;",
      "function applyFold(){",
      "body.style.display=open?'block':'none';",
      "chev.textContent=open?'\u25be':'\u25b8';}",
      "head.addEventListener('click',function(){open=!open;applyFold();});",
      "applyFold();",
      "window.locksPreview=function(){",
      "var el=document.getElementById('locked'),",
      "out=document.getElementById('studio-locks');",
      "if(!el||!out)return;",
      "var parts=(el.value||'').split(',').map(function(s){return s.trim();})",
      ".filter(Boolean);",
      "out.innerHTML=parts.map(function(p){",
      "return /^#[0-9A-Fa-f]{6}$/.test(p)?",
      "'<span style=\"color:'+p.toUpperCase()+'\">\u25a0 '+p.toUpperCase()+'</span>'",
      ":'<span style=\"color:#ff8f8f\">\u2717 '+p+'</span>';",
      "}).join(' &nbsp; ');};",
      "document.addEventListener('input',function(e){",
      "if(e.target&&e.target.id==='locked')window.locksPreview();});",
      "window.locksPreview();",
      "})();"
    )))
  )

  ui <- htmlTemplate(tpl_file, studio_panel = studio_panel)

  server <- function(input, output, session) {
    # scripted sampling from the URL, mirroring the page's client-side
    # ?axes/?w/?pin scripting: ?sample=n_free,chains,warmup,iter,seed with
    # optional &locked=hex,hex &ct=0.09 &lt=0.72 &tw=8. Runs once on load;
    # lets a single link reproduce an exact posterior (reports, CI).
    ran_scripted <- FALSE
    observe({
      if (ran_scripted) return()
      qs <- session$clientData$url_search
      cat("[studio] url_search:", qs, "\n")
      if (is.null(qs) || !nzchar(qs)) return()
      ran_scripted <<- TRUE
      pq <- shiny::parseQueryString(qs)
      cat("[studio] pq$sample:", pq$sample, "| pq$locked:", pq$locked, "\n")
      if (is.null(pq$sample)) return()
      parts <- as.numeric(strsplit(pq$sample, ",")[[1]])
      cat("[studio] parts:", paste(parts, collapse = "/"), "\n")
      if (length(parts) != 5 || any(!is.finite(parts))) {
        session$sendCustomMessage("manifoldStatus",
          "error: ?sample=n_free,chains,warmup,iter,seed")
        return()
      }
      locked_txt <- if (is.null(pq$locked)) "" else pq$locked
      locked <- tryCatch(parse_locked_hex(locked_txt), error = function(e) e)
      if (inherits(locked, "error")) {
        session$sendCustomMessage("manifoldStatus",
          paste("error:", conditionMessage(locked)))
        return()
      }
      to_num <- function(x) if (is.null(x)) NULL else as.numeric(x)
      session$sendCustomMessage("manifoldStatus",
        "scripted sampling from URL params...")
      res <- tryCatch(
        run_sampling_payload(
          locked_oklab = locked, n_free = parts[1], beta = 25,
          chroma_target = to_num(pq$ct), l_target = to_num(pq$lt),
          target_weight = if (is.null(pq$tw)) 8 else as.numeric(pq$tw),
          chains = parts[2], warmup = parts[3], iter = parts[4],
          seed = parts[5]),
        error = function(e) e)
      cat("[studio] scripted sampling done, error =", inherits(res, "error"), "\n")
      if (inherits(res, "error")) {
        session$sendCustomMessage("manifoldStatus",
          paste("error:", conditionMessage(res)))
        return()
      }
      session$sendCustomMessage("manifoldData", res)
    })

    observeEvent(input$btn_sample, {
      locked <- tryCatch(parse_locked_hex(input$locked),
        error = function(e) e)
      if (inherits(locked, "error")) {
        session$sendCustomMessage("manifoldStatus",
          paste("error:", conditionMessage(locked)))
        return()
      }
      ct <- if (isTRUE(input$use_ct)) input$ct else NULL
      lt <- if (isTRUE(input$use_lt)) input$lt else NULL
      session$sendCustomMessage("manifoldStatus", sprintf(
        "sampling: %d chain(s) \u00d7 %d iters, \u03b2=%g, %d locked...",
        input$chains, input$warmup + input$iter, input$beta, nrow(locked)))
      res <- tryCatch(
        run_sampling_payload(
          locked_oklab = locked, n_free = input$n_free, beta = input$beta,
          chroma_target = ct, l_target = lt, target_weight = input$tw,
          chains = input$chains, warmup = input$warmup,
          iter = input$iter, seed = input$seed),
        error = function(e) e)
      if (inherits(res, "error")) {
        session$sendCustomMessage("manifoldStatus",
          paste("error:", conditionMessage(res)))
        return()
      }
      session$sendCustomMessage("manifoldData", res)
    })
  }

  shinyApp(ui, server)
}

app <- build_palette_studio()
