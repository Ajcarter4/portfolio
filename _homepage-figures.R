# Builds the homepage SVG figures from the real project data.
# Sourced by index.qmd. The leading underscore keeps Quarto from rendering it as a page.

suppressMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
})

# ---------- helpers ----------------------------------------------------------

rescale_to <- function(v, from, to) {
  to[1] + (v - from[1]) / (from[2] - from[1]) * (to[2] - to[1])
}
fmt <- function(v) formatC(v, format = "f", digits = 1)
fmt_n <- function(v) format(v, big.mark = ",")

# wrap HTML so Pandoc passes it through untouched
raw_html <- function(x) cat("\n```{=html}\n", x, "\n```\n", sep = "")

# ---------- data -------------------------------------------------------------

hosp <- read_csv("HCAHPS-Hospital.csv",
                 na = c("Not Available", "Not Applicable"),
                 show_col_types = FALSE)

hmean <- hosp |>
  filter(!is.na(`HCAHPS Linear Mean Value`)) |>
  pivot_wider(id_cols = `Facility ID`,
              names_from = `HCAHPS Answer Description`,
              values_from = `HCAHPS Linear Mean Value`)

staff <- tibble(
  x = as.numeric(hmean$`Staff responsiveness - linear mean score`),
  y = as.numeric(hmean$`Overall hospital rating - linear mean score`)
) |>
  drop_na()

fit     <- lm(y ~ x, data = staff)
r2      <- summary(fit)$r.squared
n_hosp  <- nrow(staff)
staff   <- staff |> mutate(fitted = fitted(fit), resid = y - fitted)

flower <- read_csv("Encelia.csv", show_col_types = FALSE) |>
  drop_na(number_rays, stem_length)

# ---------- hero figure: staff responsiveness vs overall rating --------------

hero_figure <- function(n_show = 160) {
  W <- 560; H <- 380
  L <- 52; R <- 20; T <- 20; B <- 46
  xr <- c(58, 100); yr <- c(62, 100)

  # biggest miss: the hospital furthest below its predicted rating
  outlier <- staff |> slice_min(resid, n = 1, with_ties = FALSE)

  set.seed(2026)
  shown <- staff |>
    slice_sample(n = n_show - 1) |>
    mutate(
      # integer scores overlap a lot, so jitter by up to 0.3 for display only
      xj = x + runif(n(), -0.3, 0.3),
      yj = y + runif(n(), -0.3, 0.3)
    ) |>
    filter(xj >= xr[1], xj <= xr[2], yj >= yr[1], yj <= yr[2])

  px <- function(v) rescale_to(v, xr, c(L, W - R))
  py <- function(v) rescale_to(v, yr, c(H - B, T))

  # points reveal left to right in 8 batches
  shown <- shown |> arrange(xj) |>
    mutate(batch = (row_number() - 1) %/% ceiling(n() / 8))

  dots <- sprintf(
    '<circle class="pt" cx="%s" cy="%s" r="3.2" style="--b:%d"/>',
    fmt(px(shown$xj)), fmt(py(shown$yj)), shown$batch
  )

  # fitted line across the plotted x range
  x0 <- xr[1] + 2; x1 <- xr[2]
  y0 <- coef(fit)[1] + coef(fit)[2] * x0
  y1 <- coef(fit)[1] + coef(fit)[2] * x1

  # outlier: starts on the fitted line, drops to its real value
  ox <- px(outlier$x); oy <- py(outlier$y); ofy <- py(outlier$fitted)
  drop_px <- ofy - oy

  xticks <- seq(60, 100, 10); yticks <- seq(65, 95, 10)
  grid <- c(
    sprintf('<line class="grid" x1="%s" x2="%s" y1="%s" y2="%s"/>',
            fmt(px(xticks)), fmt(px(xticks)), T, H - B),
    sprintf('<line class="grid" x1="%s" x2="%s" y1="%s" y2="%s"/>',
            L, W - R, fmt(py(yticks)), fmt(py(yticks))),
    sprintf('<text class="tick" x="%s" y="%s" text-anchor="middle">%d</text>',
            fmt(px(xticks)), H - B + 18, xticks),
    sprintf('<text class="tick" x="%s" y="%s" text-anchor="end" dominant-baseline="middle">%d</text>',
            L - 8, fmt(py(yticks)), yticks)
  )

  label_side <- if (ox > W / 2) "end" else "start"
  label_dx   <- if (ox > W / 2) -10 else 10

  paste0(
    '<figure class="hero-fig" data-motion="scatter">',
    '<div class="fig-head"><span class="fig-num">Fig. 01</span>',
    '<span class="fig-n">n = ', fmt_n(n_hosp), ' hospitals</span></div>',
    sprintf('<svg viewBox="0 0 %d %d" role="img" tabindex="0" aria-labelledby="hero-fig-title hero-fig-desc">', W, H),
    '<title id="hero-fig-title">Staff responsiveness vs overall hospital rating</title>',
    sprintf('<desc id="hero-fig-desc">Scatterplot of %d of %s US hospitals. A linear fit gives R squared = %.2f. The hospital furthest below its predicted rating is highlighted.</desc>',
            nrow(shown) + 1, fmt_n(n_hosp), r2),
    paste(grid, collapse = ""),
    sprintf('<text class="axis-label" x="%s" y="%d" text-anchor="middle">Staff responsiveness score</text>', fmt((L + W - R) / 2), H - 6),
    sprintf('<text class="axis-label" transform="translate(13 %s) rotate(-90)" text-anchor="middle">Overall rating score</text>', fmt((T + H - B) / 2)),
    '<g class="pts">', paste(dots, collapse = ""), '</g>',
    sprintf('<line class="fit" pathLength="1" x1="%s" y1="%s" x2="%s" y2="%s"/>',
            fmt(px(x0)), fmt(py(y0)), fmt(px(x1)), fmt(py(y1))),
    sprintf('<line class="resid-seg" x1="%s" x2="%s" y1="%s" y2="%s"/>',
            fmt(ox), fmt(ox), fmt(ofy), fmt(oy)),
    sprintf('<g class="outlier" style="--drop:%spx"><circle cx="%s" cy="%s" r="5.5"/></g>',
            fmt(drop_px), fmt(ox), fmt(oy)),
    sprintf('<text class="note outlier-note" x="%s" y="%s" text-anchor="%s">%d pts below fit</text>',
            fmt(ox + label_dx), fmt(oy + 4), label_side, round(-outlier$resid)),
    sprintf('<text class="note r2" x="%d" y="%d" text-anchor="end">linear fit · R² = %.2f</text>',
            W - R - 6, H - B - 14, r2),
    '</svg>',
    '<figcaption>Staff responsiveness vs overall rating. Each dot is a hospital ',
    '(', nrow(shown) + 1, ' shown, jittered to reveal overlap); the line is a linear fit on all ',
    fmt_n(n_hosp), '. Data: CMS HCAHPS survey. Hover or focus the chart to redraw the fit.</figcaption>',
    '</figure>'
  )
}

# ---------- homepage band: every dot a hospital, colored by residual --------

band_figure <- function(n_show = 700) {
  W <- 1600; H <- 320
  xr <- c(56, 101); yr <- c(min(staff$y) - 1.5, 99.5)
  px <- function(v) rescale_to(v, xr, c(0, W))
  py <- function(v) rescale_to(v, yr, c(H, 0))

  outlier <- staff |> slice_min(resid, n = 1, with_ties = FALSE)

  set.seed(2026)
  d <- staff |>
    slice_sample(n = n_show) |>
    mutate(xj = x + runif(n(), -0.45, 0.45), yj = y + runif(n(), -0.45, 0.45)) |>
    arrange(xj) |>
    mutate(
      batch = (row_number() - 1) %/% ceiling(n() / 10),
      cls = cut(resid, c(-Inf, -4, -1.5, 1.5, 4, Inf),
                labels = c("r-lo2", "r-lo1", "r-mid", "r-hi1", "r-hi2"))
    )

  dots <- sprintf('<circle class="pt %s" cx="%s" cy="%s" r="7" style="--b:%d"/>',
                  d$cls, fmt(px(d$xj)), fmt(py(d$yj)), d$batch)

  x0 <- xr[1]; x1 <- xr[2]
  y0 <- coef(fit)[1] + coef(fit)[2] * x0
  y1 <- coef(fit)[1] + coef(fit)[2] * x1

  # the hospital furthest below its predicted rating
  ox <- px(outlier$x); oy <- py(outlier$y); ofy <- py(outlier$fitted)

  paste0(
    '<figure class="band" data-motion="scatter">',
    sprintf('<svg viewBox="0 0 %d %d" preserveAspectRatio="xMidYMid slice" role="img" tabindex="0" aria-labelledby="band-title">', W, H),
    sprintf('<title id="band-title">%d US hospitals: staff responsiveness vs overall rating, colored by distance from a linear fit (R squared %.2f)</title>', n_show, r2),
    '<g class="pts">', paste(dots, collapse = ""), '</g>',
    sprintf('<line class="fit" pathLength="1" x1="%s" y1="%s" x2="%s" y2="%s"/>',
            fmt(px(x0)), fmt(py(y0)), fmt(px(x1)), fmt(py(y1))),
    sprintf('<line class="resid-seg" x1="%s" x2="%s" y1="%s" y2="%s"/>', fmt(ox), fmt(ox), fmt(ofy), fmt(oy)),
    sprintf('<g class="outlier" style="--drop:%spx"><circle cx="%s" cy="%s" r="9"/></g>',
            fmt(ofy - oy), fmt(ox), fmt(oy)),
    '</svg>',
    sprintf('<figcaption>Each dot is a U.S. hospital (%d of %s): staff responsiveness vs overall rating, colored by distance from a linear fit, R² = %.2f. Data: CMS HCAHPS.</figcaption>',
            n_show, fmt_n(n_hosp), r2),
    '</figure>'
  )
}

# ---------- project previews -------------------------------------------------

# Project 1: distribution of overall rating across hospitals
preview_hospital <- function() {
  W <- 240; H <- 150; pad <- 10
  counts <- staff |> count(y) |> filter(y >= 70)
  bw <- (W - 2 * pad) / nrow(counts)
  hmax <- max(counts$n)
  bars <- sprintf(
    '<rect class="bar" x="%s" y="%s" width="%s" height="%s"/>',
    fmt(pad + (seq_len(nrow(counts)) - 1) * bw + 1),
    fmt(H - pad - counts$n / hmax * (H - 2 * pad)),
    fmt(bw - 2),
    fmt(counts$n / hmax * (H - 2 * pad))
  )
  paste0(
    sprintf('<svg class="preview" viewBox="0 0 %d %d" aria-hidden="true">', W, H),
    paste(bars, collapse = ""),
    sprintf('<line class="base" x1="%d" x2="%d" y1="%d" y2="%d"/>', pad, W - pad, H - pad, H - pad),
    '</svg>'
  )
}

# Project 2: Encelia rays vs stem length, by species
preview_encelia <- function() {
  W <- 240; H <- 150; pad <- 12
  xr <- range(flower$number_rays) + c(-1, 1)
  yr <- c(0, max(flower$stem_length) + 2)
  set.seed(14)
  d <- flower |> mutate(
    cx = rescale_to(number_rays + runif(n(), -0.25, 0.25), xr, c(pad, W - pad)),
    cy = rescale_to(stem_length, yr, c(H - pad, pad))
  )
  dots <- sprintf('<circle class="sp sp-%s" cx="%s" cy="%s" r="3"/>',
                  tolower(d$Species), fmt(d$cx), fmt(d$cy))
  paste0(
    sprintf('<svg class="preview" viewBox="0 0 %d %d" aria-hidden="true">', W, H),
    sprintf('<line class="base" x1="%d" x2="%d" y1="%d" y2="%d"/>', pad, W - pad, H - pad, H - pad),
    paste(dots, collapse = ""),
    '</svg>'
  )
}
