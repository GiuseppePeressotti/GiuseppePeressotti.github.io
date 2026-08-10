# ---- Site figure house style ----
# Shared theme for every figure on the Data & Tools page.
# Each figure is rendered twice, once per site colour mode, and swapped in CSS
# via [data-theme="dark"]. Palette validated with the dataviz six-checks
# validator: categorical, ordinal-ramp and contrast checks all pass in both modes.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(scales)
})

# Each figure script sets THIS_FILE to its own path before sourcing this file,
# so output paths resolve without depending on the working directory.
if (!exists("THIS_FILE")) THIS_FILE <- file.path(getwd(), "theme_site.R")

# ---- Palette ----
# Warm family built on the site accent (terracotta), in three registers:
#   ord()   large-field ordinal fills   - single hue, monotone in lightness
#   sev3()  3 ordered outcomes          - monotone lightness PLUS a hue journey
#   tier5() 5 ordered tiers             - neutral + multi-hue warm sequential ramp
# sev3() and tier5() break the single-hue rule on purpose. A lightness-only ramp
# collapses toward "three greys" on small marks, because thin strokes and small
# polygons compress apparent chroma. Adding hue puts the separation on a second
# channel while lightness keeps carrying the order. Minimum CIEDE2000 over all
# pairs, checked under normal vision and deutan/protan/tritan:
#   sev3  light 19.7  dark 15.6   (bar 15, small-field marks)
#   tier5 light 15.0  dark 11.7   (bar 10, filled polygons)
# Every colour also clears its contrast bar against its own surface: 3:1 for the
# ROAD line marks, no bar for map polygons, which are judged on step-to-step dE.
PAL <- list(
  light = list(
    surface   = "#FAFAF7",
    ink       = "#1A1918",
    secondary = "#6E6E6A",
    tertiary  = "#9C9C98",
    grid      = "#E2E0DC",
    accent    = "#C24D2C",
    second    = "#008572",
    ramp4     = c("#D89A7E", "#C2694A", "#A8431F", "#7A2C0F"),
    cat3      = c("#C24D2C", "#008572", "#3D5FB8"),
    sev3      = c("#93908A", "#00655A", "#7E2A10"),
    tier5     = c("#DCDAD5", "#F0C79B", "#D8813F", "#AB4526", "#68203A"),
    muted     = "#C9C6C0",
    none      = "#DCDAD5"
  ),
  dark = list(
    surface   = "#101010",
    ink       = "#E4E2DD",
    secondary = "#8C8C88",
    tertiary  = "#5C5C58",
    grid      = "#2A2A28",
    accent    = "#D4684A",
    second    = "#2FA894",
    ramp4     = c("#8A452A", "#B15C39", "#D4784F", "#EFA98D"),
    cat3      = c("#D4684A", "#2FA894", "#6B8AD8"),
    sev3      = c("#64605A", "#35AFA6", "#F9C3AA"),
    tier5     = c("#2E2E2C", "#9C4256", "#C4602F", "#E09A63", "#F4D3A8"),
    muted     = "#4A4A46",
    none      = "#2E2E2C"
  )
)

# Colour has to encode something. Five uses are allowed on this page:
#   ord(mode, n)  ordered categories       - one hue, light to dark
#   cat3(mode)    up to three entities     - validated all-pairs for CVD
#   emph(...)     one focal category       - accent against muted grey
#   sev3(mode)    3 ordered outcomes on thin marks (neutral / teal / burnt)
#   tier5(mode)   5 ordered tiers on a choropleth (neutral + warm ramp)
# ord(), sev3() and tier5() all reverse direction in dark mode so that "more" is
# always the more saturated end against its own surface.
ord <- function(mode, n) {
  r <- PAL[[mode]]$ramp4
  if (n <= 4) return(if (mode == "light") tail(r, n) else head(r, n))
  c(if (mode == "light") r else rev(r), PAL[[mode]]$none)[seq_len(n)]
}
cat3  <- function(mode) PAL[[mode]]$cat3
sev3  <- function(mode) PAL[[mode]]$sev3
tier5 <- function(mode) PAL[[mode]]$tier5
emph <- function(mode, is_focal) {
  ifelse(is_focal, PAL[[mode]]$accent, PAL[[mode]]$muted)
}

# Count-grid value labels: replicate the sqrt-scaled gradientn fill for each
# count and pick the ink/surface pole with the higher contrast. In dark mode,
# prefer the dark text (surface) whenever it clears 3:1, so only the darkest
# cells flip to light text; in light mode use the geometric-mean luminance
# threshold. Both modes stay legible without touching the ramp, which remains
# monotone and single-hue.
cell_text_col <- function(mode, n, maxn = max(n)) {
  p <- PAL[[mode]]
  ramp <- if (mode == "light") p$ramp4 else rev(p$ramp4)
  minn <- min(n)
  t  <- (sqrt(n) - sqrt(minn)) / (sqrt(maxn) - sqrt(minn))
  i  <- pmin(floor(t * 3) + 1, 3)
  tt <- (t - (i - 1) / 3) * 3
  # Interpolate per count. col2rgb() returns a 3 x N matrix filled column-wise,
  # so sweep() aligns each count's t-value with its own RGB column.
  v  <- sweep(col2rgb(ramp[i]), 2, 1 - tt, "*") + sweep(col2rgb(ramp[i + 1]), 2, tt, "*")
  lin <- function(x) ifelse(x <= 0.03928, x / 12.92, ((x + 0.055) / 1.055)^2.4)
  w  <- c(0.2126, 0.7152, 0.0722)
  Lf <- colSums(lin(v / 255) * w)
  Li <- sum(lin(col2rgb(p$ink) / 255) * w)
  Ls <- sum(lin(col2rgb(p$surface) / 255) * w)
  if (mode == "light") {
    thr <- sqrt((max(Li, Ls) + 0.05) * (min(Li, Ls) + 0.05)) - 0.05
    dark  <- if (Li > Ls) p$surface else p$ink
    light <- if (Li > Ls) p$ink else p$surface
    ifelse(Lf >= thr, dark, light)
  } else {
    # dark mode: surface is near-black text, ink is light text. Use dark text
    # wherever it clears 3:1 against the fill; only the darkest cells flip to light.
    cr_dark <- (Lf + 0.05) / (Ls + 0.05)
    ifelse(cr_dark >= 3, p$surface, p$ink)
  }
}

# ---- Typography ----
# The site loads Inter from Google Fonts. Mirror it in the figures when the
# download succeeds; fall back to the default sans rather than failing a render.
SITE_FONT <- "sans"
init_fonts <- function() {
  ok <- try({
    sysfonts::font_add_google("Inter", "Inter")
    showtext::showtext_auto()
    showtext::showtext_opts(dpi = 200)
    TRUE
  }, silent = TRUE)
  if (!inherits(ok, "try-error")) SITE_FONT <<- "Inter" else
    message("NOTE: Inter unavailable, falling back to system sans")
  invisible(SITE_FONT)
}

# ---- Theme ----
# Recessive axes and grid: one axis of gridlines only, no panel border, no
# background fill beyond the site surface.
theme_site <- function(mode = "light", grid = c("y", "x", "none")) {
  grid <- match.arg(grid)
  p <- PAL[[mode]]
  gl <- element_line(colour = p$grid, linewidth = 0.3)
  th <- theme_minimal(base_family = SITE_FONT, base_size = 9) +
    theme(
      plot.background   = element_rect(fill = p$surface, colour = NA),
      panel.background  = element_rect(fill = p$surface, colour = NA),
      panel.border      = element_blank(),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_blank(),
      axis.title        = element_text(colour = p$secondary, size = 8.5),
      axis.title.x      = element_text(margin = margin(t = 8)),
      axis.title.y      = element_text(margin = margin(r = 8)),
      axis.text         = element_text(colour = p$secondary, size = 8),
      axis.ticks        = element_blank(),
      # No figure on the page carries a subtitle any more: the title has to tell
      # the whole story on its own, so it takes the full gap the subtitle used to
      # occupy and wraps rather than running off the panel.
      plot.title         = element_text(colour = p$ink, size = 11.5, face = "plain",
                                        margin = margin(b = 12), hjust = 0,
                                        lineheight = 1.15),
      plot.subtitle      = element_text(colour = p$tertiary, size = 8.5,
                                        margin = margin(b = 12), hjust = 0),
      plot.caption       = element_text(colour = p$tertiary, size = 7.5,
                                        hjust = 0, margin = margin(t = 12)),
      plot.caption.position = "plot",
      plot.title.position   = "plot",
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = element_blank(),
      # The key sat almost against its own label, and the next entry sat almost
      # against the previous label, so a row of keys read as one run-on string.
      # legend.text's left margin opens the key-to-label gap; key.spacing.x opens
      # the entry-to-entry gap. Both are needed: neither does the other's job.
      legend.text        = element_text(colour = p$secondary, size = 8,
                                        margin = margin(l = 4, r = 2)),
      legend.key.size    = unit(9, "pt"),
      legend.key.spacing.x = unit(14, "pt"),
      legend.margin      = margin(b = 2),
      strip.text         = element_text(colour = p$ink, size = 8.5, hjust = 0,
                                        margin = margin(b = 4)),
      plot.margin        = margin(14, 16, 12, 14)
    )
  if (grid == "y") th <- th + theme(panel.grid.major.y = gl)
  if (grid == "x") th <- th + theme(panel.grid.major.x = gl)
  th
}

# ---- Marks ----
# Bars carry a rounded data-end; ggplot has no per-corner radius, so the width
# and spacing do the work instead. Keep bars thin with a visible surface gap.
BAR_WIDTH  <- 0.5
LINE_WIDTH <- 0.7    # ~2px at 200dpi
POINT_SIZE <- 1.6    # ~8px at 200dpi

# Value labels sit a constant distance past the data end, so a bar of 1 and a
# bar of 36 get the same gap. hjust would scale the gap with bar length.
bar_label <- function(mode, vals, size = 2.9) {
  geom_text(aes(label = vals), hjust = 0, nudge_x = max(vals, na.rm = TRUE) * 0.018,
            colour = PAL[[mode]]$secondary, size = size, family = SITE_FONT)
}

# geom_area interpolates linearly between points, so pairing it with geom_step
# leaves the fill floating away from the line. This expands a cumulative series
# into explicit step corners so the fill traces the same path as the line.
step_area <- function(df, x, y) {
  xs <- df[[x]]; ys <- df[[y]]
  prev <- c(0, head(ys, -1))
  out <- data.frame(x = rep(xs, each = 2), y = as.vector(rbind(prev, ys)))
  setNames(out, c(x, y))
}

# ---- Output location ----
# Site root is the parent of _figures/; figures land in figures/data/.
SITE_ROOT <- normalizePath(file.path(dirname(THIS_FILE), ".."), mustWork = TRUE)
FIG_DIR   <- file.path(SITE_ROOT, "figures", "data")

# ---- Render ----
# 7in at 200dpi = 1400px, i.e. 2x the ~700px display width.
save_fig <- function(plot_fn, name, w = 7, h = 4.2, outdir = FIG_DIR) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  for (mode in c("light", "dark")) {
    f <- file.path(outdir, sprintf("%s-%s.png", name, mode))
    ragg::agg_png(f, width = w, height = h, units = "in", res = 200,
                  background = PAL[[mode]]$surface)
    print(plot_fn(mode))
    invisible(dev.off())
    cat("  wrote", basename(f), "\n")
  }
}
