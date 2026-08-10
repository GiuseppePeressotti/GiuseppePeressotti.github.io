# ---- BRICS accession interest figures ----
# Source: Peressotti, Chan, Dao, Shafiq, Weimer & Cecconato (under review).
# Data:   BRICS project/data/analysis/BRICS_interest_final_v3.csv (195 states)
#         BRICS project/data/derived/BRICS_interest_final_v3_with_years.csv
# Status tiers are ordinal, so they take tier5(): a neutral for states carrying
# no documented interest, then a multi-hue warm ramp. Single-hue was tried first
# and failed on small polygons, where four terracotta steps are not separable.

THIS_FILE <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
source(file.path(dirname(THIS_FILE), "theme_site.R"))
init_fonts()
suppressPackageStartupMessages({ library(sf); library(rnaturalearth) })

B <- "C:/Users/Giuseppe/OneDrive - The University of Texas at Dallas/UTD/BRICS project"
d  <- read.csv(file.path(B, "data/analysis/BRICS_interest_final_v3.csv"))
dy <- read.csv(file.path(B, "data/derived/BRICS_interest_final_v3_with_years.csv"))

cat("\n-- BRICS verified counts --\n")
cat("states        :", nrow(d), "\n")
cat("ever interest :", sum(d$interest == 1, na.rm = TRUE), "\n")
cat("members       :", sum(d$member  == 1, na.rm = TRUE), "\n")
cat("partners      :", sum(d$partner == 1, na.rm = TRUE), "\n")
cat("invited       :", sum(d$invited == 1, na.rm = TRUE), "\n")

# ---- Status tiers (mutually exclusive, ordered) ----
TIERS <- c("No documented interest", "Interest expressed", "Invited", "Partner", "Member")
d <- d %>% mutate(tier = factor(case_when(
  member  == 1 ~ "Member",
  partner == 1 ~ "Partner",
  invited == 1 ~ "Invited",
  interest == 1 ~ "Interest expressed",
  TRUE ~ "No documented interest"), levels = TIERS))
print(table(d$tier))

tier_cols <- function(mode) setNames(tier5(mode), TIERS)

# ---- Figure 1: world map ----
# Natural Earth codes France and Norway as iso_a3 = "-99"; iso_a3_eh carries the
# real codes, so join on that and fall back to iso_a3 only where it is missing.
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(iso3 = ifelse(is.na(iso_a3_eh) | iso_a3_eh == "-99", iso_a3, iso_a3_eh)) %>%
  filter(iso3 != "ATA")                        # drop Antarctica: no data, wastes height
wmap <- world %>% left_join(d %>% select(iso3c, tier), by = c("iso3" = "iso3c")) %>%
  mutate(tier = factor(ifelse(is.na(as.character(tier)),
                              "No documented interest", as.character(tier)), levels = TIERS))
unmatched <- setdiff(d$iso3c, world$iso3)
cat("\nunmatched iso3 codes in data:",
    if (length(unmatched)) paste(unmatched, collapse = ", ") else "none", "\n")

fig1 <- function(mode) {
  p <- PAL[[mode]]
  # In light mode the shallow tiers are pale, and a surface-coloured stroke on a
  # pale fill over a surface-coloured background erases small states outright
  # (Singapore, Bahrain). Stroke on the grid colour instead. Dark mode keeps the
  # surface stroke, which already separates cleanly against saturated fills.
  border <- if (mode == "light") p$grid else p$surface
  ggplot(wmap) +
    geom_sf(aes(fill = tier), colour = border, linewidth = 0.12) +
    scale_fill_manual(values = tier_cols(mode), drop = FALSE) +
    coord_sf(crs = "+proj=robin", ylim = c(-5.5e6, 8.6e6), expand = FALSE, datum = NA) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(title = sprintf("Where all %d states stood on BRICS accession at the end of 2024",
                         nrow(d))) +
    theme_site(mode, grid = "none") +
    theme(axis.text = element_blank(), panel.grid = element_blank(),
          legend.position = "bottom", legend.justification = "left")
}

# ---- Figure 2: cumulative onset of interest ----
# The derived with-years file is maintained past the manuscript's cross-section:
# it carries Cameroon and a January 2026 Madagascar onset that the frozen
# analysis file does not. Cut at end-2024 so the figure matches the canonical
# count of 60, and assert that it does rather than trusting the filter.
CUTOFF <- 2024
onset <- dy %>%
  filter(interest == 1, !is.na(interest_year), interest_year <= CUTOFF) %>%
  count(interest_year, name = "new") %>%
  arrange(interest_year) %>%
  mutate(cumulative = cumsum(new))
stopifnot(sum(onset$new) == sum(d$interest == 1, na.rm = TRUE))
cat("\nonsets through", CUTOFF, ":", sum(onset$new),
    "- matches canonical analysis file\n")
print(onset)

fig2 <- function(mode) {
  p <- PAL[[mode]]
  ggplot(onset, aes(x = interest_year, y = cumulative)) +
    geom_area(data = step_area(onset, "interest_year", "cumulative"),
              fill = p$accent, alpha = 0.13, position = "identity") +
    geom_step(colour = p$accent, linewidth = LINE_WIDTH, direction = "hv") +
    scale_x_continuous(breaks = scales::pretty_breaks(6)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
    labs(title = "Cumulative states expressing interest in joining",
         x = NULL, y = "States") +
    theme_site(mode, grid = "y")
}

save_fig(fig1, "brics-map", h = 4.0)
save_fig(fig2, "brics-cumulative", h = 3.0)
cat("\nBRICS figures done.\n")
