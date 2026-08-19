# ---- BRICS accession interest figures ----
# Source: Peressotti, Chan, Dao, Shafiq, Weimer & Cecconato (under review).
# Data:   BRICS project/data/analysis/BRICS_interest_final_v3.csv (195 states)
#         BRICS project/data/derived/panel_analytical_v6.csv (2024 cross-section)
# Status tiers are ordinal, so they take tier5(): a neutral for states carrying
# no documented interest, then a multi-hue warm ramp. Single-hue was tried first
# and failed on small polygons, where four terracotta steps are not separable.
# The second figure is the positioning space: V-Dem liberal democracy against
# UNGA distance from the founders, with the median of each group marked.

THIS_FILE <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
source(file.path(dirname(THIS_FILE), "theme_site.R"))
init_fonts()
suppressPackageStartupMessages({ library(sf); library(rnaturalearth) })

# The project lives under OneDrive on both machines, at different mount points.
B_WIN <- "C:/Users/Giuseppe/OneDrive - The University of Texas at Dallas/UTD/BRICS project"
B_MAC <- file.path(path.expand("~"), "Library/CloudStorage",
                   "OneDrive-TheUniversityofTexasatDallas/UTD/BRICS project")
B <- if (dir.exists(B_WIN)) B_WIN else B_MAC
stopifnot(dir.exists(B))
d  <- read.csv(file.path(B, "data/analysis/BRICS_interest_final_v3.csv"))
pan <- read.csv(file.path(B, "data/derived/panel_analytical_v6.csv"))

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

# ---- Figure 2: the positioning space ----
# Two axes, one cross-section. x is V-Dem liberal democracy, y is UNGA voting
# distance from the five founders, both for 2024. The claim the figure carries
# is a joint one, so a single median line on either axis would understate it:
# each group gets its own bivariate median, plotted as a cross.
# The plotted set is every 2024 panel row with both measures present, which is
# neither the frozen 195-state coding nor the estimation sample, so no count
# from this figure is printed as a dataset total.
GRPS <- c("Founding member", "Expressed interest", "No documented interest")
pos <- pan %>%
  filter(year == 2024, !is.na(libdem), !is.na(unga_dist_brics)) %>%
  mutate(grp = factor(case_when(
    founding_member == 1 ~ "Founding member",
    interest_2024 == 1 | member_2024 == 1 | partner_2024 == 1 ~ "Expressed interest",
    TRUE ~ "No documented interest"), levels = GRPS),
    # Founders are the core of the interested side, not a third pole: the medians
    # split interested-or-in from everyone else.
    side = ifelse(grp == "No documented interest", "No documented interest",
                  "Expressed interest"))
stopifnot(sum(pos$grp == "Founding member") == 5)
cat("\n-- positioning figure --\n"); print(table(pos$grp))

med <- pos %>%
  group_by(side) %>%
  summarise(libdem = median(libdem), unga_dist_brics = median(unga_dist_brics),
            .groups = "drop") %>%
  mutate(side = factor(side, levels = c("Expressed interest", "No documented interest")))
print(as.data.frame(med))

# Nine labels, capped there on purpose: the five founders, the two large states
# that joined in the 2024 wave, and the two interested states that sit furthest
# from the pattern (Turkiye votes far from the founders, Uruguay is the most
# democratic state on the interested side). Names are spelled out here rather
# than pulled through countrycode, which is not a dependency of the other site
# figures. Offsets are hand-set against the render: ggrepel is not installed on
# the machine that builds these.
LAB <- data.frame(
  iso3c = c("CHN",  "RUS",   "IRN",  "TUR",     "IND",  "IDN",       "ZAF",
            "BRA",  "URY"),
  name  = c("China","Russia","Iran", "T\u00fcrkiye","India","Indonesia","South Africa",
            "Brazil","Uruguay"),
  nx    = c(0.016, -0.014,   0.016,  0.016,     0.016,  0.016,       0.016,
            0.016,  0.016),
  ny    = c(0.075, -0.020,   0.070,  0.070,     0.075,  0.105,       0.060,
            0.075,  0.070),
  # Russia is the one label that has to sit on the left: everything to its right
  # is the dense interested cluster, and the label ran through four points.
  hj    = c(0,      1,       0,      0,         0,      0,           0,
            0,      0))
lab <- pos %>%
  inner_join(LAB, by = "iso3c") %>%
  transmute(iso3c, name, nx, ny, hj, x = libdem, y = unga_dist_brics,
            grp, side)
stopifnot(nrow(lab) == nrow(LAB))

fig2 <- function(mode) {
  p <- PAL[[mode]]
  cols <- setNames(c(p$second, p$accent, p$muted), GRPS)
  ggplot(pos, aes(libdem, unga_dist_brics)) +
    # Uninterested states are the field, so they take the hollow mark and are
    # drawn first; the two interested tiers sit on top of them.
    geom_point(data = filter(pos, grp == "No documented interest"),
               aes(colour = grp), shape = 21, fill = NA, stroke = 0.5, size = 1.5) +
    geom_point(data = filter(pos, grp == "Expressed interest"),
               aes(colour = grp), shape = 16, size = 1.8) +
    geom_point(data = filter(pos, grp == "Founding member"),
               aes(colour = grp), shape = 18, size = 3.4) +
    # The median cross is drawn twice: a surface-coloured mark underneath opens a
    # gap around it, so it stays readable inside the dense interested cluster.
    geom_point(data = med, aes(shape = "Group median"), colour = p$surface,
               size = 6.2, stroke = 2.4) +
    # Not the group colour for the uninterested median: muted is a field colour,
    # and at cross size it read as a smudge. The neutral grey carries the mark.
    geom_point(data = med, aes(shape = "Group median"), size = 5.4, stroke = 1.1,
               colour = ifelse(med$side == "Expressed interest", p$accent, p$secondary)) +
    geom_text(data = lab, aes(x + nx, y + ny, label = name, hjust = hj), size = 2.5,
              family = SITE_FONT,
              colour = ifelse(lab$grp == "Founding member", p$second, p$accent)) +
    scale_colour_manual(values = cols, breaks = GRPS, drop = FALSE,
                        guide = guide_legend(order = 1, override.aes = list(
                          shape = c(18, 16, 21), size = c(3.2, 2.2, 2.2),
                          linetype = 0))) +
    scale_shape_manual(values = c("Group median" = 3),
                       guide = guide_legend(order = 2, override.aes = list(
                         colour = p$secondary, size = 3.2, stroke = 1.1))) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                       expand = expansion(mult = 0.02)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +
    labs(title = paste("States that asked to join the BRICS vote with the founders",
                       "and are less democratic:\nthe two groups' medians sit far apart on both axes"),
         x = "V-Dem liberal democracy index, 2024",
         y = "UNGA voting distance from the founders, 2024\n(lower = votes closer to the BRICS)") +
    theme_site(mode, grid = "y") +
    theme(legend.box = "horizontal", legend.spacing.x = unit(14, "pt"))
}

save_fig(fig1, "brics-map", h = 4.0)
save_fig(fig2, "brics-positioning", h = 4.2)
cat("\nBRICS figures done.\n")
