# ---- Civil resistance successor organizations figures ----
# Source: Peressotti, Medhi, Elmore & Pinckney (under review).
# Data:   who-overturns/datasets/org-data-presubmission.rds
#
# This dataset's distinguishing feature is that every organization is coded on
# several dimensions at once, so it gets the cross-tabulation form: a matrix of
# organization type against relationship to the fallen regime.

THIS_FILE <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
source(file.path(dirname(THIS_FILE), "theme_site.R"))
init_fonts()

orgs <- readRDS(file.path("C:/Users/Giuseppe/OneDrive - The University of Texas at Dallas",
                          "UTD/who-overturns/datasets/org-data-presubmission.rds"))

cat("\n-- verified counts --\n")
cat("organizations:", nrow(orgs), " transitions:", length(unique(orgs$trans.id)),
    " countries:", length(unique(orgs$country)), "\n")

TIES <- c("Central part of old regime", "Closely connected to old regime",
          "Politically neutral", "Closely connected to opposition",
          "Central part of opposition", "Did not exist before transition")
TIES_SHORT <- c("Central to\nold regime", "Close to\nold regime", "Neutral",
                "Close to\nopposition", "Central to\nopposition", "Did not yet\nexist")

grid_df <- orgs %>%
  filter(!is.na(org.type.label), org.type.label != "",
         !is.na(old.regime.label), old.regime.label != "") %>%
  count(org.type.label, old.regime.label)
cat("organizations in the cross-tab:", sum(grid_df$n), "of", nrow(orgs), "\n")
stopifnot(all(grid_df$old.regime.label %in% TIES))

type_order <- grid_df %>% group_by(org.type.label) %>%
  summarise(tot = sum(n), .groups = "drop") %>% arrange(tot) %>% pull(org.type.label)

full <- expand.grid(org.type.label = type_order, old.regime.label = TIES,
                    stringsAsFactors = FALSE) %>%
  left_join(grid_df, by = c("org.type.label", "old.regime.label")) %>%
  mutate(n = ifelse(is.na(n), 0L, n),
         org.type.label = factor(org.type.label, levels = type_order),
         tie = factor(TIES_SHORT[match(old.regime.label, TIES)], levels = TIES_SHORT))

# ---- Figure 1: the coding grid ----
# A count matrix. Empty cells are left as surface rather than filled with a
# zero-value colour, so the sparsity of the corners is visible.
fig1 <- function(mode) {
  p <- PAL[[mode]]
  r <- PAL[[mode]]$ramp4
  lab <- subset(full, n > 0)
  lab$label_col <- cell_text_col(mode, lab$n)
  ggplot(full, aes(x = tie, y = org.type.label)) +
    geom_tile(aes(fill = ifelse(n == 0, NA, n)), colour = p$surface, linewidth = 1.4) +
    geom_text(data = lab, aes(label = n, colour = label_col),
              size = 2.7, family = SITE_FONT, show.legend = FALSE) +
    scale_fill_gradientn(colours = if (mode == "light") r else rev(r),
                         na.value = p$surface, trans = "sqrt", guide = "none") +
    scale_colour_identity() +
    scale_x_discrete(position = "top") +
    labs(title = sprintf("%d organizations, by type and by closeness to the regime that fell",
                         sum(grid_df$n)),
         x = NULL, y = NULL) +
    theme_site(mode, grid = "none") +
    theme(axis.text.x.top = element_text(size = 7.2, lineheight = 0.95),
          panel.grid = element_blank())
}

save_fig(fig1, "wyb-grid", h = 4.0)

# ---- Figure 2: overturning rate by capacity score ----
# The paper's central construct. The index columns do not survive in the saved
# rds, so they are rebuilt here exactly as the manuscript does: one point for a
# "power organization" (state-linked type), one for a large membership, one for
# leading the transition government. Scores 0-3 are ordinal, so they take the
# single-hue ramp; the outcome share sits on the y axis.
d2 <- orgs %>%
  mutate(
    is_power_org = as.integer(org.type %in% c(1, 2, 3)),
    is_large     = as.integer(org.size >= 4 & org.size != 6),
    leads_govt   = as.integer(transition.gov <= 2),
    capacity     = is_power_org + is_large + leads_govt
  )
d_model <- d2 %>%
  filter(overturning.bhv != "8", org.size != 6, !is.na(is_overturner))
cap <- d_model %>%
  group_by(capacity) %>%
  summarise(n = n(), n_off = sum(is_overturner), .groups = "drop") %>%
  mutate(pct = n_off / n * 100)
cat("\n-- capacity summary (model sample n =", nrow(d_model), ") --\n")
print(as.data.frame(cap))
stopifnot(nrow(cap) == 4, all(cap$capacity == 0:3))

fig2 <- function(mode) {
  p <- PAL[[mode]]
  cols <- setNames(ord(mode, 4), as.character(0:3))
  ggplot(cap, aes(x = factor(capacity, levels = 0:3), y = pct)) +
    geom_col(aes(fill = factor(capacity, levels = 0:3)), width = 0.62) +
    geom_text(aes(label = sprintf("%.1f%%\n(n = %d)", pct, n)),
              vjust = -0.35, lineheight = 0.9, size = 2.7, family = SITE_FONT,
              colour = p$secondary) +
    scale_fill_manual(values = cols, guide = "none") +
    expand_limits(y = max(cap$pct) * 1.45) +
    labs(title = "Likelihood of overturning, by how influential the organization is",
         x = "How influential the organization is (capacity score, 0-3)",
         y = "Moved to overturn (%)") +
    theme_site(mode, grid = "y")
}

save_fig(fig2, "wyb-capacity", h = 3.4)
cat("\nSuccessor organization figures done.\n")
