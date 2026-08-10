# ---- Regime support group defections ----
# Source: Elmore, Peressotti & Pinckney (work in progress).
# Data:   defections/data/rsg-country-year-panel.csv
# Repo is collaborator-owned (JCPinckney/defections); read only, never written.
#
# View 1: the fourteen support groups (dot plot of coded observations).
# View 2: which groups slip, which hold (diverging bars of move direction by
#         group type, regimes never named).
#
# Source verification is still running, so neither view reports a result.

THIS_FILE <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
source(file.path(dirname(THIS_FILE), "theme_site.R"))
init_fonts()

panel <- read.csv(file.path("C:/Users/Giuseppe/OneDrive - The University of Texas at Dallas",
                            "UTD/defections/data/rsg-country-year-panel.csv"),
                  stringsAsFactors = FALSE)
coded <- panel %>% filter(obs_status == "coded")

cat("\n-- verified counts --\n")
cat("panel rows:", nrow(panel), " coded:", nrow(coded), "\n")
cat("regimes:", length(unique(coded$v2regidnr)),
    " countries:", length(unique(coded$country_name)),
    " years:", min(coded$year), "-", max(coded$year), "\n")

# ---- Figure 1: the fourteen groups ----
groups <- coded %>% filter(!is.na(group), group != "") %>% count(group) %>% arrange(n)
stopifnot(nrow(groups) == 14)
cat("\n-- group totals --\n"); print(as.data.frame(groups))

fig1 <- function(mode) {
  p <- PAL[[mode]]
  lv <- groups$group
  ggplot(groups, aes(x = n, y = factor(group, levels = lv))) +
    geom_segment(aes(x = 0, xend = n, yend = factor(group, levels = lv)),
                 colour = p$grid, linewidth = 0.9) +
    geom_point(colour = p$accent, size = 2.9) +
    geom_text(aes(label = format(n, big.mark = ",")), hjust = 0,
              nudge_x = max(groups$n) * 0.035, colour = p$secondary,
              size = 2.7, family = SITE_FONT) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.14))) +
    labs(title = sprintf("Coded observations per support group, across %d regimes",
                         length(unique(coded$v2regidnr))),
         x = NULL, y = NULL) +
    theme_site(mode, grid = "none") +
    theme(axis.text.x = element_blank())
}

# ---- Figure 2: which groups slip, which hold ----
# For each of the fourteen group types, take the year-to-year moves that
# actually changed something and show how they split: toward opposition
# (erosion) or back toward the regime. Regimes are never named, so no single
# country reads as a finding.
moves <- coded %>%
  filter(!is.na(group_loyalty_intensity)) %>%
  arrange(v2regidnr, group, group_name, year) %>%
  group_by(v2regidnr, group, group_name) %>%
  mutate(prev = lag(group_loyalty_intensity)) %>%
  ungroup() %>%
  filter(!is.na(prev)) %>%
  mutate(dir = case_when(
    group_loyalty_intensity > prev ~ "erosion",
    group_loyalty_intensity < prev ~ "recovery",
    TRUE ~ "flat"))
cat("\n-- move directions (adjacent coded years, same group series) --\n")
mv <- moves %>% count(dir) %>% mutate(share = n / sum(n) * 100)
print(as.data.frame(mv))

grp <- moves %>%
  filter(dir != "flat") %>%
  count(group, dir) %>%
  tidyr::pivot_wider(names_from = dir, values_from = n, values_fill = 0) %>%
  mutate(erosion_share = erosion / (erosion + recovery) * 100) %>%
  arrange(erosion_share) %>%
  mutate(
    erosion_x  = erosion_share,
    recovery_x = -100 + erosion_share,
    total      = erosion + recovery
  )
cat("\n-- erosion share by group (moves only) --\n")
print(as.data.frame(grp), row.names = FALSE)

fig2 <- function(mode) {
  p <- PAL[[mode]]
  d <- grp %>%
    tidyr::pivot_longer(c(erosion_x, recovery_x), names_to = "dir", values_to = "x") %>%
    mutate(dir = factor(ifelse(dir == "erosion_x", "erosion", "recovery"),
                        levels = c("recovery", "erosion")))
  cols <- c(recovery = p$second, erosion = p$accent)
  ggplot(d, aes(x = x, y = reorder(group, erosion_share), fill = dir)) +
    geom_col(width = 0.66) +
    geom_text(aes(label = ifelse(dir == "erosion",
                                 sprintf("%.0f%%", abs(x)), "")),
              hjust = -0.15, size = 2.3, colour = p$secondary, family = SITE_FONT) +
    geom_vline(xintercept = 0, colour = p$grid, linewidth = 0.4) +
    scale_fill_manual(values = cols, name = NULL,
                      labels = c("Back toward the regime", "Toward opposition")) +
    scale_x_continuous(labels = function(x) paste0(abs(x), "%"),
                       expand = expansion(mult = c(0.02, 0.08))) +
    labs(title = "Share of each group's moves that went toward opposition or back toward the regime",
         x = NULL, y = NULL) +
    theme_site(mode, grid = "none") +
    theme(axis.text.x = element_blank(), legend.position = "bottom",
          legend.justification = "left")
}

save_fig(fig1, "defections-groups", h = 3.6)
save_fig(fig2, "defections-slips", h = 4.2)
cat("\nDefections figures done.\n")
