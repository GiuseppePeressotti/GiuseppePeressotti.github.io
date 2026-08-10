# ---- ROAD dataset figures ----
# Source: Elmore, Medhi, Peressotti & Pinckney (2026), Journal of Peace Research.
# Data:   who-overturns/docs/JPR-paper/replication-package/ (published package)
#
# Chart forms are deliberately not shared with the other datasets on the page:
# ROAD gets a transition timeline, because every observation here has a start
# and an end, which no other dataset on the page does.

THIS_FILE <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
source(file.path(dirname(THIS_FILE), "theme_site.R"))
init_fonts()

REP <- file.path("C:/Users/Giuseppe/OneDrive - The University of Texas at Dallas",
                 "UTD/who-overturns/docs/JPR-paper/replication-package")
trans <- readRDS(file.path(REP, "cleaned_unique_dataset_transitions.rds"))
att   <- readRDS(file.path(REP, "cleaned_dataset_attempts.rds"))

cat("\n-- ROAD verified counts --\n")
cat("transitions:", nrow(trans), " countries:", length(unique(trans$country)),
    " attempts:", nrow(att), "\n")
cat("span:", min(trans$start.year), "-", max(trans$end.year),
    " overturned:", sum(trans$overturned == 1), "\n")

OUT <- c("No overturning attempt", "Attempted, transition held", "Transition overturned")
tl <- trans %>%
  mutate(outcome = factor(case_when(
           overturned == 1 ~ OUT[3],
           attempts   >  0 ~ OUT[2],
           TRUE            ~ OUT[1]), levels = OUT),
         end.plot = pmax(end.year, start.year + 0.4)) %>%
  arrange(start.year, country) %>%
  mutate(row = row_number())
stopifnot(nrow(tl) == nrow(trans))
print(table(tl$outcome))

# ---- Figure 1: every transition on one timeline ----
# One row per transition, drawn from onset to conclusion, ordered by onset so
# the 1990s cluster shows itself instead of being asserted in a caption.
fig1 <- function(mode) {
  p <- PAL[[mode]]
  ggplot(tl) +
    geom_segment(aes(x = start.year, xend = end.plot, y = row, yend = row,
                     colour = outcome), linewidth = 1.5, lineend = "round") +
    scale_colour_manual(values = setNames(sev3(mode), OUT)) +
    scale_x_continuous(breaks = seq(1975, 2015, by = 10)) +
    scale_y_reverse(expand = expansion(mult = 0.02)) +
    guides(colour = guide_legend(nrow = 1, override.aes = list(linewidth = 2.4))) +
    labs(title = sprintf("%d civil resistance transitions, beginning to conclusion",
                         nrow(tl)),
         x = NULL, y = NULL) +
    theme_site(mode, grid = "x") +
    theme(axis.text.y = element_blank(), legend.position = "bottom",
          legend.justification = "left")
}

# ---- Figure 2: mechanism, as a dot plot ----
mech <- att %>%
  filter(!is.na(attempt.cat), attempt.cat != "") %>%
  mutate(attempt.cat = recode(attempt.cat,
    "Legal changes creating a non-democratic regime" = "Legal changes toward autocracy",
    "Gradual seizure of power by new elites"         = "Gradual seizure by new elites",
    "An external violent overthrow"                  = "External violent overthrow",
    "Other, described in notes"                      = "Other",
    "A military coup"                                = "Military coup")) %>%
  count(attempt.cat) %>% arrange(n)
print(mech)

fig2 <- function(mode) {
  p <- PAL[[mode]]
  lv <- mech$attempt.cat
  ggplot(mech, aes(x = n, y = factor(attempt.cat, levels = lv))) +
    geom_segment(aes(x = 0, xend = n, yend = factor(attempt.cat, levels = lv)),
                 colour = p$grid, linewidth = 0.9) +
    geom_point(colour = p$accent, size = 3.1) +
    geom_text(aes(label = n), hjust = 0, nudge_x = max(mech$n) * 0.035,
              colour = p$secondary, size = 2.9, family = SITE_FONT) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.12))) +
    labs(title = sprintf("How the %d recorded overturning attempts were carried out",
                         sum(mech$n)),
         x = NULL, y = NULL) +
    theme_site(mode, grid = "none") +
    theme(axis.text.x = element_blank())
}

save_fig(fig1, "road-timeline", h = 4.2)
save_fig(fig2, "road-mechanisms", h = 3.0)
cat("\nROAD figures done.\n")
