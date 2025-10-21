# tidyverse
library(tidyverse)

# nflverse
library(nflverse)

# baseline
library(baseliner)

# package
library(here)
library(jsonlite)
library(ggrepel)
library(scales)

# global variables
season <- nflreadr::most_recent_season()
current_week <- 7

# baseliner style and color
style <- read_json(system.file("style.json", package = "baseliner"))
color <- read_json(system.file("color.json", package = "baseliner"))

# flip certain team colors for readability
flip_color <- c(
  "BAL",
  "BUF",
  "CHI",
  "DAL",
  "DEN",
  "DET",
  "GB",
  "HOU",
  "IND",
  "KC",
  "LA",
  "LAC",
  "LV",
  "MIA",
  "MIN",
  "NE",
  "NYG",
  "PHI",
  "PIT",
  "SEA",
  "SF",
  "TEN",
  "WAS"
)

# load team data
teams <- nflreadr::load_teams() %>%
  select(team = team_abbr, team_pri = team_color, team_sec = team_color2)

# load nfl pro passing stats
passing_stats <- read_csv(here("data", "nfl", "passing", paste0("2025_0", current_week, ".csv"))) %>%
  select(-games_played, -games_started)

# load nfl pro rushing stats
rushing_stats <- read_csv(here("data", "nfl", "rushing", paste0("2025_0", current_week, ".csv"))) %>%
  select(-games_played, -games_started) %>%
  filter(position == "QB") %>%
  distinct(player, .keep_all = TRUE)

# load quarterbacks
qbs <- nflfastR::calculate_stats(summary_level = "week", stat_type = "player") %>%
  filter(position == "QB" & week == current_week) %>%
  transmute(id = player_id, player = player_display_name, short = player_name, position, team)

# load plays stats
plays <- nflfastR::load_pbp(seasons = season) %>%
  filter(week == current_week) %>%
  filter(play == 1) %>%
  filter(qb_dropback == 1 | play_type == "run") %>%
  mutate(id = coalesce(passer_id, rusher_id)) %>%
  inner_join(qbs, by = "id") %>%
  group_by(id, player, short, team) %>%
  summarize(plays = n())

# merge passing, rushing, and plays stats
stats <- passing_stats %>%
  left_join(rushing_stats, by = "player") %>%
  inner_join(plays, by = "player") %>%
  inner_join(teams, by = "team") %>%
  select(-id, -position) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  mutate(epa = passing_epa + rushing_epa, epa_per_play = epa / plays)

# manual modification
stats <- stats %>%
  filter(attempts >= 5)

# set dynamic axis limits
lims <- list(
  xlim = c(min(stats$completion_pct_oe) - 0.01, max(stats$completion_pct_oe) + 0.01),
  ylim = c(min(stats$epa_per_play) - 0.01, max(stats$epa_per_play) + 0.01)
)

# plot epa per play (y-axis) by cpoe (x-axis)
plot <- ggplot(
  stats,
  aes(x = completion_pct_oe, y = epa_per_play)
) +
  geom_point(
    aes(
      fill = ifelse(team %in% flip_color, team_sec, team_pri),
      color = ifelse(team %in% flip_color, team_pri, team_sec)
    ),
    shape = 21,
    size = 2,
    stroke = 1,
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  geom_text_repel(
    aes(label = short),
    family = style$chart$font$family$label,
    color = style$chart$font$color$label,
    size = 2.5,
    point.padding = 0.5,
    box.padding = 0.25,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    limits = lims$xlim,
    labels = label_percent(accuracy = 1)
  ) +
  scale_y_continuous(
    limits = lims$ylim
  ) +
  annotate(
    "segment",
    x = lims$xlim[1] + 0.01,
    xend = lims$xlim[2] - 0.01,
    y = mean(stats$epa_per_play, na.rm = TRUE),
    yend = mean(stats$epa_per_play, na.rm = TRUE),
    color = color$london[[3]],
    linewidth = 0.5,
    linetype = "dashed",
    alpha = 0.6
  ) +
  annotate(
    "segment",
    x = mean(stats$completion_pct_oe, na.rm = TRUE),
    xend = mean(stats$completion_pct_oe, na.rm = TRUE),
    y = lims$ylim[1] + 0.01,
    yend = lims$ylim[2] - 0.01,
    color = color$london[[3]],
    linewidth = 0.5,
    linetype = "dashed",
    alpha = 0.6
  ) +
  geom_vline(
    xintercept = 0,
    color = color$london[[3]]
  ) +
  labs(
    title = "Expected Points from Unexpected Throws",
    subtitle = paste0("Quarterback Production in Week ", current_week),
    caption = "Charting: Lukas Nesheim (data via nflverse, NFL+)",
    x = "Completion Percentage over Expected",
    y = "EPA per Play"
  ) +
  theme_baseline_gg() +
  theme(plot.subtitle = element_text(family = "Montserrat-SemiBold", color = color$london[[5]]))

ggsave("passing.png", plot, height = 6, width = 6, units = "in", dpi = 600)