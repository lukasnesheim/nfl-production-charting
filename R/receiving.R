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

# load base nflverse stats
base_stats <- nflfastR::calculate_stats(summary_level = "week", stat_type = "player") %>%
  filter(position %in% c("WR", "TE") & week == current_week) %>%
  transmute(
    id = player_id,
    name = player_display_name,
    short = player_name,
    position,
    # team = recent_team, # season-level only
    team, # week-level only
    # gp = games, # season-level only
    receiving_fumbles,
    receiving_first_downs,
    racr,
    target_share,
    air_yards_share,
    wopr,
    fpts = fantasy_points_ppr + 0.5 * receiving_first_downs,
    # fppg = fpts / gp, # season-level only
  )

# load nfl pro stats
nfl_stats <- read_csv(here("data", "nfl", "receiving", paste0("2025_0", current_week, ".csv"))) %>%
  mutate(name = player) %>%
  select(name, everything(), -player, -position)

# merge stats and team info
stats <- base_stats %>%
  inner_join(nfl_stats, by = "name") %>%
  inner_join(teams, by = "team") %>%
  mutate(
    targets_rr = replace_na(as.numeric(targets / routes), 0),
    yards_rr = replace_na(as.numeric(receiving_yards / routes), 0),
    points_rr = replace_na(as.numeric(fpts / routes), 0),
    firsts_rr = replace_na(as.numeric(receiving_first_downs / routes), 0),
    points_tgt = replace_na(as.numeric(fpts / targets), 0)
  ) %>%
  group_by(position) %>%
  group_modify(function(.x, .y) {
    n <- if (.y$position == "WR") 40 else 10
    slice_max(.x, order_by = fpts, n = n)
  }) %>%
  ungroup() %>%
  arrange(desc(fpts))

# manual modification
stats <- stats %>%
  filter(targets > 3)

# set dynamic axis limits
lims <- list(
  xlim = c(min(stats$targets_rr) - 0.01, max(stats$targets_rr) + 0.01),
  ylim = c(min(stats$points_rr) - 0.01, max(stats$points_rr) + 0.01)
)

# plot points per route run (y-axis) by targets per route run (x-axis)
plot <- ggplot(stats, aes(x = targets_rr, y = points_rr)) +
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
    limits = lims$xlim
  ) +
  scale_y_continuous(
    limits = lims$ylim
  ) +
  annotate(
    "segment",
    x = lims$xlim[1] + 0.01,
    xend = lims$xlim[2] - 0.01,
    y = mean(stats$points_rr, na.rm = TRUE),
    yend = mean(stats$points_rr, na.rm = TRUE),
    color = color$london[[3]],
    linewidth = 0.5,
    linetype = "dashed",
    alpha = 0.6
  ) +
  annotate(
    "segment",
    x = mean(stats$targets_rr, na.rm = TRUE),
    xend = mean(stats$targets_rr, na.rm = TRUE),
    y = lims$ylim[1] + 0.01,
    yend = lims$ylim[2] - 0.01,
    color = color$london[[3]],
    linewidth = 0.5,
    linetype = "dashed",
    alpha = 0.6
  ) +
  labs(
    title = "Throw Me the Damn Ball",
    subtitle = paste0("Receiver Production in Week ", current_week),
    caption = paste0("Charting: Lukas Nesheim (data via nflverse, NFL+)"),
    x = "Targets per Route Run",
    y = "Fantasy Points per Route Run"
  ) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5, color = "#333333") +
  theme_baseline_gg() +
  theme(plot.subtitle = element_text(family = "Montserrat-SemiBold", color = color$london[[5]]))

ggsave("receiving.png", plot, height = 6, width = 6, units = "in", dpi = 600)