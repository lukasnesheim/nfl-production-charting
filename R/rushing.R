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
library(janitor)
library(fuzzyjoin)

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
  filter(position == "RB" & week == current_week) %>%
  transmute(
    id = player_id,
    name = clean_player_names(player_display_name),
    short = player_name,
    position,
    # team = recent_team, # season-level only
    team,
    # gp = games, # season-level only
    fd = rushing_first_downs + receiving_first_downs,
    fpts = fantasy_points_ppr + 0.5 * fd,
    # fppg = fpts / gp, # season-level only
  )

# load nfl pro rushing stats
nfl_stats <- read_csv(here("data", "nfl", "rushing", paste0("2025_0", current_week, ".csv"))) %>%
  mutate(name = player) %>%
  select(name, everything(), -player, -games_played, -games_started) %>%
  filter(position == "RB")

# load fantasy points data weighted opportunity stats
wo_stats <- read_csv(here("data", "fpts", "rushing", paste0("2025_0", current_week, ".csv"))) %>%
  clean_names() %>%
  transmute(name = clean_player_names(name), wo, wopg = wo_g)

test <- base_stats %>%
  stringdist_inner_join(wo_stats, by = "name", max_dist = 1)

# merge base stats and weighted opportunity stats with team info
stats <- base_stats %>%
  stringdist_inner_join(wo_stats, by = "name", max_dist = 1) %>%
  select(id, name = name.x, everything(), -name.y) %>%
  inner_join(nfl_stats, by = "name") %>%
  inner_join(teams, by = "team") %>%
  mutate(points_wo = fpts / wo) %>%
  arrange(desc(fpts))

# set dynamic axis limits
lims <- list(
  xlim = c(min(stats$wo) - 0.01, max(stats$wo) + 0.01),
  ylim = c(min(stats$points_wo) - 0.01, max(stats$points_wo) + 0.01)
)

# plot points per weighted opportunity (y-axis) by weighted opportunity (x-axis)
plot <- ggplot(stats, aes(x = wo, y = points_wo)) +
  geom_point(
    aes(
      fill = ifelse(team %in% flip_color, team_sec, team_pri),
      color = ifelse(team %in% flip_color, team_pri, team_sec)
    ),
    shape = 21,
    size = 2,
    stroke = 1
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
    max.overlaps = Inf
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
    y = median(stats$points_wo, na.rm = TRUE),
    yend = median(stats$points_wo, na.rm = TRUE),
    color = color$london[[3]],
    linewidth = 0.5,
    linetype = "dashed",
    alpha = 0.6
  ) +
  annotate(
    "segment",
    x = median(stats$wo, na.rm = TRUE),
    xend = median(stats$wo, na.rm = TRUE),
    y = lims$ylim[1] + 0.01,
    yend = lims$ylim[2] - 0.01,
    color = color$london[[3]],
    linewidth = 0.5,
    linetype = "dashed",
    alpha = 0.6
  ) +
  labs(
    title = "Opportunity Knocks",
    subtitle = paste0("Back Production in Week ", current_week),
    caption = paste0(
      "Charting: Lukas Nesheim (data via nflverse, Fantasy Points Data Suite, NFL+)\n  ",
      "\u2020 A weighted opportunity is defined as a carry or target multiplied by its relative scoring potential." # nolint
    ),
    x = expression("Weighted Opportunities"^"\u2020"),
    y = expression("Fantasy Points per Weighted Opportunity"^"\u2020")
  ) +
  theme_baseline_gg() +
  theme(
    plot.subtitle = element_text(family = "Montserrat-SemiBold", color = color$london[[5]])
  )

ggsave("rushing.png", plot, height = 6, width = 6, units = "in", dpi = 600)