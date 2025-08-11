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
current_week <- 0

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
rb_stats <- nflfastR::calculate_stats(
  seasons = season,
  summary_level = "season",
  stat_type = "player",
  season_type = "REG"
) %>%
  filter(position == "RB") %>%
  transmute(
    id = player_id,
    name = clean_player_names(player_display_name),
    short = player_name,
    pos = position,
    team = recent_team,
    gp = games,
    car = carries,
    rush_yds = rushing_yards,
    rush_tds = rushing_tds,
    rush_epa = rushing_epa,
    tgt = targets,
    rec = receptions,
    rec_yds = receiving_yards,
    rec_tds = receiving_tds,
    rec_epa = receiving_epa,
    fd = rushing_first_downs + receiving_first_downs,
    fpts = fantasy_points_ppr + 0.5 * fd,
    fppg = fpts / gp,
    fppc = fpts / car,
    fppr = fpts / rec,
    fppt = fpts / (car + rec)
  ) %>%
  arrange(desc(fpts)) %>%
  mutate(rank = seq_len(nrow(.))) %>%
  slice(1:40)

# load fantasy points data weighted opportunity stats
wo_stats <- read_csv(here("data", "rushing_2024.csv")) %>%
  clean_names() %>%
  transmute(name = clean_player_names(name), ypr, ypt, wo, wopg = wo_g)

# merge base stats and weighted opportunity stats with team info
stats <- rb_stats %>%
  stringdist_inner_join(
    wo_stats,
    by = "name",
    max_dist = 1
  ) %>%
  inner_join(
    teams,
    by = "team"
  ) %>%
  mutate(fppwo = fpts / wo) %>%
  select(id, name = name.x, everything(), -name.y)

# set dynamic axis limits
wopg_fppt_lims <- list(
  xlim = c(min(stats$wopg) - 0.01, max(stats$wopg) + 0.01),
  ylim = c(min(stats$fppwo) - 0.01, max(stats$fppwo) + 0.01)
)

# plot fppt (y-axis) by wopg (x-axis)
wopg_fppwo <- ggplot(
  stats,
  aes(x = wopg, y = fppwo)
) +
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
    limits = wopg_fppt_lims$xlim
  ) +
  scale_y_continuous(
    limits = wopg_fppt_lims$ylim,
    breaks = seq(0.9, 1.5, by = 0.2)
  ) +
  annotate(
    "segment",
    x = wopg_fppt_lims$xlim[1] + 0.01,
    xend = wopg_fppt_lims$xlim[2] - 0.01,
    y = median(stats$fppwo, na.rm = TRUE),
    yend = median(stats$fppwo, na.rm = TRUE),
    color = color$london[[3]],
    linewidth = 0.5,
    linetype = "dashed",
    alpha = 0.6
  ) +
  annotate(
    "segment",
    x = median(stats$wopg, na.rm = TRUE),
    xend = median(stats$wopg, na.rm = TRUE),
    y = wopg_fppt_lims$ylim[1] + 0.01,
    yend = wopg_fppt_lims$ylim[2] - 0.01,
    color = color$london[[3]],
    linewidth = 0.5,
    linetype = "dashed",
    alpha = 0.6
  ) +
  labs(
    title = "Opportunity Knocks",
    subtitle = "Back Production in 2024",
    caption = paste0(
      "Charting: Lukas Nesheim (data via nflverse, Fantasy Points Data Suite)\n   ",
      "\u2020 A weighted opportunity is defined as a carry or target multiplied by its relative scoring potential."
    ),
    x = expression("Weighted Opportunities"^"\u2020"),
    y = expression("Fantasy Points per Weighted Opportunity"^"\u2020")
  ) +
  theme_baseline_gg()

ggsave("wopg_fppwo.png", wopg_fppwo, height = 6, width = 6, units = "in", dpi = 600)