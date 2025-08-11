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
wr_stats <- nflfastR::calculate_stats(
  seasons = season,
  summary_level = "season",
  stat_type = "player",
  season_type = "REG"
) %>%
  filter(position == "WR") %>%
  transmute(
    id = player_id,
    name = player_display_name,
    short = player_name,
    pos = position,
    team = recent_team,
    gp = games,
    tgt = targets,
    rec = receptions,
    td = receiving_tds,
    fum = receiving_fumbles,
    iay = receiving_air_yards,
    yac = receiving_yards_after_catch,
    fd = receiving_first_downs,
    racr,
    tgt_share = target_share,
    ay_share = air_yards_share,
    wopr,
    wopr_scaled = wopr * (17 / gp),
    fpts = fantasy_points_ppr + 0.5 * fd,
    fppg = fpts / gp,
    fppt = fpts / tgt,
    fppr = fpts / rec
  ) %>%
  arrange(desc(fpts)) %>%
  mutate(rank = seq_len(nrow(.))) %>%
  slice(1:50)

# load play-by-play stats
pbp_stats <- nflfastR::load_pbp(season) %>%
  filter(complete_pass == 1) %>%
  filter(air_yards < yardline_100) %>%
  filter(!is.na(xyac_epa)) %>%
  group_by(receiver_id) %>%
  summarize(
    epa_oe = mean(yac_epa - xyac_epa),
    xfd = mean(xyac_fd),
    fd_oe = mean(first_down - xyac_fd),
    yac_epa = sum(yac_epa),
    xyac = sum(xyac_mean_yardage),
    xyac_epa = sum(xyac_epa)
  ) %>%
  ungroup() %>%
  select(id = receiver_id, everything())

# merge base stats and play-by-play stats with team info
stats <- wr_stats %>%
  inner_join(
    pbp_stats,
    by = "id"
  ) %>%
  inner_join(
    teams,
    by = "team"
  )

# set dynamic axis limits
wopr_fppt_lims <- list(
  xlim = c(min(stats$wopr_scaled) - 0.01, max(stats$wopr_scaled) + 0.01),
  ylim = c(min(stats$fppt) - 0.01, max(stats$fppt) + 0.01)
)

# plot fppt (y-axis) by wopr (x-axis)
wopr_fppt <- ggplot(
  stats,
  aes(x = wopr_scaled, y = fppt)
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
    limits = wopr_fppt_lims$xlim,
    breaks = seq(0.4, 0.8, by = 0.2)
  ) +
  scale_y_continuous(
    limits = wopr_fppt_lims$ylim,
    breaks = seq(1.5, 2.5, by = 0.5)
  ) +
  annotate(
    "segment",
    x = wopr_fppt_lims$xlim[1] + 0.01,
    xend = wopr_fppt_lims$xlim[2] - 0.01,
    y = median(stats$fppt, na.rm = TRUE),
    yend = median(stats$fppt, na.rm = TRUE),
    color = color$london[[3]],
    linewidth = 0.5,
    linetype = "dashed",
    alpha = 0.6
  ) +
  annotate(
    "segment",
    x = median(stats$wopr_scaled, na.rm = TRUE),
    xend = median(stats$wopr_scaled, na.rm = TRUE),
    y = wopr_fppt_lims$ylim[1] + 0.01,
    yend = wopr_fppt_lims$ylim[2] - 0.01,
    color = color$london[[3]],
    linewidth = 0.5,
    linetype = "dashed",
    alpha = 0.6
  ) +
  labs(
    title = "Throw Me the Damn Ball",
    subtitle = "Receiver Production in 2024",
    caption = paste0(
      "Charting: Lukas Nesheim (data via nflverse)\n   ",
      "\u2020 Weighted Opportunity Rating is defined as: (1.5 \u00D7 target share) + (0.7 \u00d7 air yards share)."
    ),
    x = expression("Weighted Opportunity Rating"^"\u2020"),
    y = "Fantasy Points per Target"
  ) +
  theme_baseline_gg()

ggsave("wopr_fppt.png", wopr_fppt, height = 6, width = 6, units = "in", dpi = 600)