# ---- Creating the data ----
# Install IRkernel if needed
# install.packages('IRkernel')
# IRkernel::installspec()
library(tidyverse)
library(hoopR)

# Load NBA player box data and save to CSV
player_box_data <- load_nba_player_box(seasons = 2002:2025)
write_csv(player_box_data, "./data/player_box_standard.csv")

# ---- Importing and cleaning the data ----
# Read data
df <- read_csv("./data/player_box_standard.csv")
df$plus_minus[df$plus_minus == "--"] <- NA

cols <- c(
  "minutes", "field_goals_made", "field_goals_attempted",
  "three_point_field_goals_made", "three_point_field_goals_attempted",
  "free_throws_made", "free_throws_attempted",
  "offensive_rebounds", "defensive_rebounds", "rebounds", "assists",
  "steals", "blocks", "turnovers", "fouls", "plus_minus", "points"
)

df[cols] <- lapply(df[cols], function(x) replace(x, is.na(x), 0))

# Top scorers
df %>%
  group_by(athlete_id) %>%
  summarise(total_points = sum(points)) %>%
  arrange(desc(total_points)) %>%
  slice_head(n = 10)

# Merge in team box data
team_data <- read_csv("./data/team_box_standard.csv")
df <- left_join(df, team_data, by = c("game_id", "team_id"), suffix = c("", "_team"))

df <- df %>%
  rename(
    points_team = team_score_team,
    fga_team = field_goals_attempted_team,
    fta_team = free_throws_attempted_team,
    orb_team = offensive_rebounds_team,
    tov_team = turnovers_team,
    minutes_team = minutes_played_team
  )

df <- df %>% arrange(game_date)

df_roles <- read_csv("./data/player_bpm_results.csv")
df <- df %>% arrange(athlete_id, season, game_id)
df_roles <- df_roles %>% arrange(athlete_id, season, game_id)
df$position_value <- df_roles$position
df$offensive_role <- df_roles$offensive_role

write_csv(df, "./data/player_box_standard_with_team.csv")

# ---- Possessions Calculation ----
df$possessions_team <- (
  df$fga_team + 0.44 * df$fta_team + df$tov_team - df$orb_team
)

df %>%
  filter(season == 2025) %>%
  group_by(team_display_name, season) %>%
  summarise(possessions_stats = summary(possessions_team))

# Calculate team minutes / 5 (for 5 players)
df$minutes_team <- df %>%
  group_by(game_id, team_id) %>%
  mutate(total_minutes = sum(minutes) / 5) %>%
  pull(total_minutes)

overtime <- df %>% filter(minutes_team >= 51)
print(length(unique(overtime$game_id)))

# Player Possessions ≈ Team Possessions × (Player Minutes / Team Minutes)
df$possessions <- round(df$possessions_team * (df$minutes / df$minutes_team))
df$possessions[is.na(df$possessions)] <- 0

# ---- Per 100 Possession Statistics ----
df <- df %>% rename(
  "3pm" = three_point_field_goals_made,
  "3pa" = three_point_field_goals_attempted,
  fgm = field_goals_made,
  fga = field_goals_attempted,
  ftm = free_throws_made,
  fta = free_throws_attempted,
  orb = offensive_rebounds,
  drb = defensive_rebounds
)

cols_to_convert <- c("points", "3pm", "assists", "turnovers", "orb", "drb", "steals", "blocks", "fouls", "fga", "fta")
for (col in cols_to_convert) {
  cp1p <- paste0(col, "_per_100_possessions")
  df[[cp1p]] <- round(replace((df[[col]] / df$possessions) * 100, is.na((df[[col]] / df$possessions) * 100), 0), 2)
}
df <- df %>% mutate(across(everything(), ~na_if(.x, Inf)))
df <- df %>% mutate(across(everything(), ~na_if(.x, -Inf)))

# ---- Calculate raw BPM ----
calculate_position_value <- function(row) row[["position_value"]]
calculate_offensive_role <- function(row) row[["offensive_role"]]

calculate_raw_bpm <- function(row) {
  pos <- as.numeric(calculate_position_value(row))
  off_role <- as.numeric(calculate_offensive_role(row))

  interpolate_pos <- function(pos1_val, pos5_val, position) {
    pos1_val + (pos5_val - pos1_val) * (position - 1) / 4
  }
  interpolate_role <- function(role1_val, role5_val, role) {
    role1_val + (role5_val - role1_val) * (role - 1) / 4
  }

  pts_coeff <- 0.860
  tpm_coeff <- 0.389
  to_coeff <- -0.964
  fouls_coeff <- -0.367

  ast_coeff <- interpolate_pos(0.580, 1.034, pos)
  orb_coeff <- interpolate_pos(0.613, 0.181, pos)
  drb_coeff <- interpolate_pos(0.116, 0.181, pos)
  stl_coeff <- interpolate_pos(1.369, 1.008, pos)
  blk_coeff <- interpolate_pos(1.327, 0.703, pos)

  fga_coeff <- interpolate_role(-0.560, -0.780, off_role)
  fta_coeff <- interpolate_role(-0.246, -0.343, off_role)

  bpm <-
    pts_coeff * row["points_per_100_possessions"] +
      tpm_coeff * row["3pm_per_100_possessions"] +
      ast_coeff * row["assists_per_100_possessions"] +
      to_coeff * row["turnovers_per_100_possessions"] +
      orb_coeff * row["orb_per_100_possessions"] +
      drb_coeff * row["drb_per_100_possessions"] +
      stl_coeff * row["steals_per_100_possessions"] +
      blk_coeff * row["blocks_per_100_possessions"] +
      fouls_coeff * row["fouls_per_100_possessions"] +
      fga_coeff * row["fga_per_100_possessions"] +
      fta_coeff * row["fta_per_100_possessions"]

  pos_adjustment <- if (pos <= 3) -0.818 * (3 - pos) / 2 else 0
  role_adjustment <- if (off_role <= 3) {
    -2.774 + 2.774 * (off_role - 1) / 2
  } else {
    2.774 * (off_role - 3) / 2
  }

  as.numeric(bpm) + pos_adjustment + role_adjustment
}

df$raw_bpm <- apply(df, 1, calculate_raw_bpm)

# ---- Minutes Z-score ----
df$minutes_z_score_player_season <- df %>%
  group_by(athlete_id, season) %>%
  mutate(zscore = (minutes - mean(minutes, na.rm = TRUE)) / sd(minutes, na.rm = TRUE)) %>%
  pull(zscore)

df$season_avg_minutes <- df %>%
  group_by(season, athlete_id) %>%
  mutate(avg_mins = mean(minutes, na.rm = TRUE)) %>%
  pull(avg_mins)

df <- df %>% filter(minutes >= 12)
minimal_cols <- c("season", "athlete_display_name", "game_date", "minutes", "points", "rebounds", "assists", "raw_bpm", "minutes_z_score_player_season")
minimal_df <- df[minimal_cols]

# ---- Team Adjustment Calculation ----
calculate_team_adjustment <- function(df, season_year) {
  season_df <- df %>% filter(season == season_year, season_type == 2)
  team_stats <- season_df %>%
    group_by(team_id) %>%
    summarise(points_team = mean(points_team, na.rm = TRUE),
              possessions_team = mean(possessions_team, na.rm = TRUE))
  league_avg_efficiency <- sum(season_df$points_team, na.rm = TRUE) / sum(season_df$possessions_team, na.rm = TRUE) * 100
  team_stats$team_efficiency <- (team_stats$points_team / team_stats$possessions_team) * 100 - league_avg_efficiency
  team_stats$avg_lead <- 0
  team_stats$lead_adjustment <- -0.35 / 2 * team_stats$avg_lead
  team_stats$adjusted_team_efficiency <- team_stats$team_efficiency + team_stats$lead_adjustment

  season_df <- left_join(season_df, team_stats[, c("team_id", "adjusted_team_efficiency")], by = "team_id")
  team_total_possessions <- season_df %>% group_by(team_id) %>% summarise(team_total_possessions = sum(possessions, na.rm = TRUE))
  season_df <- left_join(season_df, team_total_possessions, by = "team_id")
  season_df$possession_pct <- season_df$possessions / season_df$team_total_possessions
  season_df$weighted_raw_bpm <- season_df$raw_bpm * season_df$possession_pct
  team_weighted_bpm <- season_df %>% group_by(team_id) %>% summarise(team_raw_bpm_sum = sum(weighted_raw_bpm, na.rm = TRUE))
  team_adjustments <- left_join(team_stats, team_weighted_bpm, by = "team_id")
  team_adjustments$team_adjustment <- team_adjustments$adjusted_team_efficiency - team_adjustments$team_raw_bpm_sum
  team_adjustments[, c("team_id", "team_adjustment", "adjusted_team_efficiency")]
}

all_team_adjustments <- list()
for (season in unique(df$season)) {
  if (!is.na(season)) {
    season_adjustments <- calculate_team_adjustment(df, season)
    season_adjustments$season <- season
    all_team_adjustments[[length(all_team_adjustments) + 1]] <- season_adjustments
  }
}
team_adjustments_df <- bind_rows(all_team_adjustments)
df <- left_join(df, team_adjustments_df, by = c("team_id", "season"))
df$adjusted_bpm <- df$raw_bpm + df$team_adjustment
df$adjusted_bpm[is.na(df$adjusted_bpm)] <- 0

# ---- Cumulative and Average BPM Contributions ----
df$raw_bpm_mins <- df$raw_bpm * df$minutes
df$bpm_mins <- df$adjusted_bpm * df$minutes
df <- df %>% filter(minutes >= 12)
df$cumulative_raw_bpm_contribution <- df %>%
  group_by(season, athlete_id) %>%
  mutate(val = cumsum(raw_bpm_mins) / cumsum(minutes)) %>%
  pull(val)
df$cumulative_bpm_contribution <- df %>%
  group_by(season, athlete_id) %>%
  mutate(val = cumsum(bpm_mins) / cumsum(minutes)) %>%
  pull(val)
df$average_bpm_contribution <- (df$raw_bpm + df$adjusted_bpm) / 2
df$avg_mins <- df$average_bpm_contribution * df$minutes
df$cumulative_average_bpm_contribution <- df %>%
  group_by(season, athlete_id) %>%
  mutate(val = cumsum(avg_mins) / cumsum(minutes)) %>%
  pull(val)
df$final_average_bpm_contribution <- df %>%
  group_by(season, athlete_id) %>%
  mutate(val = sum(avg_mins) / sum(minutes)) %>%
  pull(val)

df$cumulative_average_z_score_player_season <- df %>%
  group_by(season) %>%
  mutate(val = (cumulative_average_bpm_contribution - mean(cumulative_average_bpm_contribution, na.rm = TRUE) ) /
                sd(cumulative_average_bpm_contribution, na.rm = TRUE)) %>%
  pull(val)

# ---- Testing and Export ----
regular_season <- df %>% filter(season_type == 2)
playoffs <- df %>% filter(season_type == 3)

write_csv(regular_season, "./data/regular_season_bpm.csv")
write_csv(playoffs, "./data/playoffs_bpm.csv")

# Plotting (example)
# library(ggplot2)
# ggplot(regular_season, aes(x = cumulative_average_z_score_player_season)) +
#   geom_histogram(bins = 50) +
#   labs(title = "Cumulative Average Z-Score Player Season Distribution",
#        x = "Cumulative Average Z-Score", y = "Frequency")