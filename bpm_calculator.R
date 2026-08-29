library(tidyverse)
source("bpm_calculator.R") # assuming BPMCalculator R6 class is in bpm_calculator.R

# Load your box score data
box_scores <- read_csv("./data/player_box_standard_with_team.csv", col_types = cols(.default = "c"))
box_scores <- box_scores %>% mutate(across(where(is.character), as.numeric, .names = "num_{col}"))

# Rename columns if needed to match expected format
column_mapping <- c(
  "points" = "points",
  "field_goals_made" = "field_goals_made",
  "field_goals_attempted" = "field_goals_attempted",
  "three_point_field_goals_made" = "three_point_field_goals_made",
  "three_point_field_goals_attempted" = "three_point_field_goals_attempted",
  "free_throws_made" = "free_throws_made",
  "free_throws_attempted" = "free_throws_attempted",
  "offensive_rebounds" = "offensive_rebounds",
  "defensive_rebounds" = "defensive_rebounds",
  "rebounds" = "rebounds",
  "assists" = "assists",
  "steals" = "steals",
  "blocks" = "blocks",
  "turnovers" = "turnovers",
  "fouls" = "fouls",
  "plus_minus" = "plus_minus"
)
box_scores <- box_scores %>%
  rename(!!!column_mapping) %>%
  filter(season_type == 2)

# Initialize BPM calculator
bpm_calc <- BPMCalculator$new()

# Calculate BPM metrics
bpm_results <- bpm_calc$calculate_bpm(box_scores)

cat("BPM calculations complete. Results saved to player_bpm_results.csv\n")

# Calculate BPM*min and season_bpm
bpm_results <- bpm_results %>%
  mutate(`BPM*min` = BPM * minutes) %>%
  group_by(athlete_id, season) %>%
  mutate(season_bpm = sum(`BPM*min`, na.rm = TRUE) / sum(minutes, na.rm = TRUE)) %>%
  ungroup()

# Save results
write_csv(bpm_results, "./data/player_bpm_results.csv")

# Top 20 2025 players with at least 1000 minutes
top_2025 <- bpm_results %>%
  group_by(athlete_id, season) %>%
  filter(sum(minutes, na.rm = TRUE) >= 1000) %>%
  ungroup() %>%
  arrange(desc(season_bpm)) %>%
  distinct(athlete_id, season, .keep_all = TRUE) %>%
  filter(season == 2025) %>%
  slice_head(n = 20) %>%
  select(season, athlete_display_name, season_bpm)

top_2025