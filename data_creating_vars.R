# Load required libraries
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)

options(dplyr.summarise.inform = FALSE)

# Set options for display
options(max.print = 1000)

# Load datasets
injuries <- read_csv("./data/injuries.csv") %>%
  select(-"Unnamed: 0")

injuries <- injuries %>% filter(Date >= '2001-07-01')

injuries$Relinquished <- str_trim(injuries$Relinquished)
injuries$Acquired <- str_trim(injuries$Acquired)

# Check Klay Thompson
injuries %>% filter(Relinquished == "Klay Thompson" | Acquired == "Klay Thompson") %>%
  write_csv("temp.csv")

df <- read_csv("./Data/regular_season_bpm.csv")

player_bpm <- read_csv("./data/player_bpm_results.csv")

player_bpm <- player_bpm %>%
  rename(calculated_position = position) %>%
  select(game_id, season, athlete_id, athlete_display_name, calculated_position, BPM, season_bpm)

df <- df %>%
  left_join(player_bpm, by = c("game_id", "season", "athlete_id", "athlete_display_name")) %>%
  rename(bpm = BPM)

# Filter for players with >1000 minutes and drop duplicates
df <- df %>%
  arrange(desc(season_bpm)) %>%
  group_by(athlete_display_name, season) %>%
  filter(sum(minutes, na.rm=TRUE) > 1000) %>%
  ungroup() %>%
  distinct(athlete_display_name, season, .keep_all = TRUE)

# Remove All-Star teams
teams_to_remove <- c('All-Stars', 'Team LeBron', 'Team Giannis', 'World', 'USA', 'Team Stephen', 'Team Candace', 'Team Shaq', 'Team Durant', 'Team Kenny')
df <- df %>% filter(!(team_name %in% teams_to_remove))

write_csv(df, "./data/merged_bpm_data.csv")

# Find athlete names with multiple IDs
name_id_mapping <- df %>%
  group_by(athlete_display_name) %>%
  summarise(n_ids = n_distinct(athlete_id)) %>%
  filter(n_ids > 1)

problematic_records <- df %>%
  filter(athlete_display_name %in% name_id_mapping$athlete_display_name) %>%
  select(athlete_display_name, athlete_id, team_name) %>%
  distinct()

cat("Found", nrow(name_id_mapping), "athlete names with multiple IDs\n")
print(problematic_records)

gpt_injury_data <- read_csv("./Data/cleaned_injuries_recent2.csv")
gpt_injury_data$Player <- str_trim(gpt_injury_data$Player)

merged_injuries <- injuries %>%
  left_join(gpt_injury_data, by = c("Relinquished" = "Player", "Date" = "Date"))

print(table(merged_injuries$Injury))

print(table(merged_injuries$`Injury Category`))

# BPM * min should accumulate by period, then divide by games
# Calculate season_bpm
df <- df %>%
  mutate(`bpm*mins` = bpm * minutes) %>%
  group_by(season, athlete_id) %>%
  mutate(season_bpm = sum(`bpm*mins`, na.rm=TRUE) / sum(minutes, na.rm=TRUE)) %>%
  ungroup()

df <- df %>%
  group_by(athlete_id, season) %>%
  mutate(season_bpm_zscore = ifelse(sum(minutes, na.rm=TRUE) >= 1000,
                                    (season_bpm - mean(season_bpm, na.rm=TRUE)) / sd(season_bpm, na.rm=TRUE),
                                    NA_real_)) %>%
  ungroup()

write_csv(df, "./data/season_bpm_zscore.csv")

# Best seasons
best_seasons <- df %>%
  group_by(athlete_id, season) %>%
  filter(sum(minutes, na.rm=TRUE) >= 1200) %>%
  arrange(desc(season_bpm_zscore)) %>%
  distinct(athlete_id, season, .keep_all=TRUE) %>%
  ungroup() %>%
  head(20) %>%
  select(season, athlete_display_name, season_bpm_zscore)

write_csv(best_seasons, "./data/to_display/top_bpm_players.csv")

best_2025 <- df %>%
  group_by(athlete_id, season) %>%
  filter(sum(minutes, na.rm=TRUE) >= 1200) %>%
  arrange(desc(season_bpm_zscore)) %>%
  distinct(athlete_id, season, .keep_all=TRUE) %>%
  filter(season == 2025) %>%
  head(20) %>%
  select(season, athlete_display_name, season_bpm_zscore)

write_csv(best_2025, "./data/to_display/top_bpm_players_2025.csv")

# Validate z-score calculation for LeBron James in 2010
std_2010 <- sd(df$season_bpm[df$season == 2010], na.rm=TRUE)
mean_2010 <- mean(df$season_bpm[df$season == 2010], na.rm=TRUE)
lebron_bpm_2010 <- max(df$season_bpm[df$season == 2010 & df$athlete_display_name == "LeBron James"], na.rm=TRUE)
lebron_bpm_2010_zscore <- (lebron_bpm_2010 - mean_2010) / std_2010
cat("Standard Deviation for 2010 season:", std_2010, "\n")
cat("Mean for 2010 season:", mean_2010, "\n")
cat("LeBron James' BPM in 2010:", lebron_bpm_2010, "\n")
cat("LeBron James' BPM Z-Score in 2010:", lebron_bpm_2010_zscore, "\n")

# Overall z-score and mean
std_all <- sd(df$season_bpm, na.rm=TRUE)
mean_all <- mean(df$season_bpm, na.rm=TRUE)
cat("Standard Deviation for 2002-2025 season:", std_all, "\n")
cat("Mean for 2002-2025 season:", mean_all, "\n")
lebron_bpm_2010_zscore_global <- (lebron_bpm_2010 - mean_all) / std_all
cat("LeBron James' BPM Z-Score in 2010 (global):", lebron_bpm_2010_zscore_global, "\n")

# Visualizing Z-score distributions for 2025
plot_df <- df %>%
  group_by(athlete_id, season) %>%
  filter(sum(minutes, na.rm=TRUE) >= 1000) %>%
  ungroup() %>%
  filter(season == 2025) %>%
  distinct(athlete_id, season, .keep_all=TRUE)

ggplot(plot_df, aes(x=season_bpm_zscore)) +
  geom_histogram(bins=30, fill="forestgreen", alpha=0.5) +
  geom_vline(aes(xintercept=mean(season_bpm_zscore, na.rm=TRUE)), color="red", linetype="dashed") +
  labs(title="Season BPM Z-Score Distribution in 2025",
       x="Season BPM Z-Score", y="Count") +
  theme_minimal()

# Visualizing Z-score distributions by season
plot_df2 <- df %>%
  distinct(athlete_id, season, .keep_all=TRUE)
ggplot(plot_df2, aes(x=factor(season), y=season_bpm_zscore)) +
  geom_boxplot() +
  labs(title="Season BPM Z-Score Distribution by Season",
       x="Season", y="Season BPM Z-Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("./data/to_display/season_bpm_zscore_boxplot.png", width=14, height=7, dpi=300)

# Injury categorization function in R
categorize_injury <- function(injury_desc) {
  if (is.na(injury_desc)) return(NA)
  injury_lower <- tolower(injury_desc)
  if (str_detect(injury_lower, "torn acl|acl tear")) return("ACL Tear")
  else if (str_detect(injury_lower, "achilles")) {
    if (str_detect(injury_lower, "tear|rupture|torn")) return("Achilles (Tear)")
    else return("Achilles (Minor)")
  } else if (str_detect(injury_lower, "ankle")) return("Ankle")
  else if (str_detect(injury_lower, "back|spine|vertebrae|disc")) return("Back")
  else if (str_detect(injury_lower, "calf")) return("Calf")
  else if (str_detect(injury_lower, "concussion")) return("Concussion")
  else if (str_detect(injury_lower, "elbow")) return("Elbow")
  else if (str_detect(injury_lower, "eye|orbital")) return("Eye")
  else if (str_detect(injury_lower, "finger|thumb|pinky|pinkie")) return("Finger")
  else if (str_detect(injury_lower, "foot|toe|plantar|fasciitis")) return("Foot")
  else if (str_detect(injury_lower, "groin")) return("Groin")
  else if (str_detect(injury_lower, "hamstring")) return("Hamstring")
  else if (str_detect(injury_lower, "hand")) return("Hand")
  else if (str_detect(injury_lower, "head|neck|cervical")) return("Head/Neck")
  else if (str_detect(injury_lower, "hip")) return("Hip")
  else if (str_detect(injury_lower, "(torn|ruptured|fractured).*knee")) return("Knee (Major)")
  else if (str_detect(injury_lower, "knee|patella|meniscus|mcl|kneecap")) return("Knee (Minor)")
  else if (str_detect(injury_lower, "leg|shin")) return("Leg")
  else if (str_detect(injury_lower, "quadricep|quad|thigh")) return("Quad/Thigh")
  else if (str_detect(injury_lower, "shoulder")) return("Shoulder")
  else if (str_detect(injury_lower, "surgery|surgical|arthroscopic")) return("Surgery")
  else if (str_detect(injury_lower, "wrist")) return("Wrist")
  else if (str_detect(injury_lower, "illness|infection|virus|flu|kidney|heart|irregular heartbeat")) return("Illness/Medical")
  else if (str_detect(injury_lower, "strain|sprain|bruise|contusion|inflammation")) return("General Strain/Sprain")
  else return("Other")
}

extract_and_categorize_injury <- function(row) {
  if (!is.na(row["Relinquished"])) {
    notes <- tolower(as.character(row["Notes"]))
    injury_desc <- NA
    if (str_detect(notes, "recovering from")) {
      m <- str_match(notes, "recovering from ([^()]+)")
      if (!is.na(m[2])) injury_desc <- str_trim(m[2])
    } else if (str_detect(notes, "\\(")) {
      injury_desc <- str_trim(str_split(notes, "\\(")[[1]][1])
    } else {
      injury_desc <- str_trim(notes)
    }
    if (!is.na(injury_desc) && nchar(injury_desc) > 0) {
      return(categorize_injury(injury_desc))
    }
    return(NA)
  }
  return(NA)
}

injuries$Injury_Category <- apply(injuries, 1, extract_and_categorize_injury)

write_csv(injuries, "./data/injuries_code_classified.csv")

# Assign periods based on comeback or season start
comebacks <- injuries %>%
  filter(!is.na(Acquired)) %>%
  select(Acquired, Date) %>%
  rename(athlete_display_name = Acquired, event_date = Date) %>%
  mutate(event_type = "comeback")

season_starts <- df %>%
  arrange(athlete_display_name, season, game_date) %>%
  group_by(athlete_display_name, season) %>%
  slice(1) %>%
  ungroup() %>%
  select(athlete_display_name, season, game_date) %>%
  rename(event_date = game_date) %>%
  mutate(event_type = "season_start")

events <- bind_rows(season_starts, comebacks) %>%
  mutate(event_date = as.Date(event_date)) %>%
  arrange(athlete_display_name, event_date) %>%
  group_by(athlete_display_name) %>%
  mutate(period = row_number()) %>%
  ungroup()

df$game_date <- as.Date(df$game_date)
df <- df %>%
  arrange(athlete_display_name, game_date)

assign_period <- function(row, events) {
  player_events <- events %>% filter(athlete_display_name == row["athlete_display_name"])
  prior_events <- player_events %>% filter(event_date <= as.Date(row["game_date"]))
  if (nrow(prior_events) > 0) {
    return(tail(prior_events$period, 1))
  } else {
    return(1)
  }
}

df$period <- apply(df, 1, function(row) assign_period(row, events))

# Save results
write_csv(df, "./data/final_bpm_with_periods.csv")