library(tidyverse)

# Read data
df <- read_csv("./data/bpm_data_to_merge.csv")
injuries <- read_csv("./data/injuries_code_classified.csv")

# Filter seasons
df <- df %>% filter(season >= 2010 & season <= 2024)

# Rename column
df <- df %>% rename(player = athlete_display_name)

# Remove ambiguous player names (multiple athlete_ids per name)
ambiguous_names <- df %>%
  group_by(player) %>%
  summarize(n_ids = n_distinct(athlete_id)) %>%
  filter(n_ids > 1) %>%
  pull(player)
df <- df %>% filter(!player %in% ambiguous_names)

# Create game count per team per season
df <- df %>%
  mutate(team_game_key = paste(season, team_id, game_id, sep = "_")) %>%
  arrange(season, team_id, game_date) %>%
  group_by(season, team_id) %>%
  mutate(game_count = dense_rank(game_date)) %>%
  ungroup() %>%
  select(-team_game_key)

# --- Creating periods ---

# 1. Comeback events
comebacks <- injuries %>%
  filter(!is.na(Acquired)) %>%
  select(player = Acquired, event_date = Date) %>%
  mutate(event_type = "comeback")

# 2. Season starts
season_starts <- df %>%
  arrange(player, season, game_date) %>%
  group_by(player, season) %>%
  slice(1) %>%
  ungroup() %>%
  select(player, season, event_date = game_date) %>%
  mutate(event_type = "season_start")

# 3. Combine events and assign period numbers
events <- bind_rows(season_starts %>% select(player, event_date, event_type),
                    comebacks %>% select(player, event_date, event_type)) %>%
  mutate(event_date = as.Date(event_date)) %>%
  arrange(player, event_date) %>%
  group_by(player) %>%
  mutate(period = row_number()) %>%
  ungroup()

# 4. Assign period to each game
df <- df %>%
  mutate(game_date = as.Date(game_date)) %>%
  arrange(player, game_date) %>%
  left_join(events %>% select(player, event_date, period), 
            by = c("player" = "player", "game_date" = "event_date")) %>%
  group_by(player) %>%
  fill(period, .direction = "down") %>%
  ungroup()

# --- Regression by injury type ---

# Prepare model_df
model_df <- df %>%
  group_by(player, period) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  arrange(player, season, period) %>%
  group_by(player) %>%
  mutate(previous_period_zscore = lag(bpm_zscore),
         difference = bpm_zscore - previous_period_zscore) %>%
  ungroup() %>%
  mutate(comeback_from_injury = ifelse(is.na(comeback_from_injury), "None", comeback_from_injury)) %>%
  select(previous_period_zscore, bpm_zscore, difference, comeback_from_injury) %>%
  drop_na()

# Regression loop by injury type
results <- map_df(unique(model_df$comeback_from_injury), function(injury) {
  subset <- model_df %>% filter(comeback_from_injury == injury)
  if (nrow(subset) == 0) return(NULL)
  fit <- lm(difference ~ previous_period_zscore, data = subset)
  tibble(
    comeback_from_injury = injury,
    r2 = summary(fit)$r.squared,
    rmse = sqrt(mean(residuals(fit)^2)),
    coef = coef(fit)[["previous_period_zscore"]],
    intercept = coef(fit)[["(Intercept)"]],
    n_samples = nrow(subset)
  )
})

results <- results %>% arrange(desc(r2))
print(results)