library(tidyverse)
library(stringr)
library(broom)
library(scales)
library(paletteer)

# Load data
df <- read_csv("./data/df_collapsed.csv")

# df <- read_csv("./data/df_with_bpm_zscores.csv")
# collapse_short_periods_forward <- function(df, min_games = 5) {
#   df <- df
#
#   # Process each player-season group
#   for (key in unique(paste(df$player, df$season, sep = "_"))) {
#     parts <- strsplit(key, "_")[[1]]
#     player <- parts[1]
#     season <- parts[2]
#
#     group <- df[df$player == player & df$season == season, ]
#     period_counts <- table(group$period)
#     periods <- sort(names(period_counts))  # ensure sorted
#     counts <- as.numeric(period_counts)
#
#     period_map <- setNames(rep(NA, length(periods)), periods)
#
#     for (i in length(periods):1) {  # reverse loop
#       p <- periods[i]
#       count <- counts[i]
#       if (count < min_games && i < length(periods)) {
#         next_p <- periods[i + 1]
#         period_map[p] <- next_p
#       } else {
#         period_map[p] <- p
#       }
#     }
#
#     # Apply the map
#     mask <- df$player == player & df$season == season
#     df$period[mask] <- unname(period_map[as.character(df$period[mask])])
#   }
#
#   return(df)
# }
# df <- collapse_short_periods_forward(df)

ages <- read_csv("./data/player_ages.csv")

# Merge age info
df <- df %>%
  left_join(ages, by = c("player" = "Player", "season" = "season"))
# Get last row for each player-period (simulate Python groupby().last())
model_df <- df %>%
  group_by(player, period) %>%
  slice_tail(n = 1) %>%
  ungroup()

# model_df = model_df |> filter(comeback_from_injury != "Other")
# Add previous period zscore and difference
model_df <- model_df %>%
  arrange(player, season, period) %>%
  mutate(
    previous_period_zscore = lag(bpm_zscore),
    next_player = lead(player)
  ) %>%
  mutate(
    previous_period_zscore = if_else(player != next_player, NA_real_, previous_period_zscore),
    difference = -previous_period_zscore + bpm_zscore
  ) %>%
  select(-next_player)
# Fill NA for comeback_from_injury
model_df$comeback_from_injury <- model_df$comeback_from_injury %>% replace_na("None")

# Select model columns and drop NA
model_df <- model_df %>%
  select(Age, previous_period_zscore, bpm_zscore, difference, comeback_from_injury, athlete_position_abbreviation) %>%
  drop_na() |>
  filter(comeback_from_injury != "Other")
view(model_df)
# Summary stats by injury
mean_none <- model_df |>
  filter(comeback_from_injury == "None") |>
  summarise(mean = mean(difference, na.rm = TRUE))
model_df <- model_df |> mutate(previous_period_zscore = previous_period_zscore - mean_none$mean, bpm_zscore = bpm_zscore - mean_none$mean, difference = difference - mean_none$mean)
injury_summary <- model_df %>%
  group_by(comeback_from_injury) %>%
  summarise(
    count = n(),
    mean_diff = mean(difference, na.rm = TRUE),
    sd_diff = sd(difference, na.rm = TRUE),
    min_diff = min(difference, na.rm = TRUE),
    max_diff = max(difference, na.rm = TRUE)
  ) %>%
  arrange(mean_diff)
view(injury_summary)

# Regression for each injury type
results <- model_df %>%
  group_by(comeback_from_injury) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(bpm_zscore ~ previous_period_zscore + Age, data = .x)),
    tidy = map(model, broom::tidy),
    glance = map(model, broom::glance)
  ) %>%
  unnest(glance) %>%
  select(comeback_from_injury, r.squared, sigma, statistic, p.value, df, nobs) %>%
  arrange(desc(r.squared))
print(results)

# Get coefficients for all injuries
coef_results <- results %>%
  left_join(
    model_df %>%
      group_by(comeback_from_injury) %>%
      nest() %>%
      mutate(
        model = map(data, ~ lm(bpm_zscore ~ previous_period_zscore + Age, data = .x)),
        tidy = map(model, broom::tidy)
      ) %>%
      unnest(tidy) %>%
      filter(term == "previous_period_zscore") %>%
      select(comeback_from_injury, estimate),
    by = "comeback_from_injury"
  ) %>%
  rename(coef_previous_period_zscore = estimate)

print(coef_results)

# One-hot encode injury type for full regression
# model_df = select(model_df, -positon)

# group bigs and smalls
# make seperate graphs for each
# centers with no injuries are base group
model_df <- model_df |> mutate(
  position = case_when(
    athlete_position_abbreviation %in% c("C", "PF", "F") ~ "Big",
    athlete_position_abbreviation %in% c("SF", "SG", "PG", "G") ~ "Small",
    TRUE ~ athlete_position_abbreviation
  )
)

model_df_dummies <- model_df %>%
  select(-athlete_position_abbreviation) |>
  mutate(comeback_from_injury = fct_explicit_na(factor(comeback_from_injury), "None")) %>%
  mutate(comeback_from_injury = fct_infreq(comeback_from_injury)) %>%
  mutate(dummy = 1) %>%
  pivot_wider(names_from = comeback_from_injury, values_from = dummy, values_fill = 0) %>%
  select(-None)

# Multiple regression
# view(model_df_dummies)
view(model_df_dummies)
model_df_dummies <- model_df_dummies |> select(-difference)
multi_model <- lm(bpm_zscore ~ previous_period_zscore + Age + ., data = model_df_dummies)
multi_model_summary <- broom::tidy(multi_model)
multi_model_summary <- multi_model_summary %>% arrange(estimate)
multi_model_summary <- tidy(multi_model) %>%
  arrange(estimate) %>%
  mutate(p.value = number(p.value, accuracy = 0.000001)) # 6 digits
view(multi_model_summary)
# add sample size
# Coefficients mean if player has same age and previous score, they are predicted to lose that amount, e.g. -.87 for achilles

# achilles <- model_df %>% filter(comeback_from_injury == "Achilles (Tear)")
# print(achilles)

# If you want to plot results
model_df

# put positions/age on seperate graph
view(multi_model_summary)
g <- ggplot(multi_model_summary |> filter(!(term %in% c("(Intercept)", "previous_period_zscore", "positionSmall", "Age")))) +
  geom_col(aes(x = reorder(term, estimate), y = estimate, fill = as.numeric(p.value))) +
  coord_flip() +
  labs(
    title = "Multiple Regression Coefficients",
    x = "Term",
    y = "Coefficient",
    fill = "p-value"
  ) +
  theme_minimal() +
  scale_fill_paletteer_c("grDevices::Sunset")
# save the graph
g
ggsave("./to_display/multi_regression_coefficients.png", g, width = 17, height = 9, bg = "white")
model_df_dummies

# Note: if a player is bad, and gets injured, he will not play 12 minutes in a game,
# meaning there is a selection bias, because bad players won't play, while good players will.
# None does not account for this, because bad players with minor injuries fall out of the rotation.
pred_df <- read_csv("./data/pred_df.csv")
# pred_df <- pred_df |> select(-player)
pred_df_dummies <- pred_df |>
  select(-player) %>%
  mutate(comeback_from_injury = fct_explicit_na(factor(comeback_from_injury), "None")) %>%
  mutate(comeback_from_injury = fct_infreq(comeback_from_injury)) %>%
  mutate(dummy = 1) %>%
  pivot_wider(names_from = comeback_from_injury, values_from = dummy, values_fill = 0)
pred_df_dummies

# This code ensures pred_df_dummies has the same columns as model_df_dummies,
# except for 'athlete_position_abbreviation', and fills missing columns with 0.

# Remove 'athlete_position_abbreviation' from both sets
model_cols <- colnames(model_df_dummies)
pred_cols <- colnames(pred_df_dummies)

# Find which columns are missing from pred_df_dummies
missing_cols <- setdiff(model_cols, pred_cols)
missing_cols
pred_df_dummies
# Add missing columns to pred_df_dummies with value 0
for (col in missing_cols) {
  pred_df_dummies[[col]] <- 0
}

# Ensure columns are in the same order as model_df_dummies (excluding position)
pred_df_dummies <- pred_df_dummies[, model_cols]
# View results (optional)
view(pred_df_dummies)
# View results
multi_model
predictions <- predict(multi_model, newdata = pred_df_dummies)

pred_df_dummies
predictions

# Add predictions to pred_df
pred_df <- pred_df %>%
  mutate(predicted_bpm_zscore = predictions)
pred_df <- pred_df |>
  select(-bpm_zscore, -difference) |>
  mutate(predicted_difference = -previous_period_zscore + predicted_bpm_zscore)
view(pred_df)
pred_df
pred_df |> write_csv("./to_display/results.csv")




g <- ggplot(multi_model_summary |> filter((term %in% c("previous_period_zscore", "positionSmall", "Age")))) +
  geom_col(aes(x = reorder(term, estimate), y = estimate, fill = as.numeric(p.value))) +
  coord_flip() +
  labs(
    title = "Multiple Regression Coefficients",
    x = "Term",
    y = "Coefficient",
    fill = "p-value"
  ) +
  theme_minimal() +
  scale_fill_paletteer_c("grDevices::Sunset")
# save the graph
g
ggsave("./to_display/multi_regression_coefficients2.png", g, width = 17, height = 9, bg = "white")
