library(tidyverse)
library(ggrepel)
library(viridis) # Load
library(paletteer) # Load paletteer for color palettes)
library(ggthemes)
###########
# PLOT 1
###########
df <- read_csv("./data/season_bpm_zscore.csv")

# Filter and deduplicate
plot_df <- df %>%
  group_by(athlete_id, season) %>%
  filter(sum(minutes) >= 1000) %>%
  ungroup() %>%
  distinct(athlete_id, season, .keep_all = TRUE)

# Identify outliers
outliers <- plot_df %>%
  filter(season_bpm_zscore > 4)

# Create label for annotation
outliers$label <- paste0(outliers$athlete_display_name, " (", outliers$season - 1, "-", outliers$season, ")", " - ", round(outliers$season_bpm_zscore, 2))

# Plot
p <- ggplot(plot_df, aes(x = factor(season), y = season_bpm_zscore)) +
  geom_boxplot(aes(group = season, fill = factor(season)), outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.8) +
  geom_text_repel(
    data = outliers,
    aes(label = label),
    color = "red",
    size = 3,
    max.overlaps = 20,
    segment.color = "transparent"
  ) +
  labs(
    title = "Season BPM Z-Score Distribution by Season",
    x = "Season",
    y = "Season BPM Z-Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
p
ggsave("./to_display/season_bpm_zscore_boxplot_r.png", p, width = 17, height = 9, dpi = 300, bg = "white")

###########
# PLOT 2
###########



# If you haven't already, load your data:
# best_2025 <- read.csv("path/to/top_bpm_players_2025.csv")

best_2025 <- read_csv("./data/to_display/top_bpm_players_2025.csv")

ggplot(best_2025, aes(x = reorder(athlete_display_name, season_bpm_zscore), y = season_bpm_zscore, fill = season_bpm_zscore)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colors = paletteer_c("ggthemes::Blue", 30)) +
  coord_flip() + # Optional: flips axes for better readability
  labs(
    title = "Top 20 Players by Season BPM Z-Score in 2025",
    x = "Athlete Display Name",
    y = "Season BPM Z-Score"
  ) +
  theme_minimal(base_size = 16)

ggsave("./to_display/top_bpm_players_2025_barplot_r.png", width = 17, height = 9, dpi = 300, bg = "white")

###########
# PLOT 3
###########


# Assuming df exists in R

plot_df <- read_csv("./data/plot_df.csv")
plot_df <- plot_df |> filter(!is.na(Age))
outliers <- plot_df %>%
  filter(season_bpm_zscore >= 4)
grouped <- plot_df |>
  group_by(Age) |>
  mutate(mean_bpm = mean(season_bpm_zscore, na.rm = TRUE))
grouped$mean_bpm
gg <- ggplot(plot_df, aes(x = factor(Age, exclude = NA), y = season_bpm_zscore)) +
  geom_boxplot(aes(fill = grouped$mean_bpm)) +
  geom_text_repel(
    data = outliers,
    aes(label = paste0(player, " (", Age, ") - ", sprintf("%.2f", season_bpm_zscore))),
    color = "red",
    size = 3,
    max.overlaps = 20,
    segment.color = "transparent"
  ) +
  labs(
    title = "Season BPM Z-Score Distribution by Age",
    x = "Age",
    y = "Season BPM Z-Score"
  ) +
  scale_fill_gradientn(colors = paletteer_c("ggthemes::Blue", 30)) +
  theme_minimal() +
  theme(legend.position = "none")


# To display plot
gg

# To save plot (optional)
ggsave("./data/to_display/age_bpm_zscore_boxplot_r.png", gg, width = 17, height = 9, dpi = 300)



###########
# PLOT 4
###########
model_df <- read_csv("./data/model_df.csv")
none_df <- model_df |>
  filter(comeback_from_injury == "None")

# Compute medians for numeric columns
none_medians <- none_df |>
  select(where(is.numeric)) |>
  summarise(across(everything(), median, na.rm = TRUE))

# Subtract the medians from the entire DataFrame (numeric columns only)
model_df_centered <- model_df |>
  mutate(across(where(is.numeric), ~ . - none_medians[[cur_column()]]))

gg_injuries <- ggplot(model_df, aes(x = comeback_from_injury, y = difference)) +
  geom_boxplot(aes(fill = median(difference))) +
  theme_minimal(base_size = 16) +
  scale_fill_gradientn(colors = paletteer_c("ggthemes::Blue", 30)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.position = "none"
  ) +
  labs(
    title = "Difference in BPM Z-Score by Comeback from Injury (Centered around None median = 0)",
    x = "Comeback from Injury",
    y = "Difference in BPM Z-Score"
  ) +
  ylim(-3, 3)
gg_injuries
ggsave("./data/to_display/injury_boxplot_r.png", gg_injuries, width = 17, height = 9, dpi = 300, bg = "white")
