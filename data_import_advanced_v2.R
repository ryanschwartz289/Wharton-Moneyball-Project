library(tidyverse)
library(hoopR)

season <- "2024-25"
valid_season_types <- c(
  "",
  "NBA Mexico City Game",
  "Emirates NBA Cup",
  "NBA Paris Games"
)

# Step 1: Get all game IDs for the season
schedule <- nba_schedule(season = as.numeric(substr(season, 1, 4))) # e.g., 2024
view(schedule)
unique(schedule$game_label)
game_ids <- schedule %>%
  filter(game_label %in% valid_season_types) %>%
  pull(game_id)
game_ids
view(filter(schedule, game_id %in% game_ids))
# Step 2: Define a function to get advanced box score for a single game
get_advanced_boxscore <- function(game_id) {
  message("Fetching advanced boxscore for game ID: ", game_id)

  tryCatch(
    {
      boxscore <- nba_boxscoreadvancedv2(game_id = game_id)

      if (!is.null(boxscore$PlayerStats) && nrow(boxscore$PlayerStats) > 0) {
        boxscore$PlayerStats %>%
          mutate(game_id = game_id)
      } else {
        message("  → No player stats data for game ", game_id)
        NULL
      }
    },
    error = function(e) {
      message("  ⚠️ Error for game ", game_id, ": ", e$message)
      NULL
    }
  )
}
# }

df <- get_advanced_boxscore(0022400037)
view(df)
# # Step 3: Loop over game IDs and collect data
# advanced_stats_all <- map_dfr(game_ids, get_advanced_boxscore)

# # Step 4: View combined data
# View(advanced_stats_all)

# write_csv(advanced_stats_all, "./data/advanced_stats_all.csv")
