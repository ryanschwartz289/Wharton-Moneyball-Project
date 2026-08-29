library(hoopR)
library(dplyr)
library(purrr)

# Step 1: Get list of players for a given season
season <- 2025
players_raw <- nba_commonallplayers(season = season)

# Step 2: Extract the data frame from the list
players <- players_raw$CommonAllPlayers
players <- players |> filter(TO_YEAR == season)
view(players)
# Step 3: Clean up to get IDs and names
player_df <- players %>%
  select(player_id = PERSON_ID, player_name = DISPLAY_FIRST_LAST)
view(player_df)

# Step 4: Define a function to get a player's game log and attach their name
get_player_logs <- function(player_id, player_name) {
  message("Fetching logs for: ", player_name, " (ID: ", player_id, ")")

  tryCatch(
    {
      logs <- nba_playergamelog(
        player_id = player_id,
        season = season,
        season_type = "Regular Season"
      )
      logs_df <- logs$PlayerGameLog

      if (!is.null(logs_df) && nrow(logs_df) > 0) {
        message("  → Found ", nrow(logs_df), " rows for ", player_name)
        logs_df %>%
          mutate(
            player_name = player_name,
            player_id = player_id
          )
      } else {
        message("  → No data returned for ", player_name)
        NULL
      }
    },
    error = function(e) {
      message("  ⚠️ Error for ", player_name, ": ", e$message)
      NULL
    }
  )
}
get_player_logs <- function(player_id, player_name) {
  message("Fetching logs for: ", player_name, " (ID: ", player_id, ")")

  tryCatch(
    {
      logs <- nba_playergamelog(
        player_id = player_id,
        season = 2024,
        season_type = "Regular Season"
      )
      logs_df <- logs$PlayerGameLog

      if (!is.null(logs_df) && nrow(logs_df) > 0) {
        message("  → Found ", nrow(logs_df), " rows for ", player_name)
        logs_df %>%
          mutate(
            player_name = player_name,
            player_id = player_id
          )
      } else {
        message("  → No data returned for ", player_name)
        NULL
      }
    },
    error = function(e) {
      message("  ⚠️ Error for ", player_name, ": ", e$message)
      NULL
    }
  )
}


# Step 5: Loop over all players (limit for testing if needed)
player_logs_all <- player_df %>%
  head(10) %>% # limit for testing; remove for full list
  pmap_dfr(get_player_logs)

# Step 6: View results
View(player_logs_all)

# logs <- nba_playergamelogs(
#   season = "2024-25",
#   season_type = "Regular Season")
# logs
l <- nba_boxscoreadvancedv2(
  season = "2024-25",
  season_type = "Regular Season"
)
view(l$PlayerStats)
