library(hoopR)
library(tidyverse)

players <- read_csv("./data/player_box_standard.csv") %>%
  select(athlete_id) %>%
  distinct()
players
n <- 0
# Get common player info for each player_id
player_data_list <- lapply(players$athlete_id, function(pid) {
  n <- n + 1
  print(n)
  nba_commonplayerinfo(league_id = "00", athlete_id = pid)$CommonPlayerInfo
})
print("1")
# Combine all data frames into one
player_data <- bind_rows(player_data_list)
print("2")
write_csv(player_data, "./data/player_data.csv")
nba_commonplayerinfo(league_id = "00", player_id = "2544")
