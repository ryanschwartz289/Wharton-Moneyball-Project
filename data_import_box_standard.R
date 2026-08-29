library(tidyverse)
library(hoopR)
df <- load_nba_player_box(seasons = 2002:2025)
view(df)
summary(df$season)

write_csv(df, "../data/player_box_standard.csv")
