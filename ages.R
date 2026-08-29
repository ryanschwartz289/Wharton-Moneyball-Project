library(rvest)
library(dplyr)
library(purrr)
library(readr)

tables <- list()
for (i in 2010:2024) {
  url <- sprintf("https://www.basketball-reference.com/leagues/NBA_%d_play-by-play.html", i)
  # Be polite! Sleep to avoid hammering the server
  Sys.sleep(0.1)
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (!is.null(page)) {
    table_node <- html_node(page, "table#pbp_stats")
    if (!is.null(table_node)) {
      table_df <- html_table(table_node, fill = TRUE)
      # Drop the first level of MultiIndex - if present
      if (inherits(table_df, "data.frame") && !is.null(colnames(table_df))) {
        names(table_df) <- make.names(names(table_df), unique = TRUE)
      }
      table_df$season <- i
      tables[[length(tables) + 1]] <- table_df
    }
  }
}

# Combine and clean
if (length(tables) > 0) {
  df <- bind_rows(tables)
  df <- df %>% select(Player, Age, season)
  df <- df %>% distinct(Player, season, .keep_all = TRUE)
  write_csv(df, "./data/player_ages.csv")
}