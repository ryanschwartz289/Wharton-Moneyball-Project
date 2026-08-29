library(dplyr)

BPMCalculator <- R6::R6Class(
  "BPMCalculator",
  public = list(
    REPLACEMENT_LEVEL = -2.0,

    position_coeffs = list(
      intercept = 2.130,
      trb_pct = 8.668,
      stl_pct = -2.486,
      pf_pct = 0.992,
      ast_pct = -3.536,
      blk_pct = 1.667
    ),
    role_coeffs = list(
      intercept = 6.00,
      ast_pct = -6.642,
      threshold_pts_pct = -8.544
    ),
    bpm_coeffs = list(
      pts = 0.860,
      `3pm` = 0.389,
      ast_pg = 0.580,
      ast_c = 1.034,
      to = -0.964,
      orb_pg = 0.613,
      orb_c = 0.181,
      drb_pg = 0.116,
      drb_c = 0.181,
      stl_pg = 1.369,
      stl_c = 1.008,
      blk_pg = 1.327,
      blk_c = 0.703,
      pf = -0.367,
      fga_creator = -0.560,
      fga_receiver = -0.780,
      fta_creator = -0.246,
      fta_receiver = -0.343
    ),
    obpm_coeffs = list(
      pts = 0.605,
      `3pm` = 0.477,
      ast = 0.476,
      to_pg = -0.579,
      to_c = -0.882,
      orb_pg = 0.606,
      orb_c = 0.422,
      drb_pg = -0.112,
      drb_c = 0.103,
      stl_pg = 0.177,
      stl_c = 0.294,
      blk_pg = 0.725,
      blk_c = 0.097,
      pf = -0.439,
      fga_creator = -0.330,
      fga_receiver = -0.472,
      fta_creator = -0.145,
      fta_receiver = -0.208
    ),
    pos_adj = list(
      pg = -0.818,
      sf = 0.00,
      c = 0.00
    ),
    role_adj = list(
      creator = -2.774,
      neutral = 0.00,
      receiver = 2.774
    ),

    preprocess_data = function(box_scores) {
      # convert minutes
      if (is.character(box_scores$minutes)) {
        box_scores$minutes <- sapply(box_scores$minutes, function(x) {
          if (grepl(":", x)) {
            parts <- strsplit(x, ":")[[1]]
            as.numeric(parts[1]) + as.numeric(parts[2]) / 60
          } else {
            as.numeric(x)
          }
        })
      }

      # IDs
      box_scores$player_season_id <- paste0(box_scores$athlete_id, "_", box_scores$season)
      box_scores$team_season_id <- paste0(box_scores$team_id, "_", box_scores$season)
      box_scores$game_team_id <- paste0(box_scores$game_id, "_", box_scores$team_id)

      # possessions
      box_scores$possessions <- box_scores$field_goals_attempted +
        0.44 * box_scores$free_throws_attempted -
        box_scores$offensive_rebounds +
        box_scores$turnovers

      team_game_possessions <- box_scores %>%
        group_by(game_id, team_id) %>%
        summarise(team_possessions = sum(possessions), .groups = "drop")

      box_scores <- left_join(box_scores, team_game_possessions, by = c("game_id", "team_id"))

      per_100_cols <- c(
        "points", "field_goals_made", "field_goals_attempted",
        "three_point_field_goals_made", "three_point_field_goals_attempted",
        "free_throws_made", "free_throws_attempted", "offensive_rebounds",
        "defensive_rebounds", "rebounds", "assists", "steals", "blocks",
        "turnovers", "fouls"
      )

      for (col in per_100_cols) {
        if (col %in% names(box_scores)) {
          box_scores[[paste0(col, "_per_100")]] <-
            box_scores[[col]] * 100 / box_scores$team_possessions
        }
      }

      box_scores
    },

    estimate_positions_and_roles = function(box_scores) {
      # Player season stats
      player_season_stats <- box_scores %>%
        group_by(player_season_id) %>%
        summarise(
          team_season_id = first(team_season_id),
          minutes = sum(minutes),
          rebounds = sum(rebounds),
          steals = sum(steals),
          fouls = sum(fouls),
          assists = sum(assists),
          blocks = sum(blocks),
          points = sum(points),
          field_goals_attempted = sum(field_goals_attempted),
          field_goals_made = sum(field_goals_made),
          free_throws_attempted = sum(free_throws_attempted),
          athlete_position_abbreviation = first(athlete_position_abbreviation),
          .groups = "drop"
        )

      team_season_totals <- box_scores %>%
        group_by(team_season_id) %>%
        summarise(
          minutes_team = sum(minutes),
          rebounds_team = sum(rebounds),
          steals_team = sum(steals),
          fouls_team = sum(fouls),
          assists_team = sum(assists),
          blocks_team = sum(blocks),
          points_team = sum(points),
          field_goals_attempted_team = sum(field_goals_attempted),
          field_goals_made_team = sum(field_goals_made),
          .groups = "drop"
        )

      player_season_stats <- left_join(player_season_stats, team_season_totals, by = "team_season_id")

      # pct stats
      player_season_stats <- player_season_stats %>%
        mutate(
          trb_pct = rebounds / rebounds_team,
          stl_pct = steals / steals_team,
          pf_pct = fouls / fouls_team,
          ast_pct = assists / assists_team,
          blk_pct = blocks / blocks_team
        )

      player_season_stats <- player_season_stats %>%
        mutate(
          pts_per_tsa = points / pmax(field_goals_attempted + 0.44 * free_throws_attempted, 1)
        )

      team_pts_per_tsa <- player_season_stats %>%
        group_by(team_season_id) %>%
        summarise(team_pts_per_tsa = sum(points) / sum(field_goals_attempted + 0.44 * free_throws_attempted), .groups = "drop")

      player_season_stats <- left_join(player_season_stats, team_pts_per_tsa, by = "team_season_id")

      player_season_stats <- player_season_stats %>%
        mutate(
          threshold = team_pts_per_tsa - 0.33,
          threshold_pts = (pts_per_tsa - threshold) * (field_goals_attempted + 0.44 * free_throws_attempted)
        ) %>%
        mutate(threshold_pts = pmax(threshold_pts, 0))

      team_threshold_pts <- player_season_stats %>%
        group_by(team_season_id) %>%
        summarise(team_threshold_pts = sum(threshold_pts), .groups = "drop")

      player_season_stats <- left_join(player_season_stats, team_threshold_pts, by = "team_season_id")

      player_season_stats <- player_season_stats %>%
        mutate(threshold_pts_pct = threshold_pts / ifelse(team_threshold_pts == 0, 1, team_threshold_pts))

      # Estimate position (regression)
      pc <- self$position_coeffs
      player_season_stats <- player_season_stats %>%
        mutate(
          estimated_position =
            pc$intercept +
            pc$trb_pct * trb_pct +
            pc$stl_pct * stl_pct +
            pc$pf_pct * pf_pct +
            pc$ast_pct * ast_pct +
            pc$blk_pct * blk_pct
        )

      # listed position
      pos_dict <- c(PG = 1, SG = 2, SF = 3, PF = 4, C = 5)
      player_season_stats <- player_season_stats %>%
        mutate(listed_position = unname(pos_dict[athlete_position_abbreviation]),
               listed_position = ifelse(is.na(listed_position), 3, listed_position))

      total_minutes <- 50
      player_season_stats <- player_season_stats %>%
        mutate(
          position_weight = minutes / (minutes + total_minutes),
          weighted_position = position_weight * estimated_position + (1 - position_weight) * listed_position,
          position = pmin(pmax(weighted_position, 1.0), 5.0)
        )

      # Estimate offensive role
      rc <- self$role_coeffs
      player_season_stats <- player_season_stats %>%
        mutate(
          estimated_role =
            rc$intercept +
            rc$ast_pct * ast_pct +
            rc$threshold_pts_pct * threshold_pts_pct
        )

      default_role <- 4.0
      player_season_stats <- player_season_stats %>%
        mutate(
          role_weight = minutes / (minutes + total_minutes),
          weighted_role = role_weight * estimated_role + (1 - role_weight) * default_role,
          offensive_role = pmin(pmax(weighted_role, 1.0), 5.0)
        )

      position_role_df <- player_season_stats %>%
        select(player_season_id, position, offensive_role)
      box_scores <- left_join(box_scores, position_role_df, by = "player_season_id")

      box_scores
    },

    interpolate_by_position = function(position, pg_value, c_value) {
      pg_value + (c_value - pg_value) * (position - 1) / 4
    },

    interpolate_by_role = function(role, creator_value, receiver_value) {
      creator_value + (receiver_value - creator_value) * (role - 1) / 4
    },

    calculate_position_adjustment = function(position) {
      pos_adj_pg <- self$pos_adj$pg
      ifelse(position < 3, (position - 3) * (pos_adj_pg / 2), 0)
    },

    calculate_role_adjustment = function(role) {
      role_adj_receiver <- self$role_adj$receiver
      (role - 3) * (role_adj_receiver / 2)
    },

    calculate_position_adjustment_offense = function(position) {
      pg_adj <- -1.698
      ifelse(position < 3, (position - 3) * (pg_adj / 2), 0)
    },

    calculate_role_adjustment_offense = function(role) {
      role_adj <- 0.860
      (role - 3) * role_adj / 2
    },

    safe_weighted_average = function(values, weights) {
      weights_sum <- sum(weights)
      if (weights_sum == 0) {
        if (length(values) > 0) {
          mean(values, na.rm = TRUE)
        } else {
          NA_real_
        }
      } else {
        weighted.mean(values, weights, na.rm = TRUE)
      }
    },

    calculate_raw_bpm = function(box_scores) {
      # Interpolated coefficients
      box_scores$ast_coeff <- self$interpolate_by_position(box_scores$position, self$bpm_coeffs$ast_pg, self$bpm_coeffs$ast_c)
      box_scores$orb_coeff <- self$interpolate_by_position(box_scores$position, self$bpm_coeffs$orb_pg, self$bpm_coeffs$orb_c)
      box_scores$drb_coeff <- self$interpolate_by_position(box_scores$position, self$bpm_coeffs$drb_pg, self$bpm_coeffs$drb_c)
      box_scores$stl_coeff <- self$interpolate_by_position(box_scores$position, self$bpm_coeffs$stl_pg, self$bpm_coeffs$stl_c)
      box_scores$blk_coeff <- self$interpolate_by_position(box_scores$position, self$bpm_coeffs$blk_pg, self$bpm_coeffs$blk_c)

      box_scores$fga_coeff <- self$interpolate_by_role(box_scores$offensive_role, self$bpm_coeffs$fga_creator, self$bpm_coeffs$fga_receiver)
      box_scores$fta_coeff <- self$interpolate_by_role(box_scores$offensive_role, self$bpm_coeffs$fta_creator, self$bpm_coeffs$fta_receiver)

      # Team shooting efficiency
      game_team_shooting <- box_scores %>%
        group_by(game_id, team_id) %>%
        summarise(
          points = sum(points),
          field_goals_attempted = sum(field_goals_attempted),
          free_throws_attempted = sum(free_throws_attempted),
          .groups = "drop"
        ) %>%
        mutate(pts_per_tsa = points / pmax(field_goals_attempted + 0.44 * free_throws_attempted, 1))

      box_scores <- left_join(
        box_scores,
        game_team_shooting %>% select(game_id, team_id, pts_per_tsa),
        by = c("game_id", "team_id")
      )

      box_scores$adjusted_points_per_100 <- box_scores$points_per_100

      # Raw BPM calculation
      bc <- self$bpm_coeffs
      box_scores$raw_bpm <- bc$pts * box_scores$adjusted_points_per_100 +
        bc$`3pm` * box_scores$three_point_field_goals_made_per_100 +
        box_scores$ast_coeff * box_scores$assists_per_100 +
        bc$to * box_scores$turnovers_per_100 +
        box_scores$orb_coeff * box_scores$offensive_rebounds_per_100 +
        box_scores$drb_coeff * box_scores$defensive_rebounds_per_100 +
        box_scores$stl_coeff * box_scores$steals_per_100 +
        box_scores$blk_coeff * box_scores$blocks_per_100 +
        bc$pf * box_scores$fouls_per_100 +
        box_scores$fga_coeff * box_scores$field_goals_attempted_per_100 +
        box_scores$fta_coeff * box_scores$free_throws_attempted_per_100

      box_scores$pos_adj_const <- self$calculate_position_adjustment(box_scores$position)
      box_scores$role_adj_const <- self$calculate_role_adjustment(box_scores$offensive_role)

      box_scores$raw_bpm_adj <- box_scores$raw_bpm +
        box_scores$pos_adj_const +
        box_scores$role_adj_const

      # OBPM coefficients
      oc <- self$obpm_coeffs
      box_scores$to_coeff_o <- self$interpolate_by_position(box_scores$position, oc$to_pg, oc$to_c)
      box_scores$orb_coeff_o <- self$interpolate_by_position(box_scores$position, oc$orb_pg, oc$orb_c)
      box_scores$drb_coeff_o <- self$interpolate_by_position(box_scores$position, oc$drb_pg, oc$drb_c)
      box_scores$stl_coeff_o <- self$interpolate_by_position(box_scores$position, oc$stl_pg, oc$stl_c)
      box_scores$blk_coeff_o <- self$interpolate_by_position(box_scores$position, oc$blk_pg, oc$blk_c)

      box_scores$fga_coeff_o <- self$interpolate_by_role(box_scores$offensive_role, oc$fga_creator, oc$fga_receiver)
      box_scores$fta_coeff_o <- self$interpolate_by_role(box_scores$offensive_role, oc$fta_creator, oc$fta_receiver)

      box_scores$raw_obpm <- oc$pts * box_scores$adjusted_points_per_100 +
        oc$`3pm` * box_scores$three_point_field_goals_made_per_100 +
        oc$ast * box_scores$assists_per_100 +
        box_scores$to_coeff_o * box_scores$turnovers_per_100 +
        box_scores$orb_coeff_o * box_scores$offensive_rebounds_per_100 +
        box_scores$drb_coeff_o * box_scores$defensive_rebounds_per_100 +
        box_scores$stl_coeff_o * box_scores$steals_per_100 +
        box_scores$blk_coeff_o * box_scores$blocks_per_100 +
        oc$pf * box_scores$fouls_per_100 +
        box_scores$fga_coeff_o * box_scores$field_goals_attempted_per_100 +
        box_scores$fta_coeff_o * box_scores$free_throws_attempted_per_100

      box_scores$pos_adj_const_o <- self$calculate_position_adjustment_offense(box_scores$position)
      box_scores$role_adj_const_o <- self$calculate_role_adjustment_offense(box_scores$offensive_role)

      box_scores$raw_obpm_adj <- box_scores$raw_obpm +
        box_scores$pos_adj_const_o +
        box_scores$role_adj_const_o

      box_scores
    },

    apply_team_adjustment = function(box_scores) {
      active_players <- box_scores %>% filter(minutes > 0)

      safe_weighted_avg <- function(x, idx) {
        minutes <- active_players$minutes[idx]
        self$safe_weighted_average(x, minutes)
      }

      # group_by game_id and team_id
      game_team_stats <- active_players %>%
        group_by(game_id, team_id) %>%
        summarise(
          minutes = sum(minutes),
          raw_bpm_adj = self$safe_weighted_average(raw_bpm_adj, minutes),
          raw_obpm_adj = self$safe_weighted_average(raw_obpm_adj, minutes),
          team_score = first(team_score),
          opponent_team_score = first(opponent_team_score),
          .groups = "drop"
        )

      game_team_stats <- game_team_stats %>%
        mutate(
          efficiency_diff = team_score - opponent_team_score,
          avg_lead = efficiency_diff / 2,
          lead_adjustment = -0.35 / 2 * avg_lead,
          adjusted_team_rating = efficiency_diff / 2 + lead_adjustment,
          team_adjustment = adjusted_team_rating - raw_bpm_adj
        )

      box_scores <- left_join(
        box_scores,
        game_team_stats %>% select(game_id, team_id, team_adjustment),
        by = c("game_id", "team_id")
      )

      box_scores$BPM <- box_scores$raw_bpm_adj + box_scores$team_adjustment
      box_scores$OBPM <- box_scores$raw_obpm_adj + box_scores$team_adjustment
      box_scores$DBPM <- box_scores$BPM - box_scores$OBPM

      box_scores
    },

    calculate_game_vorp = function(box_scores) {
      team_game_possessions <- box_scores %>%
        group_by(game_id, team_id) %>%
        summarise(team_minutes = sum(minutes), .groups = "drop")

      box_scores <- left_join(box_scores, team_game_possessions, by = c("game_id", "team_id"))
      box_scores$pct_minutes <- box_scores$minutes / box_scores$team_minutes

      box_scores$Game_VORP <- (box_scores$BPM - self$REPLACEMENT_LEVEL) * box_scores$pct_minutes / 82
      box_scores$Game_WOR <- box_scores$Game_VORP * 2.7

      box_scores
    },

    calculate_bpm = function(box_scores) {
      processed_data <- self$preprocess_data(box_scores)
      positioned_data <- self$estimate_positions_and_roles(processed_data)
      bpm_data <- self$calculate_raw_bpm(positioned_data)
      adjusted_data <- self$apply_team_adjustment(bpm_data)
      vorp_data <- self$calculate_game_vorp(adjusted_data)

      result_columns <- c(
        "game_id", "season", "athlete_id", "athlete_display_name",
        "team_id", "team_name", "position", "offensive_role",
        "minutes", "BPM", "OBPM", "DBPM", "Game_VORP", "Game_WOR",
        "points", "assists", "rebounds", "steals", "blocks", "turnovers"
      )

      available_columns <- intersect(result_columns, names(vorp_data))
      vorp_data[, available_columns, drop = FALSE]
    }
  )
)