import numpy as np
import pandas as pd


class BPMCalculator:
    def __init__(self):
        # Constants for BPM calculation
        self.REPLACEMENT_LEVEL = -2.0

        # Position regression coefficients
        self.position_coeffs = {
            "intercept": 2.130,
            "trb_pct": 8.668,
            "stl_pct": -2.486,
            "pf_pct": 0.992,
            "ast_pct": -3.536,
            "blk_pct": 1.667,
        }

        # Offensive role regression coefficients
        self.role_coeffs = {
            "intercept": 6.00,
            "ast_pct": -6.642,
            "threshold_pts_pct": -8.544,
        }

        # BPM regression coefficients
        self.bpm_coeffs = {
            "pts": 0.860,
            "3pm": 0.389,
            "ast_pg": 0.580,  # Position 1.0 (PG)
            "ast_c": 1.034,  # Position 5.0 (C)
            "to": -0.964,
            "orb_pg": 0.613,  # Position 1.0 (PG)
            "orb_c": 0.181,  # Position 5.0 (C)
            "drb_pg": 0.116,  # Position 1.0 (PG)
            "drb_c": 0.181,  # Position 5.0 (C)
            "stl_pg": 1.369,  # Position 1.0 (PG)
            "stl_c": 1.008,  # Position 5.0 (C)
            "blk_pg": 1.327,  # Position 1.0 (PG)
            "blk_c": 0.703,  # Position 5.0 (C)
            "pf": -0.367,
            "fga_creator": -0.560,  # Offensive Role 1.0 (Creator)
            "fga_receiver": -0.780,  # Offensive Role 5.0 (Receiver)
            "fta_creator": -0.246,  # Offensive Role 1.0 (Creator)
            "fta_receiver": -0.343,  # Offensive Role 5.0 (Receiver)
        }

        # OBPM regression coefficients
        self.obpm_coeffs = {
            "pts": 0.605,
            "3pm": 0.477,
            "ast": 0.476,
            "to_pg": -0.579,  # Position 1.0 (PG)
            "to_c": -0.882,  # Position 5.0 (C)
            "orb_pg": 0.606,  # Position 1.0 (PG)
            "orb_c": 0.422,  # Position 5.0 (C)
            "drb_pg": -0.112,  # Position 1.0 (PG)
            "drb_c": 0.103,  # Position 5.0 (C)
            "stl_pg": 0.177,  # Position 1.0 (PG)
            "stl_c": 0.294,  # Position 5.0 (C)
            "blk_pg": 0.725,  # Position 1.0 (PG)
            "blk_c": 0.097,  # Position 5.0 (C)
            "pf": -0.439,
            "fga_creator": -0.330,  # Offensive Role 1.0 (Creator)
            "fga_receiver": -0.472,  # Offensive Role 5.0 (Receiver)
            "fta_creator": -0.145,  # Offensive Role 1.0 (Creator)
            "fta_receiver": -0.208,  # Offensive Role 5.0 (Receiver)
        }

        # Position adjustment constants
        self.pos_adj = {
            "pg": -0.818,  # Position 1.0 (PG)
            "sf": 0.00,  # Position 3.0 (SF)
            "c": 0.00,  # Position 5.0 (C)
        }

        # Offensive role adjustment constants
        self.role_adj = {
            "creator": -2.774,  # Role 1.0 (Creator)
            "neutral": 0.00,  # Role 3.0 (Neutral)
            "receiver": 2.774,  # Role 5.0 (Receiver)
        }

    def preprocess_data(self, box_scores):
        """
        Convert raw box score data to per-100-possession metrics for each game

        Parameters:
        box_scores (DataFrame): Raw box score data

        Returns:
        DataFrame: Processed box score data with per-100-possession stats
        """
        # Convert minutes to float if necessary
        if box_scores["minutes"].dtype == "object":
            box_scores["minutes"] = box_scores["minutes"].apply(
                lambda x: (
                    float(x.split(":")[0]) + float(x.split(":")[1]) / 60
                    if ":" in str(x)
                    else float(x)
                )
            )

        # Create unique player-season identifier
        box_scores["player_season_id"] = (
            box_scores["athlete_id"].astype(str)
            + "_"
            + box_scores["season"].astype(str)
        )

        # Create unique team-season identifier
        box_scores["team_season_id"] = (
            box_scores["team_id"].astype(str) + "_" + box_scores["season"].astype(str)
        )

        # Create unique game-team identifier
        box_scores["game_team_id"] = (
            box_scores["game_id"].astype(str) + "_" + box_scores["team_id"].astype(str)
        )

        # Calculate team possessions for each game
        # Possessions = FGA + 0.44*FTA - ORB + TOV
        box_scores["possessions"] = (
            box_scores["field_goals_attempted"]
            + 0.44 * box_scores["free_throws_attempted"]
            - box_scores["offensive_rebounds"]
            + box_scores["turnovers"]
        )

        # Group by game and team to get team possessions
        team_game_possessions = (
            box_scores.groupby(["game_id", "team_id"])["possessions"]
            .sum()
            .reset_index()
        )
        team_game_possessions = team_game_possessions.rename(
            columns={"possessions": "team_possessions"}
        )

        # Merge back to original dataframe
        box_scores = pd.merge(
            box_scores, team_game_possessions, on=["game_id", "team_id"]
        )

        # Calculate per-100-possession stats for each game
        per_100_cols = [
            "points",
            "field_goals_made",
            "field_goals_attempted",
            "three_point_field_goals_made",
            "three_point_field_goals_attempted",
            "free_throws_made",
            "free_throws_attempted",
            "offensive_rebounds",
            "defensive_rebounds",
            "rebounds",
            "assists",
            "steals",
            "blocks",
            "turnovers",
            "fouls",
        ]

        # Create a mapping for column name changes if needed
        col_mapping = {
            "points": "points"  # Add mappings if column names differ from expected
        }

        # Create per-100-possession columns
        for col in per_100_cols:
            source_col = col_mapping.get(col, col)
            if source_col in box_scores.columns:
                per_100_col = f"{col}_per_100"
                box_scores[per_100_col] = (
                    box_scores[source_col] * 100 / box_scores["team_possessions"]
                )

        return box_scores

    def estimate_positions_and_roles(self, box_scores):
        """
        Estimate player positions and offensive roles based on season-level box score data

        Parameters:
        box_scores (DataFrame): Processed box score data

        Returns:
        DataFrame: Box score data with position and role estimates
        """
        # Group by player-season to calculate season-level stats
        player_season_stats = (
            box_scores.groupby("player_season_id")
            .agg(
                {
                    "team_season_id": "first",
                    "minutes": "sum",
                    "rebounds": "sum",
                    "steals": "sum",
                    "fouls": "sum",
                    "assists": "sum",
                    "blocks": "sum",
                    "points": "sum",
                    "field_goals_attempted": "sum",
                    "field_goals_made": "sum",
                    "free_throws_attempted": "sum",
                    "athlete_position_abbreviation": "first",
                }
            )
            .reset_index()
        )

        # Group by team-season to calculate team totals
        team_season_totals = (
            box_scores.groupby("team_season_id")
            .agg(
                {
                    "minutes": "sum",
                    "rebounds": "sum",
                    "steals": "sum",
                    "fouls": "sum",
                    "assists": "sum",
                    "blocks": "sum",
                    "points": "sum",
                    "field_goals_attempted": "sum",
                    "field_goals_made": "sum",
                }
            )
            .reset_index()
        )

        # Merge with team totals
        player_season_stats = pd.merge(
            player_season_stats,
            team_season_totals,
            on="team_season_id",
            suffixes=("", "_team"),
        )

        # Calculate percentage stats
        player_season_stats["trb_pct"] = (
            player_season_stats["rebounds"] / player_season_stats["rebounds_team"]
        )
        player_season_stats["stl_pct"] = (
            player_season_stats["steals"] / player_season_stats["steals_team"]
        )
        player_season_stats["pf_pct"] = (
            player_season_stats["fouls"] / player_season_stats["fouls_team"]
        )
        player_season_stats["ast_pct"] = (
            player_season_stats["assists"] / player_season_stats["assists_team"]
        )
        player_season_stats["blk_pct"] = (
            player_season_stats["blocks"] / player_season_stats["blocks_team"]
        )

        # Calculate points per true shot attempt for threshold points
        player_season_stats["pts_per_tsa"] = player_season_stats["points"] / (
            player_season_stats["field_goals_attempted"]
            + 0.44 * player_season_stats["free_throws_attempted"]
        ).clip(
            lower=1
        )  # Prevent division by zero

        # Calculate team average pts_per_tsa
        team_pts_per_tsa = (
            player_season_stats.groupby("team_season_id")
            .apply(
                lambda x: (
                    x["points"].sum()
                    / (
                        x["field_goals_attempted"].sum()
                        + 0.44 * x["free_throws_attempted"].sum()
                    )
                )
            )
            .reset_index(name="team_pts_per_tsa")
        )

        # Merge team pts_per_tsa
        player_season_stats = pd.merge(
            player_season_stats, team_pts_per_tsa, on="team_season_id"
        )

        # Calculate threshold pts_per_tsa
        player_season_stats["threshold"] = (
            player_season_stats["team_pts_per_tsa"] - 0.33
        )

        # Calculate threshold points
        player_season_stats["threshold_pts"] = (
            player_season_stats["pts_per_tsa"] - player_season_stats["threshold"]
        ) * (
            player_season_stats["field_goals_attempted"]
            + 0.44 * player_season_stats["free_throws_attempted"]
        )

        # Threshold points can't be negative
        player_season_stats["threshold_pts"] = player_season_stats[
            "threshold_pts"
        ].clip(lower=0)

        # Calculate team threshold points
        team_threshold_pts = (
            player_season_stats.groupby("team_season_id")["threshold_pts"]
            .sum()
            .reset_index(name="team_threshold_pts")
        )

        # Merge team threshold points
        player_season_stats = pd.merge(
            player_season_stats, team_threshold_pts, on="team_season_id"
        )

        # Calculate threshold points percentage
        player_season_stats["threshold_pts_pct"] = player_season_stats[
            "threshold_pts"
        ] / player_season_stats["team_threshold_pts"].replace(
            0, 1
        )  # Prevent division by zero

        # Estimate position using regression formula
        player_season_stats["estimated_position"] = (
            self.position_coeffs["intercept"]
            + self.position_coeffs["trb_pct"] * player_season_stats["trb_pct"]
            + self.position_coeffs["stl_pct"] * player_season_stats["stl_pct"]
            + self.position_coeffs["pf_pct"] * player_season_stats["pf_pct"]
            + self.position_coeffs["ast_pct"] * player_season_stats["ast_pct"]
            + self.position_coeffs["blk_pct"] * player_season_stats["blk_pct"]
        )

        # For small sample sizes, combine with listed position
        # Convert position abbreviation to numerical position
        pos_dict = {"PG": 1, "SG": 2, "SF": 3, "PF": 4, "C": 5}
        player_season_stats["listed_position"] = player_season_stats[
            "athlete_position_abbreviation"
        ].map(
            lambda x: pos_dict.get(x, 3)  # Default to SF if unknown
        )

        # Weight by minutes played
        total_minutes = 50  # Minutes weight for listed position
        player_season_stats["position_weight"] = player_season_stats["minutes"] / (
            player_season_stats["minutes"] + total_minutes
        )
        player_season_stats["weighted_position"] = (
            player_season_stats["position_weight"]
            * player_season_stats["estimated_position"]
            + (1 - player_season_stats["position_weight"])
            * player_season_stats["listed_position"]
        )

        # Clamp position between 1.0 and 5.0
        player_season_stats["position"] = player_season_stats["weighted_position"].clip(
            1.0, 5.0
        )

        # Estimate offensive role using regression formula
        player_season_stats["estimated_role"] = (
            self.role_coeffs["intercept"]
            + self.role_coeffs["ast_pct"] * player_season_stats["ast_pct"]
            + self.role_coeffs["threshold_pts_pct"]
            * player_season_stats["threshold_pts_pct"]
        )

        # Weight with default role
        default_role = 4.0  # Default offensive role
        player_season_stats["role_weight"] = player_season_stats["minutes"] / (
            player_season_stats["minutes"] + total_minutes
        )
        player_season_stats["weighted_role"] = (
            player_season_stats["role_weight"] * player_season_stats["estimated_role"]
            + (1 - player_season_stats["role_weight"]) * default_role
        )

        # Clamp role between 1.0 and 5.0
        player_season_stats["offensive_role"] = player_season_stats[
            "weighted_role"
        ].clip(1.0, 5.0)

        # Merge position and role back to original box scores
        position_role_df = player_season_stats[
            ["player_season_id", "position", "offensive_role"]
        ]
        box_scores = pd.merge(box_scores, position_role_df, on="player_season_id")

        return box_scores

    def calculate_raw_bpm(self, box_scores):
        """
        Calculate raw BPM and OBPM values for each game

        Parameters:
        box_scores (DataFrame): Box score data with position and role estimates

        Returns:
        DataFrame: Box score data with raw BPM and OBPM values
        """
        # Calculate interpolated coefficients based on position
        box_scores["ast_coeff"] = self.interpolate_by_position(
            box_scores["position"], self.bpm_coeffs["ast_pg"], self.bpm_coeffs["ast_c"]
        )
        box_scores["orb_coeff"] = self.interpolate_by_position(
            box_scores["position"], self.bpm_coeffs["orb_pg"], self.bpm_coeffs["orb_c"]
        )
        box_scores["drb_coeff"] = self.interpolate_by_position(
            box_scores["position"], self.bpm_coeffs["drb_pg"], self.bpm_coeffs["drb_c"]
        )
        box_scores["stl_coeff"] = self.interpolate_by_position(
            box_scores["position"], self.bpm_coeffs["stl_pg"], self.bpm_coeffs["stl_c"]
        )
        box_scores["blk_coeff"] = self.interpolate_by_position(
            box_scores["position"], self.bpm_coeffs["blk_pg"], self.bpm_coeffs["blk_c"]
        )

        # Calculate interpolated coefficients based on offensive role
        box_scores["fga_coeff"] = self.interpolate_by_role(
            box_scores["offensive_role"],
            self.bpm_coeffs["fga_creator"],
            self.bpm_coeffs["fga_receiver"],
        )
        box_scores["fta_coeff"] = self.interpolate_by_role(
            box_scores["offensive_role"],
            self.bpm_coeffs["fta_creator"],
            self.bpm_coeffs["fta_receiver"],
        )

        # Calculate team shooting efficiency by game
        game_team_shooting = (
            box_scores.groupby(["game_id", "team_id"])
            .agg(
                {
                    "points": "sum",
                    "field_goals_attempted": "sum",
                    "free_throws_attempted": "sum",
                }
            )
            .reset_index()
        )

        game_team_shooting["pts_per_tsa"] = game_team_shooting["points"] / (
            game_team_shooting["field_goals_attempted"]
            + 0.44 * game_team_shooting["free_throws_attempted"]
        ).clip(
            lower=1
        )  # Prevent division by zero

        # Merge back to box_scores
        box_scores = pd.merge(
            box_scores,
            game_team_shooting[["game_id", "team_id", "pts_per_tsa"]],
            on=["game_id", "team_id"],
            suffixes=("", "_game_team"),
        )

        # For simplicity, we'll use unadjusted points
        # In a more complex implementation, you could adjust points for team shooting context
        box_scores["adjusted_points_per_100"] = box_scores["points_per_100"]

        # Calculate raw BPM components for each game
        box_scores["raw_bpm"] = (
            self.bpm_coeffs["pts"] * box_scores["adjusted_points_per_100"]
            + self.bpm_coeffs["3pm"]
            * box_scores["three_point_field_goals_made_per_100"]
            + box_scores["ast_coeff"] * box_scores["assists_per_100"]
            + self.bpm_coeffs["to"] * box_scores["turnovers_per_100"]
            + box_scores["orb_coeff"] * box_scores["offensive_rebounds_per_100"]
            + box_scores["drb_coeff"] * box_scores["defensive_rebounds_per_100"]
            + box_scores["stl_coeff"] * box_scores["steals_per_100"]
            + box_scores["blk_coeff"] * box_scores["blocks_per_100"]
            + self.bpm_coeffs["pf"] * box_scores["fouls_per_100"]
            + box_scores["fga_coeff"] * box_scores["field_goals_attempted_per_100"]
            + box_scores["fta_coeff"] * box_scores["free_throws_attempted_per_100"]
        )

        # Calculate position adjustment constant
        box_scores["pos_adj_const"] = self.calculate_position_adjustment(
            box_scores["position"]
        )

        # Calculate offensive role adjustment constant
        box_scores["role_adj_const"] = self.calculate_role_adjustment(
            box_scores["offensive_role"]
        )

        # Apply adjustment constants to raw BPM
        box_scores["raw_bpm_adj"] = (
            box_scores["raw_bpm"]
            + box_scores["pos_adj_const"]
            + box_scores["role_adj_const"]
        )

        # Calculate OBPM coefficients
        box_scores["to_coeff_o"] = self.interpolate_by_position(
            box_scores["position"], self.obpm_coeffs["to_pg"], self.obpm_coeffs["to_c"]
        )
        box_scores["orb_coeff_o"] = self.interpolate_by_position(
            box_scores["position"],
            self.obpm_coeffs["orb_pg"],
            self.obpm_coeffs["orb_c"],
        )
        box_scores["drb_coeff_o"] = self.interpolate_by_position(
            box_scores["position"],
            self.obpm_coeffs["drb_pg"],
            self.obpm_coeffs["drb_c"],
        )
        box_scores["stl_coeff_o"] = self.interpolate_by_position(
            box_scores["position"],
            self.obpm_coeffs["stl_pg"],
            self.obpm_coeffs["stl_c"],
        )
        box_scores["blk_coeff_o"] = self.interpolate_by_position(
            box_scores["position"],
            self.obpm_coeffs["blk_pg"],
            self.obpm_coeffs["blk_c"],
        )

        # Calculate offensive role coefficients for OBPM
        box_scores["fga_coeff_o"] = self.interpolate_by_role(
            box_scores["offensive_role"],
            self.obpm_coeffs["fga_creator"],
            self.obpm_coeffs["fga_receiver"],
        )
        box_scores["fta_coeff_o"] = self.interpolate_by_role(
            box_scores["offensive_role"],
            self.obpm_coeffs["fta_creator"],
            self.obpm_coeffs["fta_receiver"],
        )

        # Calculate raw OBPM components for each game
        box_scores["raw_obpm"] = (
            self.obpm_coeffs["pts"] * box_scores["adjusted_points_per_100"]
            + self.obpm_coeffs["3pm"]
            * box_scores["three_point_field_goals_made_per_100"]
            + self.obpm_coeffs["ast"] * box_scores["assists_per_100"]
            + box_scores["to_coeff_o"] * box_scores["turnovers_per_100"]
            + box_scores["orb_coeff_o"] * box_scores["offensive_rebounds_per_100"]
            + box_scores["drb_coeff_o"] * box_scores["defensive_rebounds_per_100"]
            + box_scores["stl_coeff_o"] * box_scores["steals_per_100"]
            + box_scores["blk_coeff_o"] * box_scores["blocks_per_100"]
            + self.obpm_coeffs["pf"] * box_scores["fouls_per_100"]
            + box_scores["fga_coeff_o"] * box_scores["field_goals_attempted_per_100"]
            + box_scores["fta_coeff_o"] * box_scores["free_throws_attempted_per_100"]
        )

        # Position adjustment constant for OBPM
        box_scores["pos_adj_const_o"] = self.calculate_position_adjustment_offense(
            box_scores["position"]
        )

        # Offensive role adjustment constant for OBPM
        box_scores["role_adj_const_o"] = self.calculate_role_adjustment_offense(
            box_scores["offensive_role"]
        )

        # Apply adjustment constants to raw OBPM
        box_scores["raw_obpm_adj"] = (
            box_scores["raw_obpm"]
            + box_scores["pos_adj_const_o"]
            + box_scores["role_adj_const_o"]
        )

        return box_scores

    def interpolate_by_position(self, position, pg_value, c_value):
        """
        Interpolate coefficient based on position (1.0 to 5.0)
        """
        return pg_value + (c_value - pg_value) * (position - 1) / 4

    def interpolate_by_role(self, role, creator_value, receiver_value):
        """
        Interpolate coefficient based on offensive role (1.0 to 5.0)
        """
        return creator_value + (receiver_value - creator_value) * (role - 1) / 4

    def calculate_position_adjustment(self, position):
        """
        Calculate position adjustment constant
        """
        # Only applies penalty for positions less than 3.0
        return np.where(position < 3, (position - 3) * (self.pos_adj["pg"] / 2), 0)

    def calculate_role_adjustment(self, role):
        """
        Calculate offensive role adjustment constant
        """
        return (role - 3) * (self.role_adj["receiver"] / 2)

    def calculate_position_adjustment_offense(self, position):
        """
        Calculate position adjustment constant for OBPM
        """
        # Position adjustment constant for OBPM (-1.698 for PG, 0 for SF and C)
        pg_adj = -1.698
        return np.where(position < 3, (position - 3) * (pg_adj / 2), 0)

    def calculate_role_adjustment_offense(self, role):
        """
        Calculate offensive role adjustment constant for OBPM
        """
        # Role adjustment constant for OBPM (-0.860 for Creator, 0 for Neutral, 0.860 for Receiver)
        role_adj = 0.860
        return (role - 3) * role_adj / 2

    def safe_weighted_average(self, values, weights):
        """
        Calculate weighted average that handles zero-sum weights
        """
        weights_sum = weights.sum()
        if weights_sum == 0:
            # If weights sum to zero, return average or nan
            if len(values) > 0:
                return values.mean()
            return np.nan
        return np.average(values, weights=weights)

    def apply_team_adjustment(self, box_scores):
        """
        Apply team adjustment to raw BPM and OBPM values for each game

        Parameters:
        box_scores (DataFrame): Box score data with raw BPM values

        Returns:
        DataFrame: Box score data with final BPM values
        """
        # Only include players with minutes > 0 for team calculations
        active_players = box_scores[box_scores["minutes"] > 0]

        # Define safe weighted average for aggregation
        def safe_weighted_avg(x):
            minutes = active_players.loc[x.index, "minutes"]
            return self.safe_weighted_average(x, minutes)

        # Group by game_id and team_id to get game-team level data
        game_team_stats = (
            active_players.groupby(["game_id", "team_id"])
            .agg(
                {
                    "minutes": "sum",
                    "raw_bpm_adj": safe_weighted_avg,
                    "raw_obpm_adj": safe_weighted_avg,
                    "team_score": "first",
                    "opponent_team_score": "first",
                }
            )
            .reset_index()
        )

        # Calculate team efficiency differential for each game
        game_team_stats["efficiency_diff"] = (
            game_team_stats["team_score"] - game_team_stats["opponent_team_score"]
        )

        # Estimate average lead (simplified estimate)
        game_team_stats["avg_lead"] = game_team_stats["efficiency_diff"] / 2

        # Adjust for playing with lead/behind
        game_team_stats["lead_adjustment"] = -0.35 / 2 * game_team_stats["avg_lead"]

        # Calculate adjusted team rating
        game_team_stats["adjusted_team_rating"] = (
            game_team_stats["efficiency_diff"] / 2 + game_team_stats["lead_adjustment"]
        )

        # Calculate team adjustment
        game_team_stats["team_adjustment"] = (
            game_team_stats["adjusted_team_rating"] - game_team_stats["raw_bpm_adj"]
        )

        # Merge team adjustment back to box_scores
        box_scores = pd.merge(
            box_scores,
            game_team_stats[["game_id", "team_id", "team_adjustment"]],
            on=["game_id", "team_id"],
        )

        # Apply team adjustment to get final BPM and OBPM
        box_scores["BPM"] = box_scores["raw_bpm_adj"] + box_scores["team_adjustment"]
        box_scores["OBPM"] = box_scores["raw_obpm_adj"] + box_scores["team_adjustment"]

        # Calculate DBPM as the difference between BPM and OBPM
        box_scores["DBPM"] = box_scores["BPM"] - box_scores["OBPM"]

        return box_scores

    def calculate_game_vorp(self, box_scores):
        """
        Calculate game-level Value Over Replacement Player (VORP)

        Parameters:
        box_scores (DataFrame): Box score data with BPM values

        Returns:
        DataFrame: Box score data with game VORP values
        """
        # Calculate percentage of team possessions used by player in each game
        team_game_possessions = (
            box_scores.groupby(["game_id", "team_id"])["minutes"].sum().reset_index()
        )
        team_game_possessions = team_game_possessions.rename(
            columns={"minutes": "team_minutes"}
        )

        # Merge back to get percentage of team minutes
        box_scores = pd.merge(
            box_scores, team_game_possessions, on=["game_id", "team_id"]
        )

        # Calculate percentage of minutes played in the game
        box_scores["pct_minutes"] = box_scores["minutes"] / box_scores["team_minutes"]

        # Calculate VORP for the game (scaled by minutes played)
        box_scores["Game_VORP"] = (
            (box_scores["BPM"] - self.REPLACEMENT_LEVEL)
            * box_scores["pct_minutes"]
            / 82
        )

        # Calculate game wins over replacement
        box_scores["Game_WOR"] = box_scores["Game_VORP"] * 2.7

        return box_scores

    def calculate_bpm(self, box_scores):
        """
        Main function to calculate game-level BPM metrics

        Parameters:
        box_scores (DataFrame): Raw box score data

        Returns:
        DataFrame: Box score data with game-level BPM metrics
        """
        # Preprocess data
        processed_data = self.preprocess_data(box_scores)

        # Estimate positions and roles at season level
        positioned_data = self.estimate_positions_and_roles(processed_data)

        # Calculate raw BPM for each game
        bpm_data = self.calculate_raw_bpm(positioned_data)

        # Apply team adjustment for each game
        adjusted_data = self.apply_team_adjustment(bpm_data)

        # Calculate game-level VORP
        vorp_data = self.calculate_game_vorp(adjusted_data)

        # Select and return the relevant columns
        result_columns = [
            "game_id",
            "season",
            "athlete_id",
            "athlete_display_name",
            "team_id",
            "team_name",
            "position",
            "offensive_role",
            "minutes",
            "BPM",
            "OBPM",
            "DBPM",
            "Game_VORP",
            "Game_WOR",
            "points",
            "assists",
            "rebounds",
            "steals",
            "blocks",
            "turnovers",
        ]

        # Only include columns that exist in the dataframe
        available_columns = [col for col in result_columns if col in vorp_data.columns]

        return vorp_data[available_columns]
