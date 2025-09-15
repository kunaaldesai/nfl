# ===== Weekly QB aggregates (REG) =====
qb_weekly_25 <- pbp_25 %>%
  filter(season_type == "REG") %>%
  group_by(week, id, name, posteam, defteam) %>%
  summarize(
    epa_play = mean(qb_epa, na.rm = TRUE),
    cpoe = mean(cpoe, na.rm = TRUE),
    pass_epa = mean(epa[play_type == "pass"], na.rm = TRUE),
    rush_epa = mean(epa[play_type == "run"], na.rm = TRUE),
    total_pass_yards = sum(passing_yards, na.rm = TRUE),
    dropbacks = sum(qb_dropback, na.rm = TRUE),
    pass_attempts = sum(pass_attempt, na.rm = TRUE),
    pass_completions = sum(complete_pass, na.rm = TRUE),
    shotgun_pct = mean(shotgun, na.rm = TRUE),
    no_huddle_pct = mean(no_huddle, na.rm = TRUE),
    pass_touchdowns = sum(pass_touchdown, na.rm = TRUE),
    rush_touchdowns = sum(rush_touchdown, na.rm = TRUE),
    total_rush_yards = sum(rushing_yards, na.rm = TRUE),
    sacks_taken = sum(sack, na.rm = TRUE),
    interceptions_thrown = sum(interception, na.rm = TRUE),
    first_down_passes = sum(first_down_pass, na.rm = TRUE),
    first_down_rushes = sum(first_down_rush, na.rm = TRUE),
    air_yards_total = sum(air_yards, na.rm = TRUE),
    yac_total = sum(yards_after_catch, na.rm = TRUE),
    qb_scrambles = sum(qb_scramble, na.rm = TRUE),
    qb_spikes = sum(qb_spike, na.rm = TRUE),
    attempts_valid = sum(
      (complete_pass == 1 | incomplete_pass == 1 | interception == 1) &
        qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    comp_pct = 100 * pass_completions / attempts_valid,
    ypa = total_pass_yards / attempts_valid,
    pr_a = pmin(pmax((pass_completions / attempts_valid - 0.3) * 5, 0), 2.375),
    pr_b = pmin(pmax((total_pass_yards / attempts_valid - 3) * 0.25, 0), 2.375),
    pr_c = pmin(pmax((pass_touchdowns / attempts_valid) * 20, 0), 2.375),
    pr_d = pmin(pmax(2.375 - (interceptions_thrown / attempts_valid) * 25, 0), 2.375),
    passer_rating = (pr_a + pr_b + pr_c + pr_d) / 6 * 100
  ) %>%
  select(-pr_a, -pr_b, -pr_c, -pr_d) %>%
  filter(attempts_valid > 2) %>%
  select(-pass_attempts, -attempts_valid) %>%
  arrange(week, desc(epa_play))

# ===== Current week (REG) game-level aggregates =====
qb_25_current_week <- pbp_25 %>%
  filter(season_type == "REG", week == current_week) %>%
  group_by(game_id, posteam, defteam, week, id, name) %>%
  summarize(
    epa_play = mean(qb_epa, na.rm = TRUE),
    cpoe = mean(cpoe, na.rm = TRUE),
    pass_epa = mean(epa[play_type == "pass"], na.rm = TRUE),
    rush_epa = mean(epa[play_type == "run"], na.rm = TRUE),
    total_pass_yards = sum(passing_yards, na.rm = TRUE),
    dropbacks = sum(qb_dropback, na.rm = TRUE),
    pass_attempts = sum(pass_attempt, na.rm = TRUE),
    pass_completions = sum(complete_pass, na.rm = TRUE),
    shotgun_pct = mean(shotgun, na.rm = TRUE),
    no_huddle_pct = mean(no_huddle, na.rm = TRUE),
    pass_touchdowns = sum(pass_touchdown, na.rm = TRUE),
    rush_touchdowns = sum(rush_touchdown, na.rm = TRUE),
    total_rush_yards = sum(rushing_yards, na.rm = TRUE),
    sacks_taken = sum(sack, na.rm = TRUE),
    interceptions_thrown = sum(interception, na.rm = TRUE),
    first_down_passes = sum(first_down_pass, na.rm = TRUE),
    first_down_rushes = sum(first_down_rush, na.rm = TRUE),
    air_yards_total = sum(air_yards, na.rm = TRUE),
    yac_total = sum(yards_after_catch, na.rm = TRUE),
    qb_scrambles = sum(qb_scramble, na.rm = TRUE),
    qb_spikes = sum(qb_spike, na.rm = TRUE),
    attempts_valid = sum(
      (complete_pass == 1 | incomplete_pass == 1 | interception == 1) &
        qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    comp_pct = 100 * pass_completions / attempts_valid,
    ypa = total_pass_yards / attempts_valid,
    pr_a = pmin(pmax((pass_completions / attempts_valid - 0.3) * 5, 0), 2.375),
    pr_b = pmin(pmax((total_pass_yards / attempts_valid - 3) * 0.25, 0), 2.375),
    pr_c = pmin(pmax((pass_touchdowns / attempts_valid) * 20, 0), 2.375),
    pr_d = pmin(pmax(2.375 - (interceptions_thrown / attempts_valid) * 25, 0), 2.375),
    passer_rating = (pr_a + pr_b + pr_c + pr_d) / 6 * 100
  ) %>%
  select(-pr_a, -pr_b, -pr_c, -pr_d) %>%
  filter(attempts_valid > 2) %>%
  arrange(desc(epa_play))

# Useful REG-week means
mean_cpoe_current_week <- mean(qb_25_current_week$cpoe, na.rm = TRUE)
mean_epa_current_week  <- mean(qb_25_current_week$epa_play, na.rm = TRUE)

# ===== Weekly QB aggregates (POST) =====
qb_weekly_25_POST <- pbp_25 %>%
  filter(season_type == "POST") %>%
  group_by(week, id, name, posteam) %>%
  summarize(
    epa_play = mean(qb_epa, na.rm = TRUE),
    cpoe = mean(cpoe, na.rm = TRUE),
    pass_epa = mean(epa[play_type == "pass"], na.rm = TRUE),
    rush_epa = mean(epa[play_type == "run"], na.rm = TRUE),
    total_pass_yards = sum(passing_yards, na.rm = TRUE),
    dropbacks = sum(qb_dropback, na.rm = TRUE),
    pass_attempts = sum(pass_attempt, na.rm = TRUE),
    pass_completions = sum(complete_pass, na.rm = TRUE),
    shotgun_pct = mean(shotgun, na.rm = TRUE),
    no_huddle_pct = mean(no_huddle, na.rm = TRUE),
    pass_touchdowns = sum(pass_touchdown, na.rm = TRUE),
    rush_touchdowns = sum(rush_touchdown, na.rm = TRUE),
    total_rush_yards = sum(rushing_yards, na.rm = TRUE),
    sacks_taken = sum(sack, na.rm = TRUE),
    interceptions_thrown = sum(interception, na.rm = TRUE),
    first_down_passes = sum(first_down_pass, na.rm = TRUE),
    first_down_rushes = sum(first_down_rush, na.rm = TRUE),
    air_yards_total = sum(air_yards, na.rm = TRUE),
    yac_total = sum(yards_after_catch, na.rm = TRUE),
    qb_scrambles = sum(qb_scramble, na.rm = TRUE),
    qb_spikes = sum(qb_spike, na.rm = TRUE),
    attempts_valid = sum(
      (complete_pass == 1 | incomplete_pass == 1 | interception == 1) &
        qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    comp_pct = 100 * pass_completions / attempts_valid,
    ypa = total_pass_yards / attempts_valid,
    pr_a = pmin(pmax((pass_completions / attempts_valid - 0.3) * 5, 0), 2.375),
    pr_b = pmin(pmax((total_pass_yards / attempts_valid - 3) * 0.25, 0), 2.375),
    pr_c = pmin(pmax((pass_touchdowns / attempts_valid) * 20, 0), 2.375),
    pr_d = pmin(pmax(2.375 - (interceptions_thrown / attempts_valid) * 25, 0), 2.375),
    passer_rating = (pr_a + pr_b + pr_c + pr_d) / 6 * 100
  ) %>%
  select(-pr_a, -pr_b, -pr_c, -pr_d) %>%
  filter(attempts_valid > 2) %>%
  select(-pass_attempts, -attempts_valid) %>%
  arrange(week, desc(epa_play))

# ===== Current POST "week" game-level aggregates =====
qb_25_current_week_POST <- pbp_25 %>%
  filter(season_type == "POST", week == postseason_week) %>%
  group_by(game_id, posteam, defteam, week, id, name) %>%
  summarize(
    epa_play = mean(qb_epa, na.rm = TRUE),
    cpoe = mean(cpoe, na.rm = TRUE),
    pass_epa = mean(epa[play_type == "pass"], na.rm = TRUE),
    rush_epa = mean(epa[play_type == "run"], na.rm = TRUE),
    total_pass_yards = sum(passing_yards, na.rm = TRUE),
    dropbacks = sum(qb_dropback, na.rm = TRUE),
    pass_attempts = sum(pass_attempt, na.rm = TRUE),
    pass_completions = sum(complete_pass, na.rm = TRUE),
    shotgun_pct = mean(shotgun, na.rm = TRUE),
    no_huddle_pct = mean(no_huddle, na.rm = TRUE),
    pass_touchdowns = sum(pass_touchdown, na.rm = TRUE),
    rush_touchdowns = sum(rush_touchdown, na.rm = TRUE),
    total_rush_yards = sum(rushing_yards, na.rm = TRUE),
    sacks_taken = sum(sack, na.rm = TRUE),
    interceptions_thrown = sum(interception, na.rm = TRUE),
    first_down_passes = sum(first_down_pass, na.rm = TRUE),
    first_down_rushes = sum(first_down_rush, na.rm = TRUE),
    air_yards_total = sum(air_yards, na.rm = TRUE),
    yac_total = sum(yards_after_catch, na.rm = TRUE),
    qb_scrambles = sum(qb_scramble, na.rm = TRUE),
    qb_spikes = sum(qb_spike, na.rm = TRUE),
    attempts_valid = sum(
      (complete_pass == 1 | incomplete_pass == 1 | interception == 1) &
        qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    comp_pct = 100 * pass_completions / attempts_valid,
    ypa = total_pass_yards / attempts_valid,
    pr_a = pmin(pmax((pass_completions / attempts_valid - 0.3) * 5, 0), 2.375),
    pr_b = pmin(pmax((total_pass_yards / attempts_valid - 3) * 0.25, 0), 2.375),
    pr_c = pmin(pmax((pass_touchdowns / attempts_valid) * 20, 0), 2.375),
    pr_d = pmin(pmax(2.375 - (interceptions_thrown / attempts_valid) * 25, 0), 2.375),
    passer_rating = (pr_a + pr_b + pr_c + pr_d) / 6 * 100
  ) %>%
  select(-pr_a, -pr_b, -pr_c, -pr_d) %>%
  filter(attempts_valid > 2) %>%
  arrange(desc(epa_play))


# Merge PFR advanced (season) into season table
#qb_weekly_25 <- qb_weekly_25 %>%
#  left_join(qb_adv_weekly_25, by = "name")