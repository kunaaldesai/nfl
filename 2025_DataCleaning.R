# FFL_Data_Cleaning
##### Loading Packages
library(tidyverse)
library(nflreadr)
library(nflfastR)
library(nflplotR)
library(nflverse)
library(nflseedR)
library(nfl4th)
library(ffopportunity)
library(ggrepel)
library(stringr)
library(zoo)
library(nflverse)

#pbp_25 <- load_pbp(2025)
pbp_25 <- load_pbp(2025)
current_week <- 2
postseason_week <- 19

qb_adv_weekly_25 <- load_pfr_advstats(
  seasons = 2025,
  #seasons = most_recent_season(),
  stat_type = c("pass"),
  summary_level = c("week")
)


qb_adv_25 <- load_pfr_advstats(
  seasons = 2025,
  #seasons = most_recent_season(),
  stat_type = c("pass"),
  summary_level = c("season")
)

# Step 1: Convert full names to "F.Lastname" format
# Step 1 (yours): normalize to "F.Lastname"

qb_adv_25 <- qb_adv_25 %>%
  mutate(
    name = str_replace(player, "^([A-Za-z]+)\\s+([A-Za-z'\\-]+)$", "\\1.\\2"),
    name = str_replace(name, "^(.).*?\\.(.*)$", "\\1.\\2")
  )

# Step 1b: manual fixes for edge cases

qb_adv_25 <- qb_adv_25 %>%
  mutate(
    name = case_when(
      player == "Gardner Minshew II" ~ "G.Minshew II",
      str_detect(player, regex("^C\\.?\\s*J\\.?\\s*Stroud$", ignore_case = FALSE)) ~ "C.Stroud",
      TRUE ~ name
    )
  )

# Step 1: Convert full names to "F.Lastname" format
# Step 1 (yours): normalize to "F.Lastname"
qb_adv_weekly_25 <- qb_adv_weekly_25 %>%
  mutate(
    name = str_replace(pfr_player_name, "^([A-Za-z]+)\\s+([A-Za-z'\\-]+)$", "\\1.\\2"),
    name = str_replace(name, "^(.).*?\\.(.*)$", "\\1.\\2")
  )


# Step 1b: manual fixes for edge cases
qb_adv_weekly_25 <- qb_adv_weekly_25 %>%
  mutate(
    name = case_when(
      pfr_player_name == "Gardner Minshew II" ~ "G.Minshew II",
      str_detect(pfr_player_name, regex("^C\\.?\\s*J\\.?\\s*Stroud$", ignore_case = FALSE)) ~ "C.Stroud",
      TRUE ~ name
    )
  )


# qb_25 qnd qb_24
qb_25 <- pbp_25 %>%
  filter(season_type == "REG") %>%
  group_by(id, name, posteam) %>%
  summarize(
    epa_play = mean(qb_epa, na.rm = T),
    cpoe = mean(cpoe, na.rm=T),
    pass_epa = mean(epa[play_type=="pass"], na.rm = T),
    rush_epa = mean(epa[play_type=="run"], na.rm = T),
    total_epa = sum(qb_epa, na.rm = T),
    total_pass_yards = sum(passing_yards, na.rm=T),
    dropbacks = sum(qb_dropback, na.rm = T),
    pass_attempts = sum(pass_attempt, na.rm=T),
    pass_completions = sum(complete_pass, na.rm = T),
    # new QB-related stats
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
    # attempts that *count* in official stats
    attempts_valid = sum(
      (complete_pass == 1 | incomplete_pass == 1 | interception == 1) &
        qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0,
      na.rm = TRUE
    )
  ) %>%
  mutate(
    comp_pct = 100 * pass_completions / attempts_valid,
    ypa = total_pass_yards / attempts_valid,
    
    # NFL passer rating with valid attempts
    pr_a = pmin(pmax((pass_completions / attempts_valid - 0.3) * 5, 0), 2.375),
    pr_b = pmin(pmax((total_pass_yards / attempts_valid - 3) * 0.25, 0), 2.375),
    pr_c = pmin(pmax((pass_touchdowns / attempts_valid) * 20, 0), 2.375),
    pr_d = pmin(pmax(2.375 - (interceptions_thrown / attempts_valid) * 25, 0), 2.375),
    passer_rating = (pr_a + pr_b + pr_c + pr_d) / 6 * 100
  ) %>%
  select(-pr_a, -pr_b, -pr_c, -pr_d) %>%
  filter(attempts_valid > 15) %>%
  select(-pass_attempts, -attempts_valid) %>%
  arrange(desc(epa_play))




# Step 2: Merge qb_adv_24 into qb_24 by name
qb_25 <- qb_25 %>%
  left_join(qb_adv_25, by = "name")





qb_25_current_week <- pbp_25 %>%
  filter(season_type == "REG") %>%
  filter(week==current_week) %>%
  #group_by(id, name, posteam) %>%
  group_by(game_id, posteam, defteam, week, id, name) %>%
  summarize(
    epa_play = mean(qb_epa, na.rm = T),
    cpoe = mean(cpoe, na.rm=T),
    pass_epa = mean(epa[play_type=="pass"], na.rm = T),
    rush_epa = mean(epa[play_type=="run"], na.rm = T),
    total_pass_yards = sum(passing_yards, na.rm=T),
    dropbacks = sum(qb_dropback, na.rm = T),
    pass_attempts = sum(pass_attempt, na.rm=T),
    pass_completions = sum(complete_pass, na.rm = T),
    # new QB-related stats
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
    # attempts that *count* in official stats
    attempts_valid = sum(
      (complete_pass == 1 | incomplete_pass == 1 | interception == 1) &
        qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0,
      na.rm = TRUE
    )
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


qb_25_current_week_POST <- pbp_25 %>%
  filter(season_type == "POST") %>%
  filter(week==postseason_week) %>%
  #group_by(id, name, posteam) %>%
  group_by(game_id, posteam, defteam, week, id, name) %>%
  summarize(
    epa_play = mean(qb_epa, na.rm = T),
    cpoe = mean(cpoe, na.rm=T),
    pass_epa = mean(epa[play_type=="pass"], na.rm = T),
    rush_epa = mean(epa[play_type=="run"], na.rm = T),
    total_pass_yards = sum(passing_yards, na.rm=T),
    dropbacks = sum(qb_dropback, na.rm = T),
    pass_attempts = sum(pass_attempt, na.rm=T),
    pass_completions = sum(complete_pass, na.rm = T),
    # new QB-related stats
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
    # attempts that *count* in official stats
    attempts_valid = sum(
      (complete_pass == 1 | incomplete_pass == 1 | interception == 1) &
        qb_spike == 0 & qb_kneel == 0 & two_point_attempt == 0,
      na.rm = TRUE
    )
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






# ==== Weekly QB aggregates (REG) ====
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
    
    # attempts that *count* in official stats
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
    
    # NFL passer rating with valid attempts
    pr_a = pmin(pmax((pass_completions / attempts_valid - 0.3) * 5, 0), 2.375),
    pr_b = pmin(pmax((total_pass_yards / attempts_valid - 3) * 0.25, 0), 2.375),
    pr_c = pmin(pmax((pass_touchdowns / attempts_valid) * 20, 0), 2.375),
    pr_d = pmin(pmax(2.375 - (interceptions_thrown / attempts_valid) * 25, 0), 2.375),
    passer_rating = (pr_a + pr_b + pr_c + pr_d) / 6 * 100
  ) %>%
  select(-pr_a, -pr_b, -pr_c, -pr_d) %>%
  # keep only weeks with some real volume
  filter(attempts_valid > 2) %>%
  # match your season table: drop raw attempts_valid and pass_attempts
  select(-pass_attempts, -attempts_valid) %>%
  arrange(week, desc(epa_play))



# ==== (Optional) Weekly QB aggregates (POST) ====
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





# Calculate mean of avg_cpoe and avg_passer_rating and avg_passing_epa
mean_cpoe_current_week <- mean(qb_25_current_week$cpoe, na.rm = TRUE)
#mean_avg_passer_rating <- mean(QB_season_averages$passer_rating, na.rm = TRUE)
mean_epa_current_week <- mean(qb_25_current_week$epa_play, na.rm = TRUE)
mean_epa <- mean(qb_25$epa_play, na.rm = T)
mean_cpoe <- mean(qb_25$cpoe, na.rm = T)
mean_on_tgt_pct <- mean(qb_25$on_tgt_pct, na.rm = T)
mean_pressure_pct <- mean(qb_25$pressure_pct, na.rm = T)
mean_pocket_time <- mean(qb_25$pocket_time, na.rm = T)
mean_sacks_taken <- mean(qb_25$sacks_taken, na.rm = T)
mean_bad_throw_pct <- mean(qb_25$bad_throw_pct, na.rm = T)
mean_pass_epa <- mean(qb_25$pass_epa, na.rm = T)
mean_rush_epa <- mean(qb_25$rush_epa, na.rm = T)



# ---- Filter & prep: Caleb Williams weekly ----
cw_weekly <- qb_weekly_25 %>%
  filter(name == "C.Williams") %>%
  arrange(week)

cw_avg_epa <- mean(cw_weekly$epa_play, na.rm = TRUE)

# ---- Filter & prep: Caleb Williams weekly ----
league_mean_by_week <- qb_weekly_25 %>%
  group_by(week) %>%
  summarize(league_mean_epa = mean(epa_play, na.rm = TRUE), .groups = "drop")

cw_weekly_plot <- cw_weekly %>%
  left_join(league_mean_by_week, by = "week")




# for pbp do game by game

pbp_week_1 <- pbp_25 %>%
  group_by(id, name, posteam) %>%
  filter(week == 1)

caleb_pbp_week_1 <- pbp_25 %>%
  group_by(id, name, posteam) %>%
  filter(name == "C.Williams") %>%
  filter(week == 1)





# ---- Filter to C.Williams, Week 6 (from your pbp_24) ----
caleb_pbp_week_1 <- pbp_25 %>%
  filter(week == 1, name == "C.Williams") %>%
  # Keep only rows with a numeric EPA
  filter(!is.na(epa)) %>%
  # Order plays in game sequence; prefer game_seconds_remaining if present
  arrange(game_id, desc(game_seconds_remaining), qtr, desc(quarter_seconds_remaining)) %>%
  mutate(
    play_index = row_number(),
    # Basic play category (adjusts nicely if these columns are in nflfastR)
    play_cat = case_when(
      !is.na(sack) & sack == 1 ~ "Sack",
      !is.na(interception) & interception == 1 ~ "Interception",
      !is.na(fumble_lost) & fumble_lost == 1 ~ "Fumble Lost",
      !is.na(pass) & pass == 1 ~ "Pass",
      !is.na(rush) & rush == 1 ~ "Rush",
      !is.na(play_type) ~ str_to_title(play_type),
      TRUE ~ "Other"
    ),
    # Rolling EPA (last 5 plays)
    epa_roll5 = zoo::rollmean(epa, k = 5, fill = NA, align = "right")
  )

# If you prefer to peek at columns available:
# names(caleb_pbp_week_6)

# ---- Small helpers for labels & summaries ----
caleb_opp <- caleb_pbp_week_1 %>% distinct(defteam) %>% pull(defteam) %>% paste(collapse = "/")
caleb_game_avg_epa <- mean(caleb_pbp_week_1$epa, na.rm = TRUE)

# Label only most impactful snaps: top-3 and bottom-3 EPA
label_candidates <- bind_rows(
  caleb_pbp_week_1 %>% arrange(desc(epa)) %>% slice_head(n = 3),
  caleb_pbp_week_1 %>% arrange(epa)      %>% slice_head(n = 3)
) %>%
  mutate(
    label_text = paste0(
      "Q", ifelse(!is.na(qtr), qtr, "?"),
      if ("clock" %in% names(.)) paste0(" ", clock) else "",
      if ("down" %in% names(.) & "ydstogo" %in% names(.))
        paste0("  ", down, "&", ydstogo) else "",
      if ("yards_gained" %in% names(.))
        paste0("  ", yards_gained, "y") else "",
      "  (", round(epa, 2), " EPA)",
      if (!is.null(.$pass_touchdown) && any(!is.na(.$pass_touchdown))) ifelse(coalesce(pass_touchdown, 0) == 1, " TD", ""),
      if (!is.null(.$rush_touchdown) && any(!is.na(.$rush_touchdown))) ifelse(coalesce(rush_touchdown, 0) == 1, " TD", "")
    )
  )












