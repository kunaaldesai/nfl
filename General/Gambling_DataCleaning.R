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

#pbp_25 <- load_pbp(2025)
pbp_25 <- load_pbp(2025)
pbp_24 <- load_pbp(2024)
pbp_for_chatgpt <- pbp_24[1:2, ]
write.csv(pbp_for_chatgpt, "~/Documents/Kunaal's Code/R/Football/Gambling/Spreadsheets/GPT/pbp_for_chatgpt.csv", row.names = FALSE)
pbp_23 <- load_pbp(2023)

qb_adv_weekly_25 <- load_pfr_advstats(
  seasons = 2025,
  #seasons = most_recent_season(),
  stat_type = c("pass"),
  summary_level = c("week")
)

qb_adv_weekly_24 <- load_pfr_advstats(
  seasons = 2024,
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

qb_adv_24 <- load_pfr_advstats(
  seasons = 2024,
  #seasons = most_recent_season(),
  stat_type = c("pass"),
  summary_level = c("season")
)

qb_adv_25 <- qb_adv_25 %>%
  filter(pass_attempts >= 100)

qb_adv_24 <- qb_adv_24 %>%
  filter(pass_attempts >= 100)

# Step 1: Convert full names to "F.Lastname" format
# Step 1 (yours): normalize to "F.Lastname"

qb_adv_25 <- qb_adv_25 %>%
  mutate(
    name = str_replace(player, "^([A-Za-z]+)\\s+([A-Za-z'\\-]+)$", "\\1.\\2"),
    name = str_replace(name, "^(.).*?\\.(.*)$", "\\1.\\2")
  )

qb_adv_24 <- qb_adv_24 %>%
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

qb_adv_24 <- qb_adv_24 %>%
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

qb_adv_weekly_24 <- qb_adv_weekly_24 %>%
  mutate(
    name = str_replace(pfr_player_name, "^([A-Za-z]+)\\s+([A-Za-z'\\-]+)$", "\\1.\\2"),
    name = str_replace(name, "^(.).*?\\.(.*)$", "\\1.\\2")
  )

# Step 1b: manual fixes for edge cases

# Step 1b: manual fixes for edge cases
qb_adv_weekly_25 <- qb_adv_weekly_25 %>%
  mutate(
    name = case_when(
      pfr_player_name == "Gardner Minshew II" ~ "G.Minshew II",
      str_detect(pfr_player_name, regex("^C\\.?\\s*J\\.?\\s*Stroud$", ignore_case = FALSE)) ~ "C.Stroud",
      TRUE ~ name
    )
  )

qb_adv_weekly_24 <- qb_adv_weekly_24 %>%
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
  filter(attempts_valid > 100) %>%
  select(-pass_attempts, -attempts_valid) %>%
  arrange(desc(epa_play))

qb_24 <- pbp_24 %>%
  filter(season_type == "REG") %>%
  group_by(id, name, posteam) %>%
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
    
    # NFL passer rating with valid attempts
    pr_a = pmin(pmax((pass_completions / attempts_valid - 0.3) * 5, 0), 2.375),
    pr_b = pmin(pmax((total_pass_yards / attempts_valid - 3) * 0.25, 0), 2.375),
    pr_c = pmin(pmax((pass_touchdowns / attempts_valid) * 20, 0), 2.375),
    pr_d = pmin(pmax(2.375 - (interceptions_thrown / attempts_valid) * 25, 0), 2.375),
    passer_rating = (pr_a + pr_b + pr_c + pr_d) / 6 * 100
  ) %>%
  select(-pr_a, -pr_b, -pr_c, -pr_d) %>%
  filter(attempts_valid > 100) %>%
  select(-pass_attempts, -attempts_valid) %>%
  arrange(desc(epa_play))

# Step 2: Merge qb_adv_24 into qb_24 by name
qb_25 <- qb_25 %>%
  left_join(qb_adv_25, by = "name")

qb_24 <- qb_24 %>%
  left_join(qb_adv_24, by = "name")

qb_25_POST <- pbp_25 %>%
  filter(season_type == "POST") %>%
  group_by(id, name, posteam) %>%
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
  filter(attempts_valid > 5) %>%
  arrange(desc(epa_play))

qb_24_POST <- pbp_24 %>%
  filter(season_type == "POST") %>%
  group_by(id, name, posteam) %>%
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
  filter(attempts_valid > 5) %>%
  arrange(desc(epa_play))

qb_25_current_week <- pbp_25 %>%
  filter(season_type == "REG") %>%
  filter(week==1) %>%
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


qb_24_current_week <- pbp_24 %>%
  filter(season_type == "REG") %>%
  filter(week==18) %>%
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
  filter(week==20) %>%
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


qb_24_current_week_POST <- pbp_24 %>%
  filter(season_type == "POST") %>%
  filter(week==20) %>%
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


qb_weekly_24 <- pbp_24 %>%
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

# Join in PFR weekly advanced stats by name + week (you already normalized names)
qb_weekly_25 <- qb_weekly_25 %>%
  left_join(qb_adv_weekly_25, by = c("name", "week"))

qb_weekly_24 <- qb_weekly_24 %>%
  left_join(qb_adv_weekly_24, by = c("name", "week"))

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

qb_weekly_24_POST <- pbp_24 %>%
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
























team_24 <- pbp_24 %>%
  filter(season_type == "REG", !is.na(posteam)) %>%
  group_by(posteam) %>%
  summarize(
    epa_play = mean(epa, na.rm = T),
    total_epa = sum(epa, na.rm = T),
    cpoe_play = mean(cpoe, na.rm = T),
    total_passing_yards = sum(passing_yards, na.rm = T),
    total_rushing_yards = sum(rushing_yards, na.rm = T),
    total_yards = (total_passing_yards+total_rushing_yards),
    total_pass_TDs = sum(pass_touchdown, na.rm = T),
    total_rush_TDs = sum(rush_touchdown, na.rm = T),
    total_TDs = (total_pass_TDs+total_rush_TDs),
    pass_attempts = sum(pass_attempt, na.rm = T),
    pass_completions = sum(complete_pass, na.rm = T),
    rush_attempts = sum(rush_attempt, na.rm = T),
    pass_epa_play = mean(epa[play_type == "pass"], na.rm = TRUE),
    total_pass_epa = sum(epa[play_type == "pass"], na.rm = TRUE),
    rush_epa_play = mean(epa[play_type == "run"], na.rm = TRUE),
    total_rush_epa = sum(epa[play_type == "run"], na.rm = TRUE)
  ) %>%
  arrange(desc(epa_play))

team_24_current_week <- pbp_24 %>%
  filter(season_type == "REG", !is.na(posteam)) %>%
  filter(week==17) %>%
  group_by(game_id, posteam, week) %>%
  summarize(
    epa_play = mean(epa, na.rm = T),
    total_epa = sum(epa, na.rm = T),
    cpoe_play = mean(cpoe, na.rm = T),
    total_passing_yards = sum(passing_yards, na.rm = T),
    total_rushing_yards = sum(rushing_yards, na.rm = T),
    total_yards = (total_passing_yards+total_rushing_yards),
    total_pass_TDs = sum(pass_touchdown, na.rm = T),
    total_rush_TDs = sum(rush_touchdown, na.rm = T),
    total_TDs = (total_pass_TDs+total_rush_TDs),
    pass_attempts = sum(pass_attempt, na.rm = T),
    pass_completions = sum(complete_pass, na.rm = T),
    rush_attempts = sum(rush_attempt, na.rm = T),
    pass_epa_play = mean(epa[play_type == "pass"], na.rm = TRUE),
    total_pass_epa = sum(epa[play_type == "pass"], na.rm = TRUE),
    rush_epa_play = mean(epa[play_type == "run"], na.rm = TRUE),
    total_rush_epa = sum(epa[play_type == "run"], na.rm = TRUE)
  )

team_23 <- pbp_23 %>%
  filter(season_type == "REG", !is.na(posteam)) %>%
  group_by(game_id, posteam, week) %>%
  summarize(
    epa_play = mean(epa, na.rm = T),
    total_epa = sum(epa, na.rm = T),
    cpoe_play = mean(cpoe, na.rm = T),
    total_passing_yards = sum(passing_yards, na.rm = T),
    total_rushing_yards = sum(rushing_yards, na.rm = T),
    total_yards = (total_passing_yards+total_rushing_yards),
    total_pass_TDs = sum(pass_touchdown, na.rm = T),
    total_rush_TDs = sum(rush_touchdown, na.rm = T),
    total_TDs = (total_pass_TDs+total_rush_TDs),
    pass_attempts = sum(pass_attempt, na.rm = T),
    pass_completions = sum(complete_pass, na.rm = T),
    rush_attempts = sum(rush_attempt, na.rm = T),
    pass_epa_play = mean(epa[play_type == "pass"], na.rm = TRUE),
    total_pass_epa = sum(epa[play_type == "pass"], na.rm = TRUE),
    rush_epa_play = mean(epa[play_type == "run"], na.rm = TRUE),
    total_rush_epa = sum(epa[play_type == "run"], na.rm = TRUE)
  ) %>%
  arrange(desc(epa_play))

# Calculate mean of avg_cpoe and avg_passer_rating and avg_passing_epa
mean_cpoe <- mean(qb_24$cpoe, na.rm = TRUE)
#mean_avg_passer_rating <- mean(QB_season_averages$passer_rating, na.rm = TRUE)
mean_epa <- mean(qb_24$epa_play, na.rm = TRUE)
mean_pass_epa <- mean(qb_24$pass_epa, na.rm = TRUE)
mean_rush_epa <- mean(qb_24$rush_epa, na.rm = TRUE)
mean_pocket_time <- mean(qb_24$pocket_time, na.rm = TRUE)
mean_bad_throw_pct <- mean(qb_24$bad_throw_pct, na.rm = TRUE)
mean_pressure_pct <- mean(qb_24$pressure_pct, na.rm = TRUE)
mean_sacks_taken <- mean(qb_24$sacks_taken, na.rm = TRUE)
mean_on_tgt_pct <- mean(qb_24$on_tgt_pct, na.rm = TRUE)


# Calculate mean of avg_cpoe and avg_passer_rating and avg_passing_epa
#mean_cpoe_qb_weekly_24 <- mean(qb_weekly_24$cpoe, na.rm = TRUE)
#mean_avg_passer_rating <- mean(QB_season_averages$passer_rating, na.rm = TRUE)
#mean_epa_qb_weekly_24 <- mean(qb_weekly_24$epa_play, na.rm = TRUE)
#mean_pass_epa_qb_weekly_24 <- mean(qb_weekly_244$pass_epa, na.rm = TRUE)
#mean_rush_epa_qb_weekly_24 <- mean(qb_weekly_24$rush_epa, na.rm = TRUE)
#mean_pocket_time_qb_weekly_24 <- mean(qb_weekly_24$pocket_time, na.rm = TRUE)
#mean_bad_throw_pct_qb_weekly_24 <- mean(qb_weekly_24$bad_throw_pct, na.rm = TRUE)
#mean_pressure_pct_qb_weekly_24 <- mean(qb_weekly_24$pressure_pct, na.rm = TRUE)
#mean_sacks_taken_qb_weekly_24 <- mean(qb_weekly_24$sacks_taken, na.rm = TRUE)
#mean_on_tgt_pct_qb_weekly_24 <- mean(qb_weekly_24$on_tgt_pct, na.rm = TRUE)



mean_team_epa <- mean(team_24$epa_play, na.rm = TRUE)
mean_team_total_epa <- mean(team_24$total_epa, na.rm = TRUE)
mean_team_rush_epa <-mean(team_24$rush_epa_play, na.rm = TRUE)
mean_team_pass_epa <-mean(team_24$pass_epa_play, na.rm = TRUE)
mean_team23_epa <- mean(team_23$epa_play, na.rm = TRUE)
mean_team23_total_epa <- mean(team_23$total_epa, na.rm = TRUE)
mean_team23_rush_epa <-mean(team_23$rush_epa_play, na.rm = TRUE)
mean_team23_pass_epa <-mean(team_23$pass_epa_play, na.rm = TRUE)


# Calculate mean of avg_cpoe and avg_passer_rating and avg_passing_epa
mean_cpoe_current_week <- mean(qb_24_current_week$cpoe, na.rm = TRUE)
#mean_avg_passer_rating <- mean(QB_season_averages$passer_rating, na.rm = TRUE)
mean_epa_current_week <- mean(qb_24_current_week$epa_play, na.rm = TRUE)



# ---- Filter & prep: Caleb Williams weekly ----
cw_weekly <- qb_weekly_24 %>%
  filter(name == "C.Williams") %>%
  arrange(week)

cw_avg_epa <- mean(cw_weekly$epa_play, na.rm = TRUE)

# ---- Filter & prep: Caleb Williams weekly ----
league_mean_by_week <- qb_weekly_24 %>%
  group_by(week) %>%
  summarize(league_mean_epa = mean(epa_play, na.rm = TRUE), .groups = "drop")

cw_weekly_plot <- cw_weekly %>%
  left_join(league_mean_by_week, by = "week")




# ---- Filter & prep: Jayden Daniels weekly ----
jw_weekly <- qb_weekly_24 %>%
  filter(name == "J.Daniels") %>%
  arrange(week)

jw_avg_epa <- mean(jw_weekly$epa_play, na.rm = TRUE)

jw_weekly_plot <- jw_weekly %>%
  left_join(league_mean_by_week, by = "week")


#aaron rodgers

ar_weekly <- qb_weekly_24 %>%
  filter(name =="A.Rodgers") %>%
  arrange(week)


ar_avg_epa <- mean(ar_weekly$epa_play, na.rm = TRUE)

ar_weekly_plot <- ar_weekly %>%
  left_join(league_mean_by_week, by = "week")












# for pbp do game by game

pbp_week_6 <- pbp_24 %>%
  group_by(id, name, posteam) %>%
  filter(week == 6)

caleb_pbp_week_6 <- pbp_24 %>%
  group_by(id, name, posteam) %>%
  filter(name == "C.Williams") %>%
  filter(week == 6)

write.csv(pbp_week_6, "~/Documents/Kunaal's Code/R/Football/Gambling/Spreadsheets/GPT/pbp_week_6.csv", row.names = FALSE)
write.csv(caleb_pbp_week_6, "~/Documents/Kunaal's Code/R/Football/Gambling/Spreadsheets/GPT/caleb_pbp_week_6.csv", row.names = FALSE)




# ---- Filter to C.Williams, Week 6 (from your pbp_24) ----
caleb_pbp_week_6 <- pbp_24 %>%
  filter(week == 6, name == "C.Williams") %>%
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
caleb_opp <- caleb_pbp_week_6 %>% distinct(defteam) %>% pull(defteam) %>% paste(collapse = "/")
caleb_game_avg_epa <- mean(caleb_pbp_week_6$epa, na.rm = TRUE)

# Label only most impactful snaps: top-3 and bottom-3 EPA
label_candidates <- bind_rows(
  caleb_pbp_week_6 %>% arrange(desc(epa)) %>% slice_head(n = 3),
  caleb_pbp_week_6 %>% arrange(epa)      %>% slice_head(n = 3)
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
















