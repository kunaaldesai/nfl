# ===== Season RB aggregates (REG only) + merge adv =====

rb_25 <- pbp_25 %>%
  filter(season_type == "REG") %>%
  filter(!is.na(rusher_player_name), rusher_player_name != "") %>%
  group_by(id, rusher_player_name, posteam) %>%
  summarize(
    epa_play = mean(epa, na.rm = TRUE),
    total_epa = sum(epa, na.rm = TRUE),
    rush_attempts = sum(rush_attempt, na.rm = TRUE),
    avg_rush_attempts = mean(rush_attempt, na.rm = TRUE),
    total_rushing_yards = sum(rushing_yards, na.rm = TRUE),
    avg_rushing_yards = mean(rushing_yards, na.rm = TRUE),
    total_yards_gained = sum(yards_gained, na.rm = TRUE),
    rushing_touchdowns = sum(rush_touchdown, na.rm = TRUE),
    avg_rushing_touchdowns = mean(rush_touchdown, na.rm = TRUE),
    rushing_first_downs = sum(first_down_rush, na.rm = TRUE),
    avg_rushing_first_downs = mean(first_down_rush, na.rm = TRUE)
  ) %>%
  filter(rush_attempts >= 5) %>%
  arrange(desc(rush_attempts))

# Merge PFR advanced (season) into season table
rb_25 <- rb_25 %>%
  left_join(rb_adv_25, by = c("rusher_player_name", "posteam"))