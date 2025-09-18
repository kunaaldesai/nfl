# ===== Season WR aggregates (REG only) + merge adv =====

wr_weekly_25 <- pbp_25 %>%
  filter(season_type == "REG") %>%
  filter(!is.na(receiver_player_name), receiver_player_name != "") %>%
  group_by(week, id, receiver_player_name, posteam, defteam) %>%
  summarize(
    epa_play = mean(epa, na.rm = TRUE),
    total_epa = sum(epa, na.rm = TRUE),
    targets = n(),
    yac_epa_play = mean(yac_epa, na.rm= TRUE),
    total_yac_epa = sum(yac_epa, na.rm = TRUE),
    completed_yac_epa = mean(comp_yac_epa, na.rm = TRUE),
    total_completed_yac_epa = sum(comp_yac_epa, na.rm = TRUE),
    xyac_epa_play = mean(xyac_epa, na.rm = TRUE),
    total_xyac_epa = sum(xyac_epa, na.rm = TRUE),
    xyac_mean_yardage_play = mean(xyac_mean_yardage, na.rm = TRUE),
    total_xyac_mean_yardage = sum(xyac_mean_yardage, na.rm = TRUE),
    xyac_median_yardage_play	= mean(xyac_median_yardage, na.rm = TRUE),
    total_xyac_median_yardage	= mean(xyac_median_yardage, na.rm = TRUE),
    total_receiving_yards = sum(receiving_yards, na.rm = TRUE),
    total_yards_gained = sum(yards_gained, na.rm = TRUE),
    total_yac = sum(yards_after_catch, na.rm = TRUE),
    avg_yac = mean(yards_after_catch, na.rm = TRUE),
    rec_touchdown = sum(pass_touchdown, na.rm = TRUE),
    avg_rec_touchdown = mean(pass_touchdown, na.rm = TRUE)
  ) %>%
  filter(targets >= 1) %>%
  arrange(desc(targets))