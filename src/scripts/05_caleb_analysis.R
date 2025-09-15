# ===== Caleb Williams weekly & league mean track =====

cw_weekly <- qb_weekly_25 %>%
  filter(name == "C.Williams") %>%
  arrange(week)

cw_avg_epa <- mean(cw_weekly$epa_play, na.rm = TRUE)

league_mean_by_week <- qb_weekly_25 %>%
  group_by(week) %>%
  summarize(league_mean_epa = mean(epa_play, na.rm = TRUE), .groups = "drop")

cw_weekly_plot <- cw_weekly %>%
  left_join(league_mean_by_week, by = "week")

# ===== pbp slices (week 1 / Caleb) =====

pbp_week_1 <- pbp_25 %>%
  group_by(id, name, posteam) %>%
  filter(week == 1)

pbp_week_2 <- pbp_25 %>%
  group_by(id, name, posteam) %>%
  filter(week == 2)

caleb_pbp_week_1 <- pbp_25 %>%
  filter(week == 1, name == "C.Williams") %>%
  filter(!is.na(epa)) %>%
  arrange(game_id, desc(game_seconds_remaining), qtr, desc(quarter_seconds_remaining)) %>%
  mutate(
    play_index = row_number(),
    play_cat = case_when(
      !is.na(sack) & sack == 1 ~ "Sack",
      !is.na(interception) & interception == 1 ~ "Interception",
      !is.na(fumble_lost) & fumble_lost == 1 ~ "Fumble Lost",
      !is.na(pass) & pass == 1 ~ "Pass",
      !is.na(rush) & rush == 1 ~ "Rush",
      !is.na(play_type) ~ str_to_title(play_type),
      TRUE ~ "Other"
    ),
    epa_roll5 = zoo::rollmean(epa, k = 5, fill = NA, align = "right")
  )

caleb_pbp_week_2 <- pbp_25 %>%
  filter(week == 2, name == "C.Williams") %>%
  filter(!is.na(epa)) %>%
  arrange(game_id, desc(game_seconds_remaining), qtr, desc(quarter_seconds_remaining)) %>%
  mutate(
    play_index = row_number(),
    play_cat = case_when(
      !is.na(sack) & sack == 1 ~ "Sack",
      !is.na(interception) & interception == 1 ~ "Interception",
      !is.na(fumble_lost) & fumble_lost == 1 ~ "Fumble Lost",
      !is.na(pass) & pass == 1 ~ "Pass",
      !is.na(rush) & rush == 1 ~ "Rush",
      !is.na(play_type) ~ str_to_title(play_type),
      TRUE ~ "Other"
    ),
    epa_roll5 = zoo::rollmean(epa, k = 5, fill = NA, align = "right")
  )

caleb_opp <- caleb_pbp_week_1 %>% distinct(defteam) %>% pull(defteam) %>% paste(collapse = "/")
caleb_game_avg_epa_week_1 <- mean(caleb_pbp_week_1$epa, na.rm = TRUE)
caleb_opp <- caleb_pbp_week_2 %>% distinct(defteam) %>% pull(defteam) %>% paste(collapse = "/")
caleb_game_avg_epa_week_2 <- mean(caleb_pbp_week_2$epa, na.rm = TRUE)

# Label only the most impactful snaps (top-3 & bottom-3 EPA)
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
      if (!is.null(.$pass_touchdown) && any(!is.na(.$pass_touchdown)))
        ifelse(coalesce(pass_touchdown, 0) == 1, " TD", ""),
      if (!is.null(.$rush_touchdown) && any(!is.na(.$rush_touchdown)))
        ifelse(coalesce(rush_touchdown, 0) == 1, " TD", "")
    )
  )
