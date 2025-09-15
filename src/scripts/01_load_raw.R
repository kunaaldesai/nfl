# ===== Load raw data (pbp + PFR advanced) =====

# pbp for the current season
pbp_25 <- load_pbp(season)

# PFR advanced QB stats (season + weekly)
qb_adv_weekly_25 <- load_pfr_advstats(
  seasons = season,
  stat_type = c("pass"),
  summary_level = c("week")
)

qb_adv_25 <- load_pfr_advstats(
  seasons = season,
  stat_type = c("pass"),
  summary_level = c("season")
)
