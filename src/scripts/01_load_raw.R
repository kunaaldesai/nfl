# ===== Load raw data (pbp + PFR advanced) =====

# pbp for the current season
pbp_25 <- load_pbp(season)

# pbp for all time (this will take a while to load, comment it out if you don't want it)
pbp <- load_pbp(seasons = T)

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

qb_adv <- load_pfr_advstats(
  seasons = TRUE,
  stat_type = c("pass"),
  summary_level = c("season")
)


qb_adv_weekly <- load_pfr_advstats(
  seasons = TRUE,
  stat_type = c("pass"),
  summary_level = c("week")
)


