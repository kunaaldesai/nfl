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

# PFR advanced WR stats (season + weekly)
wr_adv_weekly_25 <- load_pfr_advstats(
  seasons = season,
  stat_type = c("rec"),
  summary_level = c("week")
)

wr_adv_25 <- load_pfr_advstats(
  seasons = season,
  stat_type = c("rec"),
  summary_level = c("season")
)

wr_adv <- load_pfr_advstats(
  seasons = TRUE,
  stat_type = c("rec"),
  summary_level = c("season")
)


wr_adv_weekly <- load_pfr_advstats(
  seasons = TRUE,
  stat_type = c("rec"),
  summary_level = c("week")
)

# PFR advanced RB stats (season + weekly)
rb_adv_weekly_25 <- load_pfr_advstats(
  seasons = season,
  stat_type = c("rush"),
  summary_level = c("week")
)

rb_adv_25 <- load_pfr_advstats(
  seasons = season,
  stat_type = c("rush"),
  summary_level = c("season")
)

rb_adv <- load_pfr_advstats(
  seasons = TRUE,
  stat_type = c("rush"),
  summary_level = c("season")
)


rb_adv_weekly <- load_pfr_advstats(
  seasons = TRUE,
  stat_type = c("rush"),
  summary_level = c("week")
)

# PFR advanced DEF stats (season + weekly)
def_adv_weekly_25 <- load_pfr_advstats(
  seasons = season,
  stat_type = c("def"),
  summary_level = c("week")
)

def_adv_25 <- load_pfr_advstats(
  seasons = season,
  stat_type = c("def"),
  summary_level = c("season")
)

def_adv <- load_pfr_advstats(
  seasons = TRUE,
  stat_type = c("def"),
  summary_level = c("season")
)


def_adv_weekly <- load_pfr_advstats(
  seasons = TRUE,
  stat_type = c("def"),
  summary_level = c("week")
)



