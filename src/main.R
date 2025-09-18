# Orchestrates the full pipeline
source("src/scripts/00_setup.R")
source("src/scripts/01_load_raw.R")
source("src/scripts/02_clean_names.R")
source("src/scripts/03_qb_season_aggregates.R")
source("src/scripts/04_qb_weekly_aggregates.R")
source("src/scripts/05_caleb_analysis.R")
source("src/scripts/06_wr_season_aggregates.R")
source("src/scripts/07_wr_weekly_aggregates.R")
source("src/scripts/08_rb_season_aggregates.R")
source("src/scripts/09_rb_weekly_aggregates.R")




message("Pipeline complete ✅")


