# Orchestrates the full pipeline
source("scripts/00_setup.R")
source("scripts/01_load_raw.R")
source("scripts/02_clean_names.R")
source("scripts/03_qb_season_aggregates.R")
source("scripts/04_qb_weekly_aggregates.R")

message("Pipeline complete ✅")
