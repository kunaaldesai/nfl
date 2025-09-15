# ===== Setup (packages, options, simple config) =====

# Loading Packages
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

# Config you might tweak week-to-week
current_week <- 2
postseason_week <- 19
season <- 2025

# Print for visibility
message(glue::glue("Season: {season}, REG week: {current_week}, POST week: {postseason_week}"))
