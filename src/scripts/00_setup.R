# ===== Setup =====

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

# tweak each week
current_week <- 3
postseason_week <- 19
season <- 2025

# Print
message(glue::glue("Season: {season}, REG week: {current_week}, POST week: {postseason_week}"))
