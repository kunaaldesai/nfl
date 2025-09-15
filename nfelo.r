# FFL_Data_Cleaning
##### Loading Packages
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
library(nflverse)


# Specify the file path
#nflelo_original <- "~/Documents/Kunaal's Code/R/Football/Gambling/Spreadsheets/Load/nfelo_2023.csv"
# Load the CSV file into a data frame
#nflelo_original <- read.csv(nflelo_original)

#nflelo_original <- nflelo_original %>%
#  select(-elo_rank, everything()) %>%
#  arrange(-nfelo)
# Save fantasy_stats as a CSV file
#write.csv(nflelo_original, file = "~/Documents/Kunaal's Code/R/Football/Gambling/Spreadsheets/nflelo_original.csv", row.names = FALSE)
