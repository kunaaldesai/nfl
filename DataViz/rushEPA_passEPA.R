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
library(zoo)

# X = Rush EPA, Y = Pass QB EPA
ggplot(data = qb_25,
       mapping = aes(x = rush_epa, y = pass_epa)) +
  geom_point(aes(size = dropbacks, color = name), alpha = 0.7) +
  geom_segment(aes(xend = rush_epa, yend = pass_epa),
               data = qb_25,
               color = "black", alpha = 0.3) +  # Add transparent black lines
  geom_text_repel(aes(label = name),
                  size = 3,
                  box.padding = 0.5,
                  point.padding = 0.2,
                  segment.color = "black",
                  max.overlaps = Inf) +
  geom_hline(yintercept = mean_pass_epa, linetype = "dotted", color = "red", alpha = 0.8) +
  geom_vline(xintercept = mean_rush_epa, linetype = "dotted", color = "red", alpha = 0.8) +
  labs(title = "Pass vs Rush EPA/Play (Excl. Players with 0 rushes)",
       x = "Rush EPA/Play",
       y = "Pass EPA/Play",
       size = "Dropbacks",
       color = "Player Name") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(color = 'none')

  # Current Week
# X = Rush EPA, Y = Pass EPA
ggplot(data = team_24,
       mapping = aes(x = rush_epa_play, y = pass_epa_play)) +
  geom_point(aes(size = total_yards, color = posteam), alpha = 0.7) +
  geom_segment(aes(xend = rush_epa_play, yend = pass_epa_play),
               data = team_24,
               color = "black", alpha = 0.3) +  # Add transparent black lines
  geom_text_repel(aes(label = posteam),
                  size = 3,
                  box.padding = 0.5,
                  point.padding = 0.2,
                  segment.color = "black",
                  max.overlaps = Inf) +
  geom_hline(yintercept = mean_team_pass_epa, linetype = "dotted", color = "red", alpha = 0.8) +
  geom_vline(xintercept = mean_team_rush_epa, linetype = "dotted", color = "red", alpha = 0.8) +
  labs(title = "Comparing Rush vs Pass EPA/Play",
       x = "EPA/Play on Rushing Plays",
       y = "EPA/Play on Passing Plays",
       size = "Total Yards",
       color = "Team Name") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(color = 'none')