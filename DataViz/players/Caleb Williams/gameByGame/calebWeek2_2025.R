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

# GAME PLAY BY PLAY

# ---- Plot: Play-by-Play EPA (Week 2) ----
ggplot(caleb_pbp_week_2, aes(x = play_index, y = epa)) +
  # Points per play (size by |EPA|, shape by play type)
  geom_point(aes(size = abs(epa), shape = play_cat), alpha = 0.9) +
  # Rolling EPA line to show trend
  geom_line(aes(y = epa_roll5), linewidth = 1, alpha = 0.8) +
  # Horizontal reference lines
  geom_hline(yintercept = caleb_game_avg_epa_week_2, linetype = "dotted", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotdash", alpha = 0.6) +
  # Label biggest positive/negative plays
  ggrepel::geom_text_repel(
    data = label_candidates,
    aes(label = label_text),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.25,
    segment.color = "black",
    max.overlaps = Inf
  ) +
  scale_size_continuous(name = "|EPA|") +
  labs(
    title = "C.Williams — Play-by-Play EPA (Week 2)",
    subtitle = paste0("Opponent: ", caleb_opp_week_2, 
                      "   ·   Dotted = Game avg (", round(caleb_game_avg_epa_week_2, 3), 
                      "),  Dot-dash = 0  ·  Rolling line = 5-play mean"),
    x = "Play Sequence (Week 1)",
    y = "EPA",
    shape = "Play Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")


