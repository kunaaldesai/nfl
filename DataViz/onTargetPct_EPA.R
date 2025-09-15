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



# X = On Tgt Pct, Y = EPA
ggplot(data = qb_25,
       mapping = aes(x = on_tgt_pct, y = epa_play)) +
  geom_point(aes(size = dropbacks, color = name), alpha = 0.7) +
  geom_segment(aes(xend = on_tgt_pct, yend = epa_play),
               data = qb_25,
               color = "black", alpha = 0.3) +  # Add transparent black lines
  geom_text_repel(aes(label = name),
                  size = 3,
                  box.padding = 0.5,
                  point.padding = 0.2,
                  segment.color = "black",
                  max.overlaps = Inf) +
  geom_hline(yintercept = mean_epa, linetype = "dotted", color = "red", alpha = 0.8) +
  geom_vline(xintercept = mean_on_tgt_pct, linetype = "dotted", color = "red", alpha = 0.8) +
  labs(title = "Quarterback Accuracy Over Production (2025 NFL Season)",
       x = "On Target Percentage",
       y = "EPA/Play",
       size = "Dropbacks",
       color = "Player Name") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(color = 'none')