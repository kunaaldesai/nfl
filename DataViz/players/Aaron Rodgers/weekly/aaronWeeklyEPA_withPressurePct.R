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

# Aaron Rodgers Weekly EPA with Pressure Pct
ggplot(ar_weekly_plot, aes(x = week, y = epa_play)) +
  geom_line(alpha = 0.7) +
  geom_point(aes(size = dropbacks, color = times_pressured_pct), alpha = 0.9) +
  ggrepel::geom_text_repel(
    aes(label = defteam),
    size = 3,
    box.padding = 0.4,
    point.padding = 0.2,
    segment.color = "black",
    max.overlaps = Inf
  ) +
  geom_hline(yintercept = ar_avg_epa, linetype = "dotted", alpha = 0.8) +
  geom_line(aes(y = league_mean_epa), linetype = "dotdash", alpha = 0.7) +
  scale_x_continuous(breaks = unique(ar_weekly_plot$week)) +
  scale_color_gradient(
    name = "Times Pressured %",
    low = "lightblue",
    high = "darkblue",
    labels = scales::percent_format(accuracy = 1)   # expects 0–1 input
    # , limits = c(0, 0.5)  # optional: fix scale to 0–50% for consistency
  ) +
  labs(
    title = "Aaron Rodgers — Weekly EPA/Play vs League Mean (2024 REG)",
    subtitle = paste0("Dotted = Player avg (", round(ar_avg_epa, 3),
                      "), Dot-dash = League weekly mean"),
    x = "Week",
    y = "EPA/Play",
    size = "Dropbacks"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal")