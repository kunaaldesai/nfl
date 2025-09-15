#GamblingData_Viz
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
library(zoo)



# X = CPOE, Y = EPA
ggplot(data = qb_25,
       mapping = aes(x = cpoe, y = epa_play)) +
  geom_point(aes(size = dropbacks, color = name), alpha = 0.7) +
  geom_segment(aes(xend = cpoe, yend = epa_play),
               data = qb_25,
               color = "black", alpha = 0.3) +  # Add transparent black lines
  geom_text_repel(aes(label = name),
                  size = 3,
                  box.padding = 0.5,
                  point.padding = 0.2,
                  segment.color = "black",
                  max.overlaps = Inf) +
  geom_hline(yintercept = mean_epa, linetype = "dotted", color = "red", alpha = 0.8) +
  geom_vline(xintercept = mean_cpoe, linetype = "dotted", color = "red", alpha = 0.8) +
  labs(title = "Quarterback Efficiency (2025 NFL Season)",
       x = "Completion Percentage Above Expectation (CPOE)",
       y = "EPA/Play",
       size = "Dropbacks",
       color = "Player Name") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(color = 'none')



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

# Current Week
# X = CPOE, Y = EPA
ggplot(data = qb_25_current_week,
       mapping = aes(x = cpoe, y = epa_play)) +
  geom_point(aes(size = dropbacks, color = name), alpha = 0.7) +
  geom_segment(aes(xend = cpoe, yend = epa_play),
               data = qb_25_current_week,
               color = "black", alpha = 0.3) +  # Add transparent black lines
  geom_text_repel(aes(label = name),
                  size = 3,
                  box.padding = 0.5,
                  point.padding = 0.2,
                  segment.color = "black",
                  max.overlaps = Inf) +
  geom_hline(yintercept = mean_epa_current_week, linetype = "dotted", color = "red", alpha = 0.8) +
  geom_vline(xintercept = mean_cpoe_current_week, linetype = "dotted", color = "red", alpha = 0.8) +
  labs(title = "Quarterback Efficiency (Current Week)",
       x = "Completion Percentage Above Expectation (CPOE)",
       y = "EPA/Play",
       size = "Dropbacks",
       color = "Player Name") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(color = 'none')


# X = Pressure Pct, Y = EPA
ggplot(data = qb_25,
       mapping = aes(x = pressure_pct, y = epa_play)) +
  geom_point(aes(size = dropbacks, color = name), alpha = 0.7) +
  geom_segment(aes(xend = pressure_pct, yend = epa_play),
               data = qb_25,
               color = "black", alpha = 0.3) +  # Add transparent black lines
  geom_text_repel(aes(label = name),
                  size = 3,
                  box.padding = 0.5,
                  point.padding = 0.2,
                  segment.color = "black",
                  max.overlaps = Inf) +
  geom_hline(yintercept = mean_epa, linetype = "dotted", color = "red", alpha = 0.8) +
  geom_vline(xintercept = mean_pressure_pct, linetype = "dotted", color = "red", alpha = 0.8) +
  labs(title = "Quarterback Production Under Pressure (2024 NFL Season)",
       x = "Pressure Percentage (%)",
       y = "EPA/Play",
       size = "Dropbacks",
       color = "Player Name") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(color = 'none')



# X = Pocket Time, Y = EPA
ggplot(data = qb_25,
       mapping = aes(x = pocket_time, y = epa_play)) +
  geom_point(aes(size = dropbacks, color = name), alpha = 0.7) +
  geom_segment(aes(xend = pocket_time, yend = epa_play),
               data = qb_25,
               color = "black", alpha = 0.3) +  # Add transparent black lines
  geom_text_repel(aes(label = name),
                  size = 3,
                  box.padding = 0.5,
                  point.padding = 0.2,
                  segment.color = "black",
                  max.overlaps = Inf) +
  geom_hline(yintercept = mean_epa, linetype = "dotted", color = "red", alpha = 0.8) +
  geom_vline(xintercept = mean_pocket_time, linetype = "dotted", color = "red", alpha = 0.8) +
  labs(title = "Quarterback Efficiency (2025 NFL Season)",
       x = "Pocket Time",
       y = "EPA/Play",
       size = "Dropbacks",
       color = "Player Name") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(color = 'none')



# X = Sacks Taken, Y = EPA/Play
ggplot(data = qb_25,
       mapping = aes(x = sacks_taken, y = epa_play)) +
  geom_point(aes(size = dropbacks, color = name), alpha = 0.7) +
  geom_segment(aes(xend = sacks_taken, yend = epa_play),
               data = qb_25,
               color = "black", alpha = 0.3) +  # Add transparent black lines
  geom_text_repel(aes(label = name),
                  size = 3,
                  box.padding = 0.5,
                  point.padding = 0.2,
                  segment.color = "black",
                  max.overlaps = Inf) +
  geom_hline(yintercept = mean_epa, linetype = "dotted", color = "red", alpha = 0.8) +
  geom_vline(xintercept = mean_sacks_taken, linetype = "dotted", color = "red", alpha = 0.8) +
  labs(title = "Quarterback Efficiency (2025 NFL Season)",
       x = "Sacks Taken",
       y = "EPA/Play",
       size = "Dropbacks",
       color = "Player Name") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(color = 'none')





# X = Bad Throw Percentage, Y = EPA/Play
ggplot(data = qb_25,
       mapping = aes(x = bad_throw_pct, y = epa_play)) +
  geom_point(aes(size = dropbacks, color = name), alpha = 0.7) +
  geom_segment(aes(xend = bad_throw_pct, yend = epa_play),
               data = qb_25,
               color = "black", alpha = 0.3) +  # Add transparent black lines
  geom_text_repel(aes(label = name),
                  size = 3,
                  box.padding = 0.5,
                  point.padding = 0.2,
                  segment.color = "black",
                  max.overlaps = Inf) +
  geom_hline(yintercept = mean_epa, linetype = "dotted", color = "red", alpha = 0.8) +
  geom_vline(xintercept = mean_bad_throw_pct, linetype = "dotted", color = "red", alpha = 0.8) +
  labs(title = "Quarterback Efficiency (2025 NFL Season)",
       x = "Bad Throw Percentage",
       y = "EPA/Play",
       size = "Dropbacks",
       color = "Player Name") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(color = 'none')



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















ggplot(cw_weekly_plot, aes(x = week, y = epa_play)) +
  geom_line(alpha = 0.7) +
  geom_point(aes(size = dropbacks), alpha = 0.9) +
  ggrepel::geom_text_repel(
    aes(label = defteam),
    size = 3,
    box.padding = 0.4,
    point.padding = 0.2,
    segment.color = "black",
    max.overlaps = Inf
  ) +
  geom_hline(yintercept = cw_avg_epa, linetype = "dotted", alpha = 0.8) +
  geom_line(aes(y = league_mean_epa), linetype = "dotdash", alpha = 0.7) +
  scale_x_continuous(breaks = unique(cw_weekly_plot$week)) +
  labs(
    title = "C.Williams — Weekly EPA/Play vs League Mean (2024 REG)",
    subtitle = paste0("Dotted = Player avg (", round(cw_avg_epa, 3),
                      "), Dot-dash = League weekly mean"),
    x = "Week",
    y = "EPA/Play",
    size = "Dropbacks"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



ggplot(cw_weekly_plot, aes(x = week, y = epa_play)) +
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
  geom_hline(yintercept = cw_avg_epa, linetype = "dotted", alpha = 0.8) +
  geom_line(aes(y = league_mean_epa), linetype = "dotdash", alpha = 0.7) +
  scale_x_continuous(breaks = unique(cw_weekly_plot$week)) +
  scale_color_gradient(
    name = "Times Pressured %",
    low = "lightblue",
    high = "darkblue",
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Caleb Williams — Weekly EPA/Play vs League Mean (2024 REG)",
    subtitle = paste0("Dotted = Player avg (", round(cw_avg_epa, 3),
                      "), Dot-dash = League weekly mean"),
    x = "Week",
    y = "EPA/Play",
    size = "Dropbacks"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal")





# ---- Plot ----
ggplot(jw_weekly_plot, aes(x = week, y = epa_play)) +
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
  geom_hline(yintercept = jw_avg_epa, linetype = "dotted", alpha = 0.8) +
  geom_line(aes(y = league_mean_epa), linetype = "dotdash", alpha = 0.7) +
  scale_x_continuous(breaks = unique(jw_weekly_plot$week)) +
  scale_color_gradient(
    name = "Times Pressured %",
    low = "lightblue",
    high = "darkblue",
    labels = scales::percent_format(accuracy = 1)   # expects 0–1 input
    # , limits = c(0, 0.5)  # optional: fix scale to 0–50% for consistency
  ) +
  labs(
    title = "Jayden Daniels — Weekly EPA/Play vs League Mean (2024 REG)",
    subtitle = paste0("Dotted = Player avg (", round(jw_avg_epa, 3),
                      "), Dot-dash = League weekly mean"),
    x = "Week",
    y = "EPA/Play",
    size = "Dropbacks"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.box = "horizontal")
















# ---- Plot ----
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







# GAME PLAY BY PLAY

# ---- Plot: Play-by-Play EPA (Week 6) ----
ggplot(caleb_pbp_week_1, aes(x = play_index, y = epa)) +
  # Points per play (size by |EPA|, shape by play type)
  geom_point(aes(size = abs(epa), shape = play_cat), alpha = 0.9) +
  # Rolling EPA line to show trend
  geom_line(aes(y = epa_roll5), linewidth = 1, alpha = 0.8) +
  # Horizontal reference lines
  geom_hline(yintercept = caleb_game_avg_epa, linetype = "dotted", alpha = 0.8) +
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
    title = "C.Williams — Play-by-Play EPA (Week 1)",
    subtitle = paste0("Opponent: ", caleb_opp, 
                      "   ·   Dotted = Game avg (", round(caleb_game_avg_epa, 3), 
                      "),  Dot-dash = 0  ·  Rolling line = 5-play mean"),
    x = "Play Sequence (Week 1)",
    y = "EPA",
    shape = "Play Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
























