# ===== Normalize player names to "F.Lastname" and handle edge cases =====

# Helper to convert "First Last" -> "F.Last"
normalize_name_vec <- function(x) {
  x %>%
    str_replace("^([A-Za-z]+)\\s+([A-Za-z'\\-]+)$", "\\1.\\2") %>% # First Last -> First.Last
    str_replace("^(.).*?\\.(.*)$", "\\1.\\2")                     # First.Last -> F.Last
}

# Season-level PFR adv
qb_adv_25 <- qb_adv_25 %>%
  mutate(
    name = normalize_name_vec(player),
    name = case_when(
      player == "Gardner Minshew II" ~ "G.Minshew II",
      str_detect(player, regex("^C\\.?\\s*J\\.?\\s*Stroud$", ignore_case = FALSE)) ~ "C.Stroud",
      TRUE ~ name
    )
  )

# Weekly PFR adv
qb_adv_weekly_25 <- qb_adv_weekly_25 %>%
  mutate(
    name = normalize_name_vec(pfr_player_name),
    name = case_when(
      pfr_player_name == "Gardner Minshew II" ~ "G.Minshew II",
      str_detect(pfr_player_name, regex("^C\\.?\\s*J\\.?\\s*Stroud$", ignore_case = FALSE)) ~ "C.Stroud",
      TRUE ~ name
    )
  )
