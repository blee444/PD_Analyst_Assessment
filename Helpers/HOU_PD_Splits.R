# ================================
# L2W AND SEASON STATS
# ================================

get_season_stats <- function(pitcher_id, events, pitches) {
  ev <- events[events$pitcher_id == pitcher_id, ]
  pt <- pitches[pitches$pitcher_id == pitcher_id, ]
  
  total_pa <- sum(ev$pa, na.rm = TRUE)
  
  tibble::tibble(
    PA = total_pa,
    BB_pct = round(calculate_bb_pct(ev, total_pa), 2),
    K_pct  = round(calculate_k_pct(ev, total_pa), 2),
    SLG    = round(calculate_slg(ev), 3),
    FPS_pct = round(calculate_fps_pct(pt), 2)
  )
}

get_last_two_weeks_stats <- function(pitcher_id, events, pitches) {
  ev <- events[events$pitcher_id == pitcher_id, ]
  pt <- pitches[pitches$pitcher_id == pitcher_id, ]
  
  most_recent <- max(ev$sched_date, na.rm = TRUE)
  start_date <- most_recent - 14
  
  ev_l2w <- ev[ev$sched_date >= start_date, ]
  pt_l2w <- pt[pt$sched_date >= start_date, ]
  
  total_pa <- sum(ev_l2w$pa, na.rm = TRUE)
  
  tibble::tibble(
    Last_Appearance = as.character(most_recent),
    PA = total_pa,
    BB_pct = round(calculate_bb_pct(ev_l2w, total_pa), 2),
    K_pct  = round(calculate_k_pct(ev_l2w, total_pa), 2),
    SLG    = round(calculate_slg(ev_l2w), 3),
    FPS_pct = round(calculate_fps_pct(pt_l2w), 2)
  )
}

compare_pitcher_stats <- function(season, l2w) {
  metrics <- c("BB_pct","K_pct","SLG","FPS_pct")
  
  comp <- tibble::tibble(
    Metric = c("BB%","K%","SLG","FPS%"),
    Season = unlist(season[metrics]),
    Last_2_Weeks = unlist(l2w[metrics])
  )
  
  comp <- comp |>
    dplyr::mutate(
      Difference = ifelse(Metric == "PA",
                          Season - Last_2_Weeks,
                          Last_2_Weeks - Season),
      Trend = dplyr::case_when(
        Metric %in% c("K%","FPS%") & Difference > 0 ~ "Improving",
        Metric %in% c("BB%","SLG") & Difference < 0 ~ "Improving",
        Difference == 0 ~ "Stable",
        TRUE ~ "Declining"
      )
    )
  
  comp
}
