run_pitcher_report <- function(pitcher_id,
                               events,
                               pitches,
                               goals,
                               rolling_window = 5) {
  
  display_player_goals(pitcher_id, goals)
  
  season <- get_season_stats(pitcher_id, events, pitches)
  l2w    <- get_last_two_weeks_stats(pitcher_id, events, pitches)
  comp   <- compare_pitcher_stats(season, l2w)
  
  cat("\nSEASON STATS\n------------------\n")
  print(season)
  
  cat("\nLAST 2 WEEKS\n------------------\n")
  print(l2w)
  
  cat("\nPROGRESS VS SEASON\n------------------\n")
  print(comp)
  
  plot_rolling_stats(pitcher_id, events, pitches, rolling_window)
  
  invisible(list(
    season = season,
    last_two_weeks = l2w,
    comparison = comp
  ))
}
