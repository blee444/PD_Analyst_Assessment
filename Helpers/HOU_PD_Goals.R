display_player_goals <- function(player_id, goals) {
  g <- goals[goals$player_id == player_id, ][1, ]
  
  cat("Primary:  ", g$Primary.Goal, "\n")
  cat("Secondary:", g$Secondary.Goal, "\n")
  cat("Tertiary: ", g$Tertiary.Goal, "\n\n")
  
  invisible(g)
}
