# ================================
# METRIC HELPERS
# ================================

calculate_bb_pct <- function(events_df, total_pa) {
  if (total_pa == 0) return(NA_real_)
  sum(events_df$bb, na.rm = TRUE) / total_pa * 100
}

calculate_k_pct <- function(events_df, total_pa) {
  if (total_pa == 0) return(NA_real_)
  sum(events_df$so, na.rm = TRUE) / total_pa * 100
}

calculate_slg <- function(events_df) {
  total_ab <- sum(events_df$ab, na.rm = TRUE)
  if (total_ab == 0) return(NA_real_)
  
  total_bases <-
    sum(events_df$X1b, na.rm = TRUE) +
    2 * sum(events_df$X2b, na.rm = TRUE) +
    3 * sum(events_df$X3b, na.rm = TRUE) +
    4 * sum(events_df$hr,  na.rm = TRUE)
  
  total_bases / total_ab
}

calculate_fps_pct <- function(pitches_df) {
  first_pitches <- pitches_df[
    pitches_df$balls_before == 0 &
      pitches_df$strikes_before == 0, ]
  
  if (nrow(first_pitches) == 0) return(NA_real_)
  
  strikes <- sum(!(first_pitches$pitch_result %in%
                     c("ball", "blocked_ball")), na.rm = TRUE)
  
  strikes / nrow(first_pitches) * 100
}
