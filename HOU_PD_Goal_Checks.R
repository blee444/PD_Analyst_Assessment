# ================================
# GOALS HELPER: 2-WEEKS & SEASON
# ================================

plot_and_compare_goals <- function(player_id, events, pitches, goals, season_stats) {
  
  player_goals <- goals[goals$player_id == player_id, ]
  if (nrow(player_goals) == 0) stop("No goals found for player_id: ", player_id)
  player_goals <- player_goals[1, ]
  
  # ----------------------
  # Get last 2 weeks stats
  # ----------------------
  l2w_stats <- get_last_two_weeks_stats(player_id, events, pitches)
  if (is.null(l2w_stats) || l2w_stats$PA == 0) stop("No last-two-weeks stats available")
  
  # ----------------------
  # Helper: parse goal text
  # ----------------------
  parse_goal <- function(goal_text) {
    if (is.na(goal_text) || goal_text == "") return(list(metric=NA, target=NA, direction=NA))
    
    metric <- if (grepl("BB%", goal_text)) "BB%" 
    else if (grepl("K%", goal_text)) "K%" 
    else if (grepl("FPinZ%", goal_text) || grepl("FPS%", goal_text)) "FPS%" 
    else NA
    
    target <- as.numeric(gsub(".*?(\\d+)%.*", "\\1", goal_text))
    
    direction <- if (grepl("Decrease|less", goal_text)) "decrease"
    else if (grepl("Increase|more", goal_text)) "increase"
    else NA
    
    list(metric=metric, target=target, direction=direction)
  }
  
  primary   <- parse_goal(player_goals$Primary.Goal)
  secondary <- parse_goal(player_goals$Secondary.Goal)
  tertiary  <- parse_goal(player_goals$Tertiary.Goal)
  
  # ----------------------
  # Helper: calculate goal result (2-week table)
  # ----------------------
  calc_goal_result <- function(goal_level, parsed_goal) {
    if (is.na(parsed_goal$metric)) return(NULL)
    
    current_value <- switch(parsed_goal$metric,
                            "BB%" = l2w_stats$BB_pct,
                            "K%"  = l2w_stats$K_pct,
                            "FPS%"= l2w_stats$FPS_pct,
                            NA)
    if (is.na(current_value)) return(NULL)
    
    if (parsed_goal$direction == "decrease") {
      difference <- parsed_goal$target - current_value
      status <- ifelse(current_value <= parsed_goal$target, "On Track", "Not Yet")
    } else if (parsed_goal$direction == "increase") {
      difference <- current_value - parsed_goal$target
      status <- ifelse(current_value >= parsed_goal$target, "On Track", "Not Yet")
    } else {
      difference <- NA
      status <- "Unknown"
    }
    
    data.frame(
      Goal_Level = goal_level,
      Metric     = parsed_goal$metric,
      Target     = parsed_goal$target,
      Current    = round(current_value, 2),
      Difference = round(difference, 2),
      Status     = status,
      stringsAsFactors = FALSE
    )
  }
  
  # ----------------------
  # 2-Week Comparison Table
  # ----------------------
  goal_table <- rbind(
    calc_goal_result("Primary", primary),
    calc_goal_result("Secondary", secondary),
    calc_goal_result("Tertiary", tertiary)
  )
  
  # ----------------------
  # Season-long Progress Plots
  # ----------------------
  plot_goal_progress <- function(season_stats, goals_list) {
    par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
    
    for (goal in goals_list) {
      if (is.na(goal$metric)) next
      
      metric_vals <- switch(goal$metric,
                            "BB%" = season_stats$BB_pct,
                            "K%"  = season_stats$K_pct,
                            "FPS%"= season_stats$FPS_pct,
                            rep(NA, nrow(season_stats)))
      
      dates <- season_stats$date
      
      plot(dates, metric_vals, type="l", lwd=2,
           col=ifelse(goal$direction=="increase","blue","red"),
           xlab="Date", ylab=goal$metric)
      
      points(dates, metric_vals, pch=19,
             col=ifelse(goal$direction=="increase","blue","red"), cex=0.7)
      
      abline(h=goal$target, lty=2, col="gray", lwd=2)
    }
    
    par(mfrow=c(1,1))
  }
  
  # ----------------------
  # Return results
  # ----------------------
  list(
    table_2weeks = goal_table,
    season_plot = function() plot_goal_progress(season_stats, list(primary, secondary, tertiary)),
    l2w_stats = l2w_stats,
    primary = primary,
    secondary = secondary,
    tertiary = tertiary
  )
}

# ================================
# GOAL COMPARISON FUNCTION (FOR SHINY APP)
# ================================

plot_goal_comparisons <- function(player_id, events, pitches, goals) {
  
  # Get player's goals (NOTE: using player_id, not pitcher_id)
  player_goals <- goals[goals$player_id == player_id, ]
  
  if (nrow(player_goals) == 0) {
    return(list(
      results = data.frame(
        Goal_Level = "No goals set",
        Metric = NA,
        Target = NA,
        Current = NA,
        Difference = NA,
        Status = NA
      ),
      primary = list(metric = NA, target = NA, direction = NA),
      secondary = list(metric = NA, target = NA, direction = NA),
      tertiary = list(metric = NA, target = NA, direction = NA),
      l2w_stats = NULL
    ))
  }
  
  player_goals <- player_goals[1, ]
  
  # Calculate last 2 weeks stats
  l2w_stats <- get_last_two_weeks_stats(player_id, events, pitches)
  
  if (is.null(l2w_stats) || l2w_stats$PA == 0) {
    return(list(
      results = data.frame(
        Goal_Level = "No recent stats",
        Metric = NA,
        Target = NA,
        Current = NA,
        Difference = NA,
        Status = NA
      ),
      primary = list(metric = NA, target = NA, direction = NA),
      secondary = list(metric = NA, target = NA, direction = NA),
      tertiary = list(metric = NA, target = NA, direction = NA),
      l2w_stats = l2w_stats
    ))
  }
  
  # ----------------------
  # Helper: parse goal text
  # ----------------------
  parse_goal <- function(goal_text) {
    if (is.na(goal_text) || goal_text == "") return(list(metric=NA, target=NA, direction=NA))
    
    metric <- if (grepl("BB%", goal_text)) "BB%" 
    else if (grepl("K%", goal_text)) "K%" 
    else if (grepl("FPinZ%", goal_text) || grepl("FPS%", goal_text)) "FPS%" 
    else NA
    
    target <- as.numeric(gsub(".*?(\\d+)%.*", "\\1", goal_text))
    
    direction <- if (grepl("Decrease|less", goal_text)) "decrease"
    else if (grepl("Increase|more", goal_text)) "increase"
    else NA
    
    list(metric=metric, target=target, direction=direction)
  }
  
  primary   <- parse_goal(player_goals$Primary.Goal)
  secondary <- parse_goal(player_goals$Secondary.Goal)
  tertiary  <- parse_goal(player_goals$Tertiary.Goal)
  
  # ----------------------
  # Helper: calculate goal result
  # ----------------------
  calc_goal_result <- function(goal_level, parsed_goal) {
    if (is.na(parsed_goal$metric)) return(NULL)
    
    current_value <- switch(parsed_goal$metric,
                            "BB%" = l2w_stats$BB_pct,
                            "K%"  = l2w_stats$K_pct,
                            "FPS%"= l2w_stats$FPS_pct,
                            NA)
    if (is.na(current_value)) return(NULL)
    
    if (parsed_goal$direction == "decrease") {
      difference <- parsed_goal$target - current_value
      status <- ifelse(current_value <= parsed_goal$target, "On Track", "Not Yet")
    } else if (parsed_goal$direction == "increase") {
      difference <- current_value - parsed_goal$target
      status <- ifelse(current_value >= parsed_goal$target, "On Track", "Not Yet")
    } else {
      difference <- NA
      status <- "Unknown"
    }
    
    data.frame(
      Goal_Level = goal_level,
      Metric     = parsed_goal$metric,
      Target     = paste0(parsed_goal$target, "%"),
      Current    = paste0(round(current_value, 1), "%"),
      Difference = paste0(ifelse(difference >= 0, "+", ""), round(difference, 1), "%"),
      Status     = status,
      stringsAsFactors = FALSE
    )
  }
  
  # ----------------------
  # Build results table
  # ----------------------
  results <- rbind(
    calc_goal_result("Primary", primary),
    calc_goal_result("Secondary", secondary),
    calc_goal_result("Tertiary", tertiary)
  )
  
  if (is.null(results) || nrow(results) == 0) {
    results <- data.frame(
      Goal_Level = "No valid goals",
      Metric = NA,
      Target = NA,
      Current = NA,
      Difference = NA,
      Status = NA
    )
  }
  
  return(list(
    results = results,
    primary = primary,
    secondary = secondary,
    tertiary = tertiary,
    l2w_stats = l2w_stats
  ))
}

