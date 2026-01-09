# HOU_PD_PDF_Export.R
# Helper function for generating pitcher development PDF reports

generate_pitcher_pdf <- function(file, pitcher_id, report_data, events, pitches, goals) {
  
  # Create PDF
  pdf(file, width = 11, height = 8.5)
  
  # Title Page
  plot.new()
  text(0.5, 0.9, "Pitcher Development Report", cex = 2, font = 2)
  text(0.5, 0.8, paste("Pitcher ID:", pitcher_id), cex = 1.5)
  text(0.5, 0.7, paste("Report Date:", Sys.Date()), cex = 1.2)
  
  # Goals and Stats Tables
  plot.new()
  text(0.5, 0.95, "Player Goals and Statistics", cex = 1.5, font = 2)
  
  # Capture goals as text
  goals_text <- capture.output(display_player_goals(pitcher_id, goals))
  text(0.1, 0.88, paste(goals_text, collapse = "\n"), adj = c(0, 1), cex = 0.7, family = "mono")
  
  # Season stats table
  text(0.05, 0.70, "Season Stats:", cex = 1.2, font = 2, adj = 0)
  season_formatted <- report_data$season |>
    dplyr::mutate(
      BB_pct = paste0(sprintf("%.1f", BB_pct), "%"),
      K_pct = paste0(sprintf("%.1f", K_pct), "%"),
      SLG = sub("^0", "", sprintf("%.3f", SLG)),
      FPS_pct = paste0(sprintf("%.1f", FPS_pct), "%")
    ) |>
    dplyr::rename(
      `BB%` = BB_pct,
      `K%` = K_pct,
      `FPS%` = FPS_pct
    )
  
  # Convert to matrix for display
  season_matrix <- as.matrix(season_formatted)
  y_pos <- 0.65
  x_start <- 0.05
  col_width <- 0.12
  
  # Print column headers
  for (i in 1:ncol(season_matrix)) {
    text(x_start + (i-1) * col_width, y_pos, colnames(season_matrix)[i], 
         adj = 0, cex = 0.8, font = 2)
  }
  
  # Print data rows
  for (row in 1:nrow(season_matrix)) {
    y_pos <- y_pos - 0.03
    for (col in 1:ncol(season_matrix)) {
      text(x_start + (col-1) * col_width, y_pos, season_matrix[row, col], 
           adj = 0, cex = 0.7, family = "mono")
    }
  }
  
  # Last Two Weeks stats table
  text(0.05, 0.50, "Last Two Weeks Stats:", cex = 1.2, font = 2, adj = 0)
  l2w_formatted <- report_data$l2w |>
    dplyr::mutate(
      BB_pct = paste0(sprintf("%.1f", BB_pct), "%"),
      K_pct = paste0(sprintf("%.1f", K_pct), "%"),
      SLG = sub("^0", "", sprintf("%.3f", SLG)),
      FPS_pct = paste0(sprintf("%.1f", FPS_pct), "%")
    ) |>
    dplyr::rename(
      `Latest` = Last_Appearance,
      `BB%` = BB_pct,
      `K%` = K_pct,
      `FPS%` = FPS_pct
    )
  
  # Convert to matrix for display
  l2w_matrix <- as.matrix(l2w_formatted)
  y_pos <- 0.45
  x_start <- 0.05
  col_width <- 0.12
  
  # Print column headers
  for (i in 1:ncol(l2w_matrix)) {
    text(x_start + (i-1) * col_width, y_pos, colnames(l2w_matrix)[i], 
         adj = 0, cex = 0.8, font = 2)
  }
  
  # Print data rows
  for (row in 1:nrow(l2w_matrix)) {
    y_pos <- y_pos - 0.03
    for (col in 1:ncol(l2w_matrix)) {
      text(x_start + (col-1) * col_width, y_pos, l2w_matrix[row, col], 
           adj = 0, cex = 0.7, family = "mono")
    }
  }
  
  # Goal comparison table
  text(0.05, 0.30, "Goal Comparison Table:", cex = 1.2, font = 2, adj = 0)
  goal_comp <- report_data$goals_info$results
  if (!is.null(goal_comp) && nrow(goal_comp) > 0) {
    goal_matrix <- as.matrix(goal_comp)
    y_pos <- 0.25
    x_start <- 0.05
    col_width <- 0.15
    
    # Print column headers
    for (i in 1:ncol(goal_matrix)) {
      text(x_start + (i-1) * col_width, y_pos, colnames(goal_matrix)[i], 
           adj = 0, cex = 0.8, font = 2)
    }
    
    # Print data rows
    for (row in 1:nrow(goal_matrix)) {
      y_pos <- y_pos - 0.03
      for (col in 1:ncol(goal_matrix)) {
        text(x_start + (col-1) * col_width, y_pos, goal_matrix[row, col], 
             adj = 0, cex = 0.7, family = "mono")
      }
    }
  }
  
  # Goal Progress Plots
  
  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
  goals_info <- report_data$goals_info
  
  for (goal in list(goals_info$primary, goals_info$secondary, goals_info$tertiary)) {
    if (is.na(goal$metric)) next
    
    current_val <- switch(goal$metric,
                          "BB%" = goals_info$l2w_stats$BB_pct,
                          "K%"  = goals_info$l2w_stats$K_pct,
                          "FPS%"= goals_info$l2w_stats$FPS_pct)
    
    target_val <- goal$target
    color <- ifelse(goal$direction == "increase", "blue", "red")
    
    barplot(
      height = c(current_val, target_val),
      names.arg = c("Current", "Target"),
      col = c(color, "gray"),
      main = paste(goal$metric, "-", toupper(goal$direction)),
      ylab = goal$metric,
      ylim = c(0, max(current_val, target_val) * 1.2)
    )
    abline(h = target_val, lty = 2, col = "black", lwd = 1.5)
  }
  
  dev.off()
}