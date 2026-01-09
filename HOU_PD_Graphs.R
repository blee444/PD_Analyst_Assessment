# ================================
# ROLLING VISUALS
# ================================

# Function: Create rolling statistics graphs for a pitcher
plot_rolling_stats <- function(pitcher_id, events, pitches, window = 1) {
  
  # Load required library
  if (!require(ggplot2)) {
    stop("ggplot2 package is required. Install with: install.packages('ggplot2')")
  }
  
  # Filter data for the specific pitcher
  pitcher_events  <- events[events$pitcher_id == pitcher_id, ]
  pitcher_pitches <- pitches[pitches$pitcher_id == pitcher_id, ]
  
  if (nrow(pitcher_events) == 0) {
    stop("No events found for pitcher_id: ", pitcher_id)
  }
  
  # Get unique game dates and sort them
  dates <- sort(unique(pitcher_events$sched_date))
  
  if (length(dates) < window) {
    stop(
      "Not enough appearances for rolling statistics. Need at least ",
      window, " appearances, found ", length(dates)
    )
  }
  
  # Initialize results data frame
  rolling_stats <- data.frame(
    date = dates,
    appearance_num = seq_along(dates),
    BB_pct  = NA,
    K_pct   = NA,
    SLG     = NA,
    FPS_pct = NA
  )
  
  # Calculate rolling stats
  for (i in window:length(dates)) {
    
    # Rolling window indices
    start_idx <- i - window + 1
    end_idx   <- i
    window_dates <- dates[start_idx:end_idx]
    
    # Filter rolling window data
    events_window  <- pitcher_events[pitcher_events$sched_date %in% window_dates, ]
    pitches_window <- pitcher_pitches[pitcher_pitches$sched_date %in% window_dates, ]
    
    # Total PA
    total_pa <- sum(events_window$pa, na.rm = TRUE)
    
    if (total_pa > 0) {
      rolling_stats$BB_pct[i]  <- calculate_bb_pct(events_window, total_pa)
      rolling_stats$K_pct[i]   <- calculate_k_pct(events_window, total_pa)
      rolling_stats$SLG[i]     <- calculate_slg(events_window)
      rolling_stats$FPS_pct[i] <- calculate_fps_pct(pitches_window)
    }
  }
  
  # Remove rows before window is filled
  rolling_stats <- rolling_stats[
    !is.na(rolling_stats$BB_pct) |
      !is.na(rolling_stats$K_pct)  |
      !is.na(rolling_stats$SLG)    |
      !is.na(rolling_stats$FPS_pct),
  ]
  
  if (nrow(rolling_stats) == 0) {
    stop("No valid rolling statistics could be calculated")
  }
  
  # ======================
  # Date range logic
  # ======================
  
  # Ensure Date class (important safeguard)
  rolling_stats$date <- as.Date(rolling_stats$date)
  
  if (nrow(rolling_stats) == 1) {
    
    # Single-appearance case → zoom to 2025 if possible
    plot_dates <- rolling_stats$date[
      format(rolling_stats$date, "%Y") == "2025"
    ]
    
    if (length(plot_dates) > 0) {
      xlim_dates <- as.Date(c("2025-01-01", "2025-12-31"))
    } else {
      # Fallback: small buffer around the single date
      xlim_dates <- rolling_stats$date + c(-7, 7)
    }
    
  } else {
    # Normal case → full range
    xlim_dates <- range(rolling_stats$date, na.rm = TRUE)
  }
  
  # ======================
  # Plotting
  # ======================
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  
  # BB%
  plot(
    rolling_stats$date, rolling_stats$BB_pct,
    type = "l", lwd = 2, col = "red",
    xlab = "Date", ylab = "BB%",
    main = paste0("Rolling ", window, "-Appearance BB%"),
    las = 1,
    xlim = xlim_dates
  )
  points(rolling_stats$date, rolling_stats$BB_pct, pch = 19, col = "red", cex = 0.7)
  grid()
  abline(h = mean(rolling_stats$BB_pct, na.rm = TRUE),
         lty = 2, col = "darkred", lwd = 1.5)
  
  # K%
  plot(
    rolling_stats$date, rolling_stats$K_pct,
    type = "l", lwd = 2, col = "blue",
    xlab = "Date", ylab = "K%",
    main = paste0("Rolling ", window, "-Appearance K%"),
    las = 1,
    xlim = xlim_dates
  )
  points(rolling_stats$date, rolling_stats$K_pct, pch = 19, col = "blue", cex = 0.7)
  grid()
  abline(h = mean(rolling_stats$K_pct, na.rm = TRUE),
         lty = 2, col = "darkblue", lwd = 1.5)
  
  # SLG
  plot(
    rolling_stats$date, rolling_stats$SLG,
    type = "l", lwd = 2, col = "darkgreen",
    xlab = "Date", ylab = "SLG",
    main = paste0("Rolling ", window, "-Appearance SLG"),
    las = 1,
    xlim = xlim_dates
  )
  points(rolling_stats$date, rolling_stats$SLG, pch = 19, col = "darkgreen", cex = 0.7)
  grid()
  abline(h = mean(rolling_stats$SLG, na.rm = TRUE),
         lty = 2, col = "darkgreen", lwd = 1.5)
  
  # FPS%
  plot(
    rolling_stats$date, rolling_stats$FPS_pct,
    type = "l", lwd = 2, col = "purple",
    xlab = "Date", ylab = "FPS%",
    main = paste0("Rolling ", window, "-Appearance FPS%"),
    las = 1,
    xlim = xlim_dates
  )
  points(rolling_stats$date, rolling_stats$FPS_pct, pch = 19, col = "purple", cex = 0.7)
  grid()
  abline(h = mean(rolling_stats$FPS_pct, na.rm = TRUE),
         lty = 2, col = "purple", lwd = 1.5)
  
  par(mfrow = c(1, 1))
  
  invisible(rolling_stats)
}
