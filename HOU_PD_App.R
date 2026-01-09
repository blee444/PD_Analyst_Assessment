library(shiny)
library(dplyr)
library(tidyverse)
library(readxl)
library(data.table)
library(stats)
library(ggplot2)
library(DBI)

# ================================
# SOURCE HELPERS AND DATA
# ================================

pitches <- read.csv("/Users/brianlee/Downloads/pd_analyst_pitches.csv")
events <- read.csv("/Users/brianlee/Downloads/pd_analyst_events.csv")
goals <- read.csv("/Users/brianlee/Downloads/pd_analyst_goals.csv")

pitches$sched_date <- as.Date(pitches$sched_date, format = "%m/%d/%Y") 
events$sched_date <- as.Date(events$sched_date, format = "%m/%d/%Y") 

source("~/Downloads/HOU/HOU_PD_Graphs.R")
source("~/Downloads/HOU/HOU_PD_Splits.R")
source("~/Downloads/HOU/HOU_PD_Calcs.R")
source("~/Downloads/HOU/HOU_PD_Master_Function.R")
source("~/Downloads/HOU/HOU_PD_Goals.R")
source("~/Downloads/HOU/HOU_PD_Goal_Checks.R")
source("~/Downloads/HOU/HOU_PD_PDF.R") 


# ================================
# UI
# ================================

ui <- fluidPage(
  
  titlePanel("Pitcher Development Report"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        inputId = "pitcher_id",
        label = "Select Pitcher",
        choices = NULL
      ),
      actionButton(
        inputId = "run_report",
        label = "Run Pitcher Report",
        class = "btn-primary"
      ),
      br(), br(),
      downloadButton(
        outputId = "download_pdf",
        label = "Export Report as PDF",
        class = "btn-success"
      )
    ),
    
    mainPanel(
      h3("Player Goals"),
      verbatimTextOutput("goals_out"),
      
      h3("Season Stats"),
      tableOutput("season_table"),
      
      h3("Last Two Weeks Stats"),
      tableOutput("l2w_table"),
      
      h3("Goal Comparison Table (2-Week Stats)"),
      tableOutput("goal_comparison_table"),
      
      h3("Rolling Performance Trends (Season)"),
      plotOutput("rolling_plots", height = "600px"),
      
      h3("Goal Progress Plots (Season vs Targets)"),
      plotOutput("goal_plots", height = "400px")
    )
  )
)

# ================================
# SERVER
# ================================

server <- function(input, output, session) {
  
  # Populate pitcher dropdown from events
  observe({
    pitcher_choices <- sort(unique(events$pitcher_id))
    
    updateSelectInput(
      session,
      "pitcher_id",
      choices  = pitcher_choices,
      selected = pitcher_choices[1]
    )
  })
  
  # Reactive: main report data
  report_data <- eventReactive(input$run_report, {
    season <- get_season_stats(input$pitcher_id, events, pitches)
    l2w    <- get_last_two_weeks_stats(input$pitcher_id, events, pitches)
    comparison <- compare_pitcher_stats(season, l2w)
    
    # Goal comparison
    goals_info <- plot_goal_comparisons(
      player_id = input$pitcher_id,
      events = events,
      pitches = pitches,
      goals = goals
    )
    
    list(
      season = season,
      l2w = l2w,
      comparison = comparison,
      goals_info = goals_info
    )
  })
  
  # =========================
  # Render outputs
  # =========================
  
  output$goals_out <- renderPrint({
    req(input$run_report)
    display_player_goals(input$pitcher_id, goals)
  })
  
  output$season_table <- renderTable({
    req(report_data())
    data <- report_data()$season
    
    # Format the display
    data |>
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
  }, spacing = "m", width = "auto")
  
  output$l2w_table <- renderTable({
    req(report_data())
    data <- report_data()$l2w
    
    # Format the display
    data |>
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
  }, spacing = "m", width = "auto")
  
  output$goal_comparison_table <- renderTable({
    req(report_data())
    report_data()$goals_info$results
  })
  
  # Rolling stats plots
  output$rolling_plots <- renderPlot({
    req(input$run_report)
    plot_rolling_stats(
      pitcher_id = input$pitcher_id,
      events = events,
      pitches = pitches,
      window = 1
    )
  })
  
  # Goal comparison plots
  output$goal_plots <- renderPlot({
    req(input$run_report)
    goals_info <- report_data()$goals_info
    
    par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
    
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
    
    par(mfrow = c(1, 1))
  })
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("pitcher_report_", input$pitcher_id, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      generate_pitcher_pdf(
        file = file,
        pitcher_id = input$pitcher_id,
        report_data = report_data(),
        events = events,
        pitches = pitches,
        goals = goals
      )
    }
  )
}

# ================================
# RUN APP
# ================================

shinyApp(ui = ui, server = server)