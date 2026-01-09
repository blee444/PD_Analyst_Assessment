# PD Analyst Assessment — Pitcher Development Report
This repository hosts a Shiny app that generates pitcher development information. 

## Features

* The interface, for coach use, demonstrates player goals, comparisons to current performance (season and last 2 weeks) and rolling performance/goal trends.
* KPI stats include BB%, K%, FPS%, and SLG.
* PDF export functionality provides a more abridged, player-facing version for wider use among personnel.

## Project Structure

* app.R - Main Shiny application
* Data - CSV files for pitch, event, and goal tracking
* Helpers - Supporting R scripts for calculations of statistics, plot generation, and PDF export functionality
* README.md - Project documentation
* rsconnect - Online uploaded version on shinyapps.io

## Requirements/Instructions

* Install required packages in R:
install.packages(c(
  "shiny", "dplyr", "tidyverse", "readxl", 
  "data.table", "stats", "ggplot2", "DBI", "here"
))

* In R or RStudio, navigate to the project root and run:
shiny::runApp()

## Performance considerations

* Given the smaller sample sizes of many pitchers, a 1-appearance rolling window was used as a baseline for performance tracking to balance sample size with larger trend adjustment.
* Static plots were used to improve performance, updating only when new pitcher_ids were selected and rendered.

## Example workflows

* Postgame after-action reports
* Coaches and managers can utilize this data to assess player performance postgame and understand personnel strengths/weaknesses on a game-to-game basis.

* Monthly progress check-ins
* PD staff can touch base with players on recent performance to alter practice/drill focus based on which milestones they have reached, and which they need to focus more on.

* Breakout performance identification
* Outlier performance, both good and bad, can be monitored using this app. In turn, such flagging can be a factor in considering player promotion/demotion.
