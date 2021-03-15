library(shiny)
library(tidyverse)
library(hexbin)
library(httr)
library(jsonlite)

source("helpers.R")
source("court_themes.R")
source("plot_court.R")
source("players_data.R")
source("fetch_shots.R")
source("hex_chart.R")
source("scatter_chart.R")
source("heatmap_chart.R")

shinyUI(
  fixedPage(
    theme = "flatly.css",
    title = "NBA Shot Charts",

    tags$head(
      tags$link(rel = "apple-touch-icon", href = "logo.png"),
      tags$link(rel = "icon", href = "logo.png"),
      tags$link(rel = "stylesheet", type = "text/css", href = "shared/selectize/css/selectize.bootstrap3.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.10.0/css/bootstrap-select.min.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.10.0/js/bootstrap-select.min.js"),
      tags$script(src = "shared/selectize/js/selectize.min.js"),
      tags$script(src = "ballr.js"),
      uiOutput("shot_chart_css"),
      includeScript("www/google-analytics.js")
    ),

    fixedRow(class = "app-header",
      div(class = "container",
        fixedRow(class = "header-content",
          div(class = "col-xs-3 col-md-3",
            radioButtons(inputId = "view_type",
                       label = "",
                       choices = c("Individual", "Comparsion"),
                       selected = "Individual"),
            
            selectInput(inputId = "player_name",
                      label = "",
                      choices = c("Enter a player..." = "", available_players$name),
                      selected = default_player$name,
                      selectize = FALSE),
            uiOutput("compare_players")
          ),
          div(class = "col-xs-3 col-md-3",            

              dateRangeInput(inputId = "date_range",
                             label = "Date range",
                             start = "2020-01-01",
                             end = "2021-03-12"),

              radioButtons(inputId = "season_type",
                           label = "Season Type",
                           choices = c("Regular Season", "Playoffs"),
                           selected = default_season_type),
              # uiOutput("datetime_range_slider"),
              div(class = "reg_season_select", 
                selectInput(inputId = "season",
                            label = "Reg Season",
                            choices = rev(default_seasons),
                            selected = default_season,
                            selectize = FALSE)
              ),

              radioButtons(inputId = "last_ten_gms",
                           label = "Last 10 Gms",
                           choices = c("All", "Last 10 Gms"),
                           selected = "All"),

              uiOutput("hex_metric_buttons"),
              uiOutput("hexbinwidth_slider"),
              uiOutput("hex_radius_slider"),

              uiOutput("scatter_size_slider"),
              uiOutput("scatter_alpha_slider"),
          ),
          div(class = "col-xs-3 col-md-3",
            selectInput(inputId = "shot_zone_basic_filter",
                        label = "",
                        choices = c("Above the Break 3",
                                    "Left Corner 3",
                                    "Right Corner 3",
                                    "Mid-Range",
                                    "In The Paint (Non-RA)",
                                    "Restricted Area"),
                        multiple = TRUE,
                        selectize = FALSE),

            selectInput(inputId = "shot_zone_angle_filter",
                        label = "",
                        choices = c("Left Side" = "Left Side(L)",
                                    "Left Center" = "Left Side Center(LC)",
                                    "Center" = "Center(C)",
                                    "Right Center" = "Right Side Center(RC)",
                                    "Right Side" = "Right Side(R)"),
                        multiple = TRUE,
                        selectize = FALSE),

            selectInput(inputId = "shot_distance_filter",
                        label = "",
                        choices = c("0-8 ft" = "Less Than 8 ft.",
                                    "8-16 ft" = "8-16 ft.",
                                    "16-24 ft" = "16-24 ft.",
                                    "24+ ft" = "24+ ft."),
                        multiple = TRUE,
                        selectize = FALSE),

            selectInput(inputId = "shot_result_filter",
                        label = "",
                        choices = c("All" = "all", "Made" = "made", "Missed" = "missed"),
                        selected = "all",
                        selectize = FALSE)
          ),
          div(class = "col-xs-1 col-md-1",
          ),
          div(class = "col-xs-2 col-md-2 chart-type",
            radioButtons(inputId = "chart_type",
                       label = "",
                       choices = c("Hexagonal", "Density"),
                       selected = "Hexagonal"),
          )
        )
      )
    ),

    fixedRow(class = "primary-content",
      div(class = "col-sm-12 col-md-12 shot-chart-body",
        fixedPage(class = "filter-info", 
          uiOutput("shot_filters_applied")
        ),
        fixedRow(class = "player-content",
          div(class = "col-sm-12 col-md-12",
            fixedRow(class = "shot-chart-header",
              div(class = "col-sm-2 col-md-2", 
                uiOutput("player_photo")
              ),
              div(class = "col-sm-4 col-md-4", 
                h3(textOutput("chart_header_player")),
                h4(textOutput("chart_header_team"))
              ),
              div(class = "col-sm-6 col-md-6",                
              )
            ),
            fixedRow(class = "shot-chart-content",
              div(class = "col-sm-8 col-md-8",
                div(class = "shot-chart-container",

                  plotOutput("court", width = 800, height = "auto"),
                ),
                div(class = "download-link-container",
                  uiOutput("download_link")
                )
              ),
              div(class= "col-sm-4 col-md-4 summary-stats-container",
                uiOutput("summary_stats")
              )
            ),
            fixedRow(class = "app-footer",
              HTML('<img src="logo.png" />'),
              HTML('<div></div>'),
              HTML('<img src="logo.png" />')
            )
          )
        ),
        uiOutput('compare_players_content'),
      )
    )
  )
)
