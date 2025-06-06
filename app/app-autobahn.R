# app.R

library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(stringr)

# 1) Load delay data once at app‐startup
delay_data_filtered_by_length <- read_csv("./app_data/data_plotting_app.csv") %>% 
  mutate(averageSpeed = round(averageSpeed, digits = 2))

ui <- fluidPage(
  titlePanel("Delays on German highways"),
  h5(paste0(delay_data_filtered_by_length$startTimestamp %>% min() %>% format(., "%d.%m.%Y"), 
            "-", delay_data_filtered_by_length$startTimestamp %>% max() %>% format(., "%d.%m.%Y"), 
            ", colored by delay time")),
  
  sidebarLayout(
    sidebarPanel(
      # 1) Filter by weekend_day
      checkboxGroupInput(
        inputId  = "weekendFilter",
        label    = "Show:",
        choices  = sort(unique(delay_data_filtered_by_length$weekend_day)),
        selected = sort(unique(delay_data_filtered_by_length$weekend_day))
      ),
      
      # 2) Filter by date (based on startTimestamp)
      sliderInput(
        inputId    = "dateFilter",
        label      = "Date of Delay:",
        min        = min(as.Date(delay_data_filtered_by_length$startTimestamp)),
        max        = max(as.Date(delay_data_filtered_by_length$startTimestamp)),
        value      = c(
          min(as.Date(delay_data_filtered_by_length$startTimestamp)),
          max(as.Date(delay_data_filtered_by_length$startTimestamp))
        ),
        timeFormat = "%Y-%m-%d"
      ),
      
      # 3) Filter by time‐of‐day (HH:MM). We use Europe/Berlin tz so “00:00” truly means midnight.
      sliderInput(
        inputId    = "timeFilter",
        label      = "Time of Day (HH:MM):",
        min        = as.POSIXct("1970-01-01 00:00:00", tz = "Europe/Berlin"),
        max        = as.POSIXct("1970-01-01 23:59:00", tz = "Europe/Berlin"),
        value      = c(
          as.POSIXct("1970-01-01 00:00:00", tz = "Europe/Berlin"),
          as.POSIXct("1970-01-01 23:59:00", tz = "Europe/Berlin")
        ),
        timeFormat = "%H:%M",
        step       = 60
      ),
      
      # 4) Placeholder for clicked‐marker details
      div(
        style = "border:1px solid #ccc; padding:10px; margin-top:80px;",
        htmlOutput("clickInfo")
      )
    ),
    
    mainPanel(
      # ── The Leaflet map ───────────────────────────────────────────────────────
      leafletOutput("germanyMap", height = "700px"),
      
      # ── A fluidRow containing two panels, one left and one right, outside the map ──
      tags$div(
        style = "margin-top: 10px;",
        fluidRow(
          column(
            width = 6,
            # Bottom‐left: dynamic count of delays
            div(
              style = "text-align: left; font-size: 14px;",
              textOutput("countInfo", inline = TRUE)
            )
          ),
          column(
            width = 6,
            # Bottom‐right: static "Sources"
            div(
              style = "text-align: right; font-size: 14px;",
              HTML("<strong>Sources:</strong> Highways: OSM<br/>Delays: Autobahn API")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Precompute a color palette mapping delay_minutes from green (0) to red (30)
  pal <- colorNumeric(
    palette  = c("#00ff00", "#ff0000"),
    domain   = c(0, 30),
    na.color = "#ff0000"
  )
  
  # ═══ Reactive subset based on weekend, date, time‐of‐day ═══
  filteredData <- reactive({
    df_full <- delay_data_filtered_by_length
    
    # 1) If no weekend_day is selected → empty data.frame
    if (is.null(input$weekendFilter) || length(input$weekendFilter) == 0) {
      empty_df    <- df_full[FALSE, ]
      empty_df$id_ <- integer(0)
      return(empty_df)
    }
    
    # 2) Filter by weekend_day
    df <- df_full[df_full$weekend_day %in% input$weekendFilter, ]
    
    # 3) Filter by date range (on startTimestamp)
    if (!is.null(input$dateFilter)) {
      date_vec <- as.Date(df$startTimestamp)
      df <- df[
        (date_vec >= input$dateFilter[1]) &
          (date_vec <= input$dateFilter[2]),
      ]
    }
    
    # 4) Drop any rows whose timestamp is exactly midnight (“00:00”)—
    #    i.e. we convert to POSIXct in Berlin tz, check H:M != "00:00".
    if (nrow(df) > 0) {
      df$start_posix <- as.POSIXct(df$startTimestamp, tz = "Europe/Berlin")
      df <- df[format(df$start_posix, "%H:%M") != "00:00", ]
      df$start_posix <- NULL
    }
    
    # 5) Filter by the time‐of‐day slider (if any rows remain)
    if (nrow(df) > 0 && !is.null(input$timeFilter)) {
      # a) Re‐create a “time-of-day” column on dummy date 1970-01-01
      df$start_posix <- as.POSIXct(df$startTimestamp, tz = "Europe/Berlin")
      df$time_of_day <- as.POSIXct(
        format(df$start_posix, "1970-01-01 %H:%M:%S"),
        tz = "Europe/Berlin"
      )
      
      # b) Keep only those where time_of_day is between slider endpoints
      df <- df[
        (df$time_of_day >= input$timeFilter[1]) &
          (df$time_of_day <= input$timeFilter[2]),
      ]
      
      # c) Drop helpers
      df$start_posix  <- NULL
      df$time_of_day <- NULL
    }
    
    # 6) Add unique layerId for each row
    df$id_ <- seq_len(nrow(df))
    df
  })
  
  # ═══ Render the Leaflet map with circle markers ═══
  output$germanyMap <- renderLeaflet({
    df0 <- filteredData()
    
    leaflet(data = df0) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lng = 10, lat = 51, zoom = 6) %>%
      {
        if (nrow(df0) > 0) {
          addCircleMarkers(
            .,
            lng         = ~longitude,
            lat         = ~latitude,
            radius      = 4,
            color       = ~pal(delay_minutes),
            stroke      = FALSE,
            fillOpacity = 0.8,
            layerId     = ~id_
          )
        } else {
          .
        }
      }
  })
  
  # ═══ When filteredData() changes, update only the markers ═══
  observe({
    df <- filteredData()
    proxy <- leafletProxy("germanyMap")
    
    proxy %>% clearMarkers()
    if (nrow(df) > 0) {
      proxy %>% addCircleMarkers(
        data        = df,
        lng         = ~longitude,
        lat         = ~latitude,
        radius      = 4,
        color       = ~pal(delay_minutes),
        stroke      = FALSE,
        fillOpacity = 0.8,
        layerId     = ~id_
      )
    }
  })
  
  # ═══ Render “clickInfo” (unchanged) ═══
  output$clickInfo <- renderUI({
    click <- input$germanyMap_marker_click
    if (is.null(click)) {
      return(HTML("<em>Click on a point to see details here</em>"))
    }
    df  <- filteredData()
    row <- df[df$id_ == click$id, ]
    if (nrow(row) == 0) {
      return(NULL)
    }
    HTML(paste0(
      "<strong>", row$title, "</strong><br/><br/>",
      "<strong>Delay:</strong> ", row$delay_minutes, " min<br/><br/>",
      "<strong>Avg speed:</strong> ", row$averageSpeed, " km/h<br/><br/>",
      "<strong>Date:</strong> ",
      format(as.POSIXct(row$startTimestamp), "%d %b %Y, %H:%M"),
      "<br/><br/>",
      "<strong>Description:</strong> ",
      row$description %>%
        str_replace_all(., "Beginn: \\d{2}\\.\\d{2}\\.\\d{2} um \\d{2}\\:\\d{2} Uhr", "") %>%
        str_replace_all(., "(Angespannte Verkehrslage|Zusammengesetzte Verkehrsinformation), seit \\d{2}\\.\\d{2}\\.\\d{4}, \\d{2}:\\d{2} ", "") %>%
        str_replace_all(., "Reisezeitverlust:", "<br/><br/><strong>Lost travel time:</strong>") %>%
        str_replace_all(., "Durchschnittsgeschwindigkeit:\\s\\d*\\skm\\/h", "") %>%
        str_replace_all(., "Minuten", "minutes") %>% 
        str_replace_all(., "Im Stillstand", "")
    ))
  })
  
  # ═══ New: Render the dynamic count “Based on X delays.” ═══
  output$countInfo <- renderText({
    n <- nrow(filteredData())
    paste0("Based on ", n, " delays ",
           "exceeding ", delay_data_filtered_by_length$delay_minutes %>% min() -1, " min.")
  })
}

shinyApp(ui, server)
