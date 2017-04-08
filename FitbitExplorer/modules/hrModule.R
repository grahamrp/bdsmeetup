# Shiny module to explore heartrate data

# Calculate heartrate by hour, shared by later plots
hrByHour <- 
  idhrTbl %>% 
  mutate(date = floor_date(datetime, unit = "day"),
         hour = hour(datetime),
         dow = wday(datetime),
         wday = weekdays(date)) %>% 
  group_by(date, dow, wday, hour) %>% 
  summarise(rate = mean(rate, na.rm = TRUE)) %>% 
  arrange(date, hour)

#' Create heartrate UI
#' 
#' Creates the UI for heartrate exploration
#' 
#' @param id unique identifier for instance of module
hrUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    navbarPage("HR",
               tabPanel("Overview",
                        fluidRow(
                          textOutput(ns("hrTrend")),
                          checkboxInput(ns("showEvents"), label = "Show Instances"),
                          checkboxInput(ns("showPeriods"), label = "Show Intervals")
                        ),
                        
                        fluidRow(
                          box(title = "Resting Heartrate", width = 12,
                              dygraphOutput(ns("resting"))
                          )
                        ),
                        
                        fluidRow(
                          box(title = "Intraday Heartrate", width = 12,
                              dateInput(ns("fromDate"), label = "Date", value = "2017-01-01"),
                              dygraphOutput(ns("averageDaily"))
                          )
                        )
                        
               ),
               tabPanel("Anomalies",
                        fluidRow(
                          box(title = "Resting Heartrate Anomalies", width = 12,
                              "Are there any anomalies? Very high? very low? Do these coincide with other events? Illness, stress, exercise", 
                              plotOutput(ns("anomResting"))
                          )
                        )
               ),
               tabPanel("Daily",
                        dateRangeInput(ns("dateRangeTypicalDay"), label = "Date Range:",
                                       start = "2017-01-01", end = "2017-03-01"),
                        fluidRow(
                          box(title = "Typical Day", width = 12,
                              
                              "  - How does my heart-rate vary over a typical day?",
                              plotOutput(ns("avgHrByHour"))
                          )
                        ),
                        fluidRow(
                          box(title = "Weekday comparison", width = 12,
                              "Does each day of the week show a different pattern?",
                              plotOutput(ns("avgHrByHourDay"))
                          )
                        )
               )
    )
  )
}

# ---------------------------- Server ------------------------------------------

#' Render heartrate UI
#' 
#' Provides heartrate plots and interactions
#' 
#' @param con connection to database (not used in this demo version as all datasets are faked in global.R)

hr <- function(input, output, session, con) {
  
  # Filtering for specific date ranges will update all the charts that depend on
  # this reactive data
  hrByHourFiltered <- reactive({
    filter(hrByHour, date > ymd(input$dateRangeTypicalDay[1]),
           date < ymd(input$dateRangeTypicalDay[2]))
  })
  
  
  # Resting Heart Rate
  output$resting <- renderDygraph({
    
    # TODO: plumb in your own data here
    # Convert to time series ready for dyGraphs
    rhrSeries <- xts(rhr$rate, order.by = rhr$date)
    
    # Create string for later evaluation. String build approach is necessary to
    # string together multiple dyGraph elements (e.g. multiple annotations)
    graphCode <- "dygraph(rhrSeries) %>% dyRangeSelector()"
    
    # Render note events if selected
    if (input$showEvents) {
      showEventsCode <- paste(paste0("dyAnnotation('", events$start,
                                     "', text = '", events$id,"', tooltip = '", events$observation, "')"),
                              collapse = " %>% ")
      graphCode <- paste(graphCode, showEventsCode, sep = " %>% ")
    }
    # Render note periods if selected
    if (input$showPeriods) {
      showPeriodsCode <- paste(paste0("dyShading(from = '", periods$start, "', to = '", periods$end, "')"),
                               collapse = " %>% ")
      showStartPeriodCode <- paste(paste0("dyEvent('", periods$start, "', label = '", periods$observation, "', labelLoc = 'bottom')"),
                                   collapse = " %>% ")
      graphCode <- paste(graphCode, showPeriodsCode, showStartPeriodCode, sep = " %>% ")
    }
    
    # Evaluate the built sring as though it were code
    eval(parse(text = graphCode))
    
  })
  
  # Resting HR year-long trend
  # TODO: Maybe don't rebuild model every render
  output$hrTrend <- renderText({
    
    # Calculate trend based on previous year's data
    # Get a year of data if available
    trendData <- 
      rhr %>% 
      top_n(n = 365, wt = date)
    # Get trend from linear model
    lmOutputs <- lm(rate ~ date, data = trendData) %>% 
      tidy() %>% 
      filter(term == "date")
    
    # Calculate how many days of data we have (in case less than a year's worth)
    nDays <- nrow(trendData)
    
    # Verbalise trend
    if (lmOutputs$p.value < 0.05) {
      # Work out direction of change
      direction <- if (lmOutputs$estimate > 0) "increasing" else "decreasing"
      
      paste("From the most recent", nDays, 
            "days it looks like your resting heart rate is", direction, "in the long run,",
            "by", round(abs(lmOutputs$estimate) * 365, 1), "beats per year")
    } else {
      paste("It looks like your resting heart rate has been pretty constant over the past",
            nDays, "days")
    }
  })
  
  # Intraday Heart Rate
  output$averageDaily <- renderDygraph({
    
    # TODO: plumb in your own data here
    
    maxDay <- idhrTbl %>% 
      mutate(day = floor_date(datetime, unit = "day")) %>% 
      summarise(day = as.Date(max(day))) %>% 
      .$day
    
    fromDay <- ifelse(input$fromDate > maxDay, maxDay, input$fromDate)
    fromDay <- as.Date(fromDay)
    
    idhr <- idhrTbl %>% 
      filter(floor_date(datetime, unit = "day") == fromDay) %>% 
      as.data.frame()
    series <- xts(idhr$rate, order.by = idhr$datetime)
    dygraph(series) %>% 
      dyRoller(rollPeriod = 5)  # Add rolling average
    
  })
  
  # Anomalies ---------------------------------------------------------------
  
  # Anomalies for Resting Heart Rate
  output$anomResting <- renderPlot({
    
    # TODO: plumb in your own data here
    latestRhr <- filter(rhr, date > max(date) - years(1)) %>% 
      arrange(date)
    qccResult <- qcc(latestRhr$rate, type = "xbar.one", labels = latestRhr$date,
                     data.name = "Resting HR", nsigmas = 6, title = "Shewhart Chart: Resting HR")
    
  })
  
  
  # Daily Trends ------------------------------------------------------------
  
  output$avgHrByHour <- renderPlot({
    hrByHourFiltered() %>% 
      mutate(weekend = if_else(dow %in% c(0, 6), "weekend", "weekday")) %>% 
      ggplot(aes(x = hour, y = rate)) + geom_line(aes(group = date, alpha = weekend)) + 
      facet_grid(weekend ~ .) +
      scale_alpha_discrete(guide = FALSE, range = c(0.1, 0.2)) +
      labs(title = "Average HR per Hour", x = "Hours in the Day", y = "Average Heart Rate")
    
  })
  
  output$avgHrByHourDay <- renderPlot({
    
    hrByHourFiltered() %>% 
      group_by(hour, wday) %>% 
      filter(date > ymd("2016-05-01") & date < ymd("2017-03-10")) %>% 
      summarise(rate = mean(rate)) %>% 
      mutate(weekend = if_else(wday %in% c("Saturday", "Sunday"), "weekend", "weekday")) %>% 
      ggplot(aes(x = hour, y = rate)) + 
      geom_line(aes(group = wday, col = wday, linetype = weekend), size  = 1.5) + 
      labs(title = "Average HR per Hour", x = "Hours in the Day", y = "Average Heart Rate") +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      scale_linetype(guide = FALSE)
    
  })
  
}






