library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)
library(dygraphs)
library(xts)
library(qcc)
library(lubridate)
library(broom)
library(hrbrthemes)  # for theme_ipsum

# Set plotting theme
theme_set(theme_ipsum())

# --- Fake some data -----------------------------------------------------------

# Normally this would be pulled from a database and manipulated as necessary
# in the hrModule.R

# Issues (health, travel, emotions, etc.)
issues <- tribble(
  ~start,~end,~observation,
  "2016-01-01","2016-02-08","Interval example 1",
  "2016-06-02","2016-06-03","Interval example 2",
  "2016-11-01","2016-11-06","Interval example 3",
  "2017-02-20","2017-02-23","Interval example 4",
  "2017-02-23","2017-02-23","Event example 1")
issues <- mutate(issues, id = row_number()) %>% 
  select(id, observation, start, end)
# Split into events and date ranges (plotted differently)
events <- filter(issues, start == end)
periods <- filter(issues, start != end)

# Resting heartrate
fakeDates <- seq(as.Date("2017-01-01"), today(), by = 1)
fakeRates <- round(runif(n = length(fakeDates), min = 50, max = 70), 0)
rhr <- data.frame(date = fakeDates,
                  rate = fakeRates)

# Intraday heartrate
fakeDatetimes <- seq(as.POSIXct("2017-01-01 00:01:00"), now(), by = "min")
fakeRates <- round(runif(n = length(fakeDatetimes), min = 45, max = 150), 0)
idhrTbl <- data.frame(datetime = fakeDatetimes,
                      rate = fakeRates)

# --- Load modules -------------------------------------------------------------
source("./modules/hrModule.R")

