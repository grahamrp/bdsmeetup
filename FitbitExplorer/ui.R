library(shiny)
library(shinydashboard)

dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Heart Rate", tabName = "heartrate", icon = icon("heartbeat"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "heartrate",
              hrUI("hr1")
      )
    )
  )
)