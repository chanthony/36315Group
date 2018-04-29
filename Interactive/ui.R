library(shiny)
library(ggplot2)
library(shinydashboard)

dataset <- diamonds

dashboardPage(
  dashboardHeader(title = "Caring Tan"),
  dashboardSidebar(
    sidebarMenu(
      #menuItem(displayed_name, reference_name, icon)
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Graph1", tabName = "graph1", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(#List of tabs
      tabItem(tabName = "home",#Each tab
              "Sample Landing Page"),
      
      tabItem(tabName = "graph1",#Tab name should match menu listing
              fluidRow(
                box(#Stick things in boxes to make them look nice
                  sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
                              value=min(1000, nrow(dataset)), step=500, round=0),
                  
                  selectInput('x', 'X', names(dataset)),
                  selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
                  selectInput('color', 'Color', c('None', names(dataset))),
                  
                  checkboxInput('jitter', 'Jitter'),
                  checkboxInput('smooth', 'Smooth'),
                  
                  selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
                  selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
                )
                ,
                box(plotOutput('plot'), height = "750px")#The plot name from server.r
              )
      )
    )
  )
)
