library(shiny)
library(ggplot2)
library(shinydashboard)
library(dashboardthemes)

#IF you do not have dashboard themes installed run the following code:
# library(devtools)
# install_github("nik01010/dashboardthemes")


dataset <- diamonds

data <- read.csv(file = "drug_consumption.csv", header = TRUE)

dashboardPage(
  
  dashboardHeader(title = "Caring Tan"),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      #menuItem(displayed_name, reference_name, icon)
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Question1", icon = icon("th"),
        menuSubItem("Plot1.1", tabName = "graph1")),
      menuItem("How Personality Relates to\n
               Drug Usage", icon = icon("th"),
        menuSubItem("Correlation by Drug", tabName = "parallelPlot"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),
    
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
      ),
      tabItem(tabName = "parallelPlot",
              fluidRow(
                box(
                  selectInput('focus_drug', 'Drug', colnames(data)[14:32])
                )
              ),
              fluidRow(
                box(plotOutput('personality_and_drugs'))
              )
      )
    )
  )
)
