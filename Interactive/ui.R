library(shiny)
library(ggplot2)
library(shinydashboard)
library(dashboardthemes)
library(plotly)
library(radarchart)

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
      menuItem("How Demographics Relates to Drug Usage", icon = icon("th"),
               menuSubItem("How Drugs are Consumed per Education Level",
                        tabName = "education_drugs"),
               menuSubItem("How Each Country Consumes Drugs",
                           tabName = 'country_drugs'),
               menuSubItem("How Drug Usage Varies by Gender", 
                           tabName = 'gender_drugs')
      ),
      menuItem("How Personality Relates to\n
               Drug Usage", icon = icon("th"),
        menuSubItem("How Raw Personality Scores Vary by Drug", tabName = 'rawPersonality'),
        menuSubItem("Correlation by Drug", tabName = "parallelPlot")
      ),
      menuItem("How Drugs Relate with Each Other", icon = icon("th"),
               menuSubItem("Correlating Drug Usage Levels", tabName = "drugxdrug"),
               menuSubItem("Similarity of Drugs", tabName = 'drugclusters')),
      menuItem("How Similar Users Use Drugs", tabName = "regressionPlot")
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme = "grey_light"),
    
    tabItems(#List of tabs
      tabItem(tabName = "home",#Each tab
              "Sample Landing Page"),

      tabItem(tabName = "parallelPlot",
              fluidRow(
                box(
                  selectInput('focus_drug', 'Drug', colnames(data)[14:32])
                )
              ),
              fluidRow(
                box(plotOutput('personality_and_drugs'), height = "720px", width = 8)
              )
      ),
      
      tabItem(tabName = "regressionPlot",
              tabBox(
                tabPanel('Introduction', 'This is a simple tool to see how users similar to yourself interact with drugs. As input, it takes
                         some simple demographic information such as your age and gender.', br(),'Additionally, it asks for some information about your personality.
                        The four categories are as follows: Neuroticism(Susceptibility to Anxiety), Openness to experiences, Extraversion, Agreeableness,
                          and Conscientiousness(Organization). For each of these categories please rate yourself on a scale of 0-100. A score of 0 for extraversion
                          would indicate that you are a total introvert while a 100 indicates being an extreme extravert.', br(),
                         'The plot below shows how recently similar users have used each drug. The error bars show you the range that similar users could
                         fall into. Please keep in mind that these are merely the otuputs of regression models and results should in no way be taken as statements
                         about individuals.', br(), 'When you are ready please click the tab above this box labeled "input" and proceed.'),
                tabPanel('Inputs',             
                  fluidRow(
                    box(width = 18,
                        column(width = 6,
                               selectInput('age', "Age Range", levels(data$Age)),
                               selectInput('gender', 'Gender', levels(data$Gender)),
                               selectInput('educate', 'Education Level', c('Some High School', 
                                                                           'High School', 'Trade School', 'Some University',
                                                                           'University Degree', 'Masters', 'PhD')),
                               selectInput('country', 'Country', levels(data$Country)),
                               selectInput('ethnic', 'Ethnicity', levels(data$Ethnicity))
                        ),
                        column(width = 6,
                               sliderInput('neuro', 'Neuroticism', min = 0, max = 100, value = 0),
                               sliderInput('extra', 'Extraversion', min = 0, max = 100, value = 0),
                               sliderInput('agree', 'Agreeableness', min = 0, max = 100, value = 0),
                               sliderInput('open', 'Openness', min = 0, max = 100, value = 0),
                               sliderInput('cScore', 'Organization', min = 0, max = 100, value = 0)
                        )
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  plotOutput('regression_plot'), width = 12
                )
              )
            ),
      tabItem(tabName = 'rawPersonality',
              fluidRow(
                box(
                  selectInput('trait', 'Personality Trait to Focus On', c("Neuroticism", "Extraversion", "Openness", "Agreeableness", "Conscientiousness"))
                )
              ),
              fluidRow(
                box(
                  plotlyOutput('rawPersonality'),    
                  width = 10
                )
              )
      ),
      tabItem(tabName = 'education_drugs',
              fluidRow(
                box(
                  selectInput('Education', 'Highest Level of Education Completed', levels(data$Education))
                )
              ),
              
              box(
                plotlyOutput('education_and_drugs'), width = 12
              )
      ),
      tabItem(tabName = 'country_drugs',
              fluidRow(
                inputPanel(
                  selectInput('country_1', 'Pick First Country', levels(data$Country)),
                  selectInput('country_2', 'Pick Second  Country', levels(data$Country),
                              selected = 'USA')
                )  
              ),
              fluidRow(
                box(
                  chartJSRadarOutput("country_drugs", width = "450", height = "300")
                )
              )
      ),
      
      tabItem(tabName = 'drugxdrug',
              fluidPage(
                inputPanel(
                  selectInput("drug_1", label = "Pick a drug",
                              choices = c("Alcohol", "Amphet", "Amyl", "Benzos", "Caff",
                                          "Cannabis", "Choc", "Coke", "Crack", "Ecstasy", 
                                          "Heroin", "Ketamine", "Legalh", "LSD", "Meth", 
                                          "Mushrooms", "Nicotine", "Semer", "VSA"), 
                              selected = "Alcohol"),
                  selectInput("drug_2", label = "Pick another drug",
                              choices = c("Alcohol", "Amphet", "Amyl", "Benzos", "Caff",
                                          "Cannabis", "Choc", "Coke", "Crack", "Ecstasy", 
                                          "Heroin", "Ketamine", "Legalh", "LSD", "Meth", 
                                          "Mushrooms", "Nicotine", "Semer", "VSA"), 
                              selected = "Cannabis")
                ),
                box(plotlyOutput("drugs_plot"), width = 12)
              )
      ),
      tabItem(tabName = 'drugclusters',
              fluidPage(
                inputPanel(
                  sliderInput("k_adjust", label = "Choose number of clusters",
                              min = 1, max = 19, value = 10, step = 1)
                ),
                plotlyOutput("dendrogram")
              )
      ),
      tabItem(tabName = 'gender_drugs',
              fluidRow(
                inputPanel(
                  selectInput("drug", label = "Pick a drug",
                              choices = colnames(data)[14:32])
                )
              ),
              fluidRow(
                box(
                  plotOutput('gender_drugs')
                )
              )
      )
    )
  )
)
