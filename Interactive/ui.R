library(shiny)
library(ggplot2)
library(shinydashboard)
library(dashboardthemes)
library(plotly)
library(radarchart)
library(d3heatmap)

#IF you do not have dashboard themes installed run the following code:
# library(devtools)
# install_github("nik01010/dashboardthemes")


dataset <- diamonds

data <- read.csv(file = "drug_consumption.csv", header = TRUE)

dashboardPage(
  skin = "purple",
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
    tabItems(#List of tabs
      tabItem(tabName = "home",#Each tab
              fluidRow(box(title = "Introduction", status = "success",solidHeader = TRUE, width = 12,
              'In this application, we analyze the different factors that may play into drug usage
              in individuals and the nature of their various relationships. Our analysis is based on the
              dataset used in the paper titled "The Five Factor Model of personality and evaluation of drug consumption risk"
              (Fehrman et al., 2015).', br(), 'The dataset contains records for 1,885 respondents. Each record contains an individuals
              basic demographics (i.e., level of education, age, gender, country of residence and ethnicity), personality 
              measurements (i.e., neuroticism, extraversion, openness to experience, agreeableness, conscientiousness,
              impulsivity, and sensation seeking), and frequency of drug usage. The dataset contains usage data for the
              following drugs: alcohol, amphetamines, amyl nitrite, benzodiazepine, cannabis, chocolate, cocaine, caffeine, 
              crack, ecstasy, heroin, ketamine, legal highs, LSD, methadone, mushrooms, nicotine, and semeron.')),
              fluidRow(
                box(title = 'Data Source', status = 'info',
                  "The Five Factor Model of personality and evaluation of drug consumption risk(Fehrman et al., 2015)"
                ),
                box(title = "Credits", status = 'info',
                    "Anthony Chan",br(),
                    "Minwhan Cho",br(),
                    "Eric Huang",br(),
                    "Jae Won Yoon"
                )
              )
      ),

      tabItem(tabName = "parallelPlot",
              fluidRow(
                box(title = "Input", status = "primary", solidHeader = TRUE,
                  selectInput('focus_drug', 'Drug', colnames(data)[14:32])
                )
              ),
              fluidRow(
                box(plotOutput('personality_and_drugs'), height = "420px", width = 8)
              ),
              fluidRow(
                box(title = "Description", status = "warning", solidHeader = TRUE,
                      "This is a plot showing the relationship of the correlations of different personality scores with drug usage levels.
                      When we choose a certain drug, we see the mapped lines highlighted for that certain drug. The lines for the
                      relations between different personalities are connected. With this plot, we can begin to make basic guesses
                      about how the different personality types consume drugs.")
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
                box(title = "Input", status = "primary", solidHeader = TRUE,
                  selectInput('trait', 'Personality Trait to Focus On',
                              c("Neuroticism", "Extraversion", "Openness", "Agreeableness", "Conscientiousness"))
                )
              ),
              fluidRow(
                box(
                  plotlyOutput('rawPersonality'),    
                  width = 10
                )
              ),
              fluidRow(
                box(title = "Instructions", status = "warning", solidHeader = TRUE,
                         "Pick a personality trait and observe its scores for every drug."),
                box(title = "Description", status = "warning", solidHeader = TRUE,
                          "This is a scatterplot of personality traits scores and drug usage jittered slightl
                           y to observe the different scores for every drug. This information is from drug users
                           who are associated with the 5 categorized personality traits above.
                           The large red dots indicate the median score for each drug. 
                           Through this plot, we can see the different drug usage preferences for people with
                           different personality traits.")
              )
      ),
      tabItem(tabName = 'education_drugs',
              fluidRow(
                box(title = "Input", status = "primary", solidHeader = TRUE,
                  selectInput('Education', 'Highest Level of Education Completed',
                              c("<16", "16", "17", "18","Some College","University Degree", 'Trade School', "Masters", "PhD"))
                )
              ),
              
              box(
                plotlyOutput('education_and_drugs'), width = 12
              ),
              box(title = "Description", status = "warning", solidHeader = TRUE,
                'The distribution above illustrates the respondents 
                usage for each drug and how recently they consumed said drugs. The graph only contains
                the data for respondents of the selected education level. Each bar represents a single drug
                and each color in that bar represents how recently each respondent reported using said drug.',
                br(), br(), 'Barring minor fluctuations, it appears that drug usage patterns are consistent across
                education levels suggesting that education levels may not have a strong influence on drug usage patterns.'
              )
      ),
      tabItem(tabName = 'country_drugs',
              fluidRow(
                box(title = "Input", status = "primary", solidHeader = TRUE,
                  selectInput('country_1', 'Pick First Country', levels(data$Country)),
                  selectInput('country_2', 'Pick Second  Country', levels(data$Country),
                              selected = 'USA')
                )  
              ),
              fluidRow(
                box(
                  chartJSRadarOutput("country_drugs", width = "550", height = "400")
                ),
                column(width = 6,
                  box(title = "Instructions", status = "warning", solidHeader = TRUE,
                      "Pick two countries and observe the levels of usage for all drugs in both countries."),
                  box(title = "Description", status = "warning", solidHeader = TRUE,
                      "	From the polar coordinates, we observe the median used period for all types of drugs
                      . Red lines and dots indicate the scores of the first country, 
                      green the second. This way we can observe differences between how the two countries consume drugs")),
                  column(box(title = "Guide", status = "warning", solidHeader = TRUE,
                        "0 - Never Used", br(), "1 - Used over a Decade Ago", br(),
                        "2 - Used in Last Decade", br(), "3 - Used in Last Year", br(),  "4 Used in Last Month", br(),
                        "5 Used in Last Week", br(), "6 Used in Last Day."), width = 6
                  )
              )
      ),
      
      tabItem(tabName = 'drugxdrug',
              fluidPage(
                box(title = "Description", status = "warning", solidHeader = TRUE,
                    "This heat map displays the correlation between respondents'
                    usage of two pairs of drugs. Within each correlation, users can observe
                    whether the usage of those two drugs are showing relative similarity or
                    difference through their correlation.", br(), br(),
                    "The goal of this graph is to see whether the usage pattern for one particular
                    drug has an effect on the usage pattern of a different drug. For example, if
                    for two drugs the correlation is very high, that may suggest that usage of one drug
                    has a positive effect on usage of another drug and vice versa for very negative correlations."),
                box(d3heatmapOutput("drugs_plot"), width = 12)
              )
      ),
      tabItem(tabName = 'drugclusters',
              fluidPage(
                box(title = "Input", status = "primary", solidHeader = TRUE,
                  sliderInput("k_adjust", label = "Choose number of clusters",
                              min = 1, max = 19, value = 10, step = 1)
                )
              ),
              fluidRow(plotlyOutput("dendrogram")),
              box(title = "Description", status = "warning", solidHeader = TRUE,
                    "Based on the correlation between respondents' usage of different types of drugs,
                    drugs that show high similarity in usage frequency and tendency can be grouped into same cluster.
                    Then, with users' choice of number of clusters, the following graph will display the dendrogram
                    of similarity of drugs. By looking at the clustering, we can observe which drugs tend to have the
                    same usage pattern and guess at some of the basic relationships between the drugs.")
      ),
      tabItem(tabName = 'gender_drugs',
              fluidRow(
                box(title = "Input", status = "primary", solidHeader = TRUE,
                    width = 12,
                    selectInput("drug", label = "Pick a drug",
                                choices = colnames(data)[14:32])
                )
              ),
              fluidPage(
                box(width = 12,
                  plotOutput('gender_drugs')
                ),
                box(title = "Instructions", status = "warning", solidHeader = TRUE,
                    "Pick a drug and observe the usage of the drug in the past year for both genders."
                  ),
                box(title = "Description", status = "warning", solidHeader = TRUE,
                      'From the pie chart, we observe that for some drugs, men and women do not have much difference
                      s in usage time periods. However, some drugs have higher percentage of male users than female users
                      . For example, cocaine, ecstasy, meth, all of which fall under the category of amphetamines,
                      have more male users than female users. Also, if we observe the classification of each drug,
                      "harder" drugs have a much greater percentage of people who have never used that drug for both genders.')
              )
      )
    )
  )
)
