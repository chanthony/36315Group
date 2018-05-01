library(shiny)
library(ggplot2)
library(RCurl)
library(dplyr)
library(magrittr)
library(plotly)

function(input, output) {
  
  dataset <- reactive({
    diamonds[sample(nrow(diamonds), input$sampleSize),]
  })
  
  data <- read.csv(file = "drug_consumption.csv", header = TRUE)
  
  output$rawPersonality <- renderPlotly({
    
    used_recently <- c("CL3", "CL4", "CL5", "CL6")
    
    drugs_df <- read.csv(text = getURL("https://raw.githubusercontent.com/chanthony/36315Group/master/Interactive/drug_consumption_personality.csv"))
    
    drugs_df <- drugs_df[drugs_df$Usage %in% used_recently,]
    
    colnames(drugs_df)[6] <- "Conscientiousness"
    
    drugs_summary <- drugs_df %>% group_by(Drug) %>%
      summarise(Neuroticism =  median(Neuroticism),
                Extraversion = median(Extraversion),
                Openness = median(Openness),
                Agreeableness = median(Agreeableness),
                Conscientiousness = median(Conscientiousness))
    
    title_string <- sprintf("%s Scores For Each Drug", input$trait)
    
    p <- ggplot(drugs_df, aes(x = Drug, y = drugs_df[,input$trait])) +
      #geom_point(alpha = 0.5) +
      geom_jitter(width = 0.2, alpha = 0.2) +
      geom_point(data = drugs_summary, aes_string(x = 'Drug', y = input$trait, size = '1.5'),
                 color = 'red') +
      theme(legend.title = element_blank()) +
      scale_size_continuous(breaks = c(2),
                          labels = c("Median Score")) +
      labs(
        x = "Drug",
        y = "Score",
        title = title_string
      )
  
    ggplotly(p)
  })
  
  #Change plot name to match plot descrition e.g. output$drug_predicted
  output$plot <- renderPlot({#Each plot to be rendered in the UI
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$jitter)
      p <- p + geom_jitter()
    if (input$smooth)
      p <- p + geom_smooth()
    
    p
    
  }, height=700)
  
  output$personality_and_drugs <- renderPlot({
    #Make a copy of the data
    temp_data <- data
    
    #Convert all the drug factor levels to something we can use
    levels(temp_data$Alcohol) <- 0:6
    levels(temp_data$Amphet) <- 0:6
    levels(temp_data$Amyl) <- 0:6
    levels(temp_data$Benzos) <- 0:6
    levels(temp_data$Caff) <- 0:6
    levels(temp_data$Cannabis) <- 0:6
    levels(temp_data$Choc) <- 0:6
    levels(temp_data$Coke) <- 0:6
    levels(temp_data$Crack) <- 0:6
    levels(temp_data$Ecstasy) <- 0:6
    levels(temp_data$Heroin) <- 0:6
    levels(temp_data$Ketamine) <- 0:6
    levels(temp_data$Legalh) <- 0:6
    levels(temp_data$LSD) <- 0:6
    levels(temp_data$Meth) <- 0:6
    levels(temp_data$Mushrooms) <- 0:6
    levels(temp_data$Nicotine) <- 0:6
    levels(temp_data$Semer) <- 0:6
    levels(temp_data$VSA) <- 0:6
    
    #Conver the factor levels to a numeric row so we can use them
    temp_data[14:32] <- lapply(temp_data[14:32], as.numeric)
    
    #Empty frame of correlation of each drug with each personality trait
    drugs_df <- data.frame(
                          Drug <- character(),
                          trait <- character(),
                          cor <- numeric(),
                          alpha <- numeric()
    )
    
    #Extract all the drugs and traits
    drugs <- colnames(temp_data)[14:32]
    traits <- colnames(temp_data)[7:10]
    
    #For every column and trait
    for(drug in drugs){
      for(trait in traits){
        cur_cor <- cor(temp_data[[drug]], temp_data[[trait]])
        alpha <- 0.2
        if(drug == input$focus_drug){
          alpha <- 0.7
        }
        drugs_df <- rbind(drugs_df, data.frame("Drug"=drug, "trait"=trait,
                                               "cor"=cur_cor, alpha = alpha))
      }
    }
    
    
    p <- ggplot(drugs_df, aes(x = trait, y = cor, group = Drug)) +
      geom_path(aes(size = 1.5, alpha = alpha)) +
      theme(legend.position = "none") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        x = "Personality Trait",
        y = "Correlation",
        title = "Correlation of Each Personality Trait with Drug Usage"
      )
    
    p
  }, width = 1000, height = 700)
  
  output$regression_plot <- renderPlot({
    #Make a copy of the data
    temp_data <- data
    
    #Convert all the drug factor levels to something we can use
    levels(temp_data$Alcohol) <- 0:6
    levels(temp_data$Amphet) <- 0:6
    levels(temp_data$Amyl) <- 0:6
    levels(temp_data$Benzos) <- 0:6
    levels(temp_data$Caff) <- 0:6
    levels(temp_data$Cannabis) <- 0:6
    levels(temp_data$Choc) <- 0:6
    levels(temp_data$Coke) <- 0:6
    levels(temp_data$Crack) <- 0:6
    levels(temp_data$Ecstasy) <- 0:6
    levels(temp_data$Heroin) <- 0:6
    levels(temp_data$Ketamine) <- 0:6
    levels(temp_data$Legalh) <- 0:6
    levels(temp_data$LSD) <- 0:6
    levels(temp_data$Meth) <- 0:6
    levels(temp_data$Mushrooms) <- 0:6
    levels(temp_data$Nicotine) <- 0:6
    levels(temp_data$Semer) <- 0:6
    levels(temp_data$VSA) <- 0:6
    
    levels(temp_data$Age) <- 0:5
    levels(temp_data$Gender) <- 0:1
    levels(temp_data$Education) <- 0:8
    levels(temp_data$Country) <- 0:6
    levels(temp_data$Ethnicity) <- 0:6
    
    temp_data <- data.frame(lapply(temp_data, FUN = as.numeric))
    
    #Extract all the drugs and traits
    drugs <- colnames(temp_data)[14:32]
    traits <- colnames(temp_data)[7:10]
    
    predicted_df <- data.frame(
      Drug <- character(),
      usage <- numeric(),
      lower <- numeric(),
      upper <- numeric()
    )
    
    #input <- data.frame(age = '18-24', gender = 'Male', educate = 'Trade School',
    #                    country = 'Canada', ethnic = 'Asian', neuro = 50, extra = 50, open = 50, cScore = 50, agree = 50)
    
    sample_df <- data.frame(Age = input$age,
                            Gender = input$gender,
                            Education = input$educate,
                            Country = input$country,
                            Ethnicity = input$ethnic,
                            Neuroticism = input$neuro,
                            Extraversion = input$extra,
                            Openness = input$open,
                            Cscore = input$cScore,
                            Agreeableness = input$agree)
    
    sample_df$Education <- match(sample_df$Education, c(0, 'Some High School', 2, 'High School',
                                                        'Some University', 'Trade School', 'University Degree',
                                                        'Masters', 'PhD'))
    
    sample_df$Age <- match(sample_df$Age, levels(data$Age))
    sample_df$Country <- match(sample_df$Country, levels(data$Country))
    sample_df$Ethnicity <- match(sample_df$Ethnicity, levels(data$Ethnicity))
    sample_df$Gender <- match(sample_df$Gender, levels(data$Gender))
    
    #drug <- 'Alcohol'
    
    for(drug in drugs){
      temp_data$Drug <- temp_data[,drug]
      reg <- lm(Drug ~ Age + Gender + Education + Country + Ethnicity + 
                  Neuroticism + Openness + Agreeableness + Cscore + Extraversion,
                data = temp_data)
      
      predicted <- predict(reg, sample_df, interval='confidence')
      
      mean <- max(0, min(6, round(predicted[1])))
      
      new_entry <- data.frame(Drug = drug, usage = mean, lower = predicted[2], upper = predicted[3])
      
      predicted_df <- rbind(predicted_df, new_entry)
    }
    
    p <- ggplot(predicted_df, aes(x = Drug, y = usage)) +
      geom_bar(stat = "identity", alpha = 0.5) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
      scale_y_continuous(breaks = 0:6, labels = c('Never', 'Over a Decade Ago', 'In Last Decade',
                                  'In Last Year', 'In Last Month', 'In Last Week',
                                  'In Last Day')) +
      labs(
        y = 'Predicted Usage',
        title = 'Predicted Usage by Drug'
      ) +
      theme(
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14)
      )
    
    p
  })
  
}