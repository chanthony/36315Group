library(shiny)
library(ggplot2)

function(input, output) {
  
  dataset <- reactive({
    diamonds[sample(nrow(diamonds), input$sampleSize),]
  })
  
  data <- read.csv(file = "drug_consumption.csv", header = TRUE)
  
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
  })
  
}