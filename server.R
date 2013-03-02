library(shiny)
library(datasets)

# Redemptions = -66938.87 + 0.006763872(Sessions) + 136197.5(Notification.Click.Through.Rate) + 643754.7(Reward.Redemption.Rate) #
# Define server logic required input and output predicted rewards

shinyServer(function(input, output) {
  
  formulaPredict <- (reactive(function() {
    # session
    session.genre <- ifelse(input$genre == "Total", as.numeric(1), ifelse(input$genre == "games", as.numeric(.9), as.numeric(.05))) 
    session.country <- ifelse(input$country == "Total", as.numeric(9000000), ifelse(input$country == "US", as.numeric(1152945), ifelse(input$country == "GB", as.numeric(747938), ifelse(input$country == "JP", as.numeric(290220), error)))) 
    session.platform <- ifelse(input$platform == "Total", as.numeric(1), ifelse(input$platform == "ios", as.numeric(.9), as.numeric(.10))) 
    sessions <- session.country*session.genre*session.platform#*as.numeric(input$gender)*as.numeric(input$age)
    # redemption rate 
    RRR <- ifelse(input$genre == "Total", as.numeric(.07), ifelse(input$genre == "games", as.numeric(.05), as.numeric(.10))) 
    # Notification.Click.Through.Rate calculation
    NCTR <- ifelse(input$genre == "Total", as.numeric(.16), ifelse(input$genre == "games", as.numeric(.16), as.numeric(.25))) 
        
    ## Calculate Reward Delivery ##
    round(-66938.87 + 0.006763872*sessions + 643754.7*RRR + 136197.5*NCTR)
  }))
  
  output$prediction <- reactiveText(function() {
    paste(formulaPredict(), "rewards per day")
  })
  
  formulaRR <- reactive(function() {
    # Reward Redemption rate
    RRR <- ifelse(input$genre == "Total", as.numeric(.07), ifelse(input$genre == "games", as.numeric(.05), as.numeric(.10))) 
    paste("Redemption Rate,",input$genre," - ", RRR*100,"%")
  })
  output$RR <- reactiveText(function() {
    formulaRR()
  })
  
  formulaCTR <- reactive(function() {
    # Notification.Click.Through.Rate calculation
    NCTR <- ifelse(input$genre == "Total", as.numeric(.16), ifelse(input$genre == "games", as.numeric(.16), as.numeric(.25))) 
    paste("Notification CTR,",input$genre," - ", NCTR*100,"%")
  })
  output$CTR <- reactiveText(function() {
    formulaCTR()
  })
  
  formulaS <- reactive(function() {
    session.genre <- ifelse(input$genre == "Total", as.numeric(1), ifelse(input$genre == "games", as.numeric(.9), as.numeric(.05))) 
    session.country <- ifelse(input$country == "Total", as.numeric(9000000), ifelse(input$country == "US", as.numeric(1152945), ifelse(input$country == "GB", as.numeric(747938), ifelse(input$country == "JP", as.numeric(290220), error)))) 
    session.platform <- ifelse(input$platform == "Total", as.numeric(1), ifelse(input$platform == "ios", as.numeric(.9), as.numeric(.10))) 
    sessions.predicted <- session.country * session.genre * session.platform
    paste("Sessions,",input$country,",",input$platform,",", input$genre," - ", round(sessions.predicted))
  })
  output$S <- reactiveText(function() {
    formulaS()
  })

})