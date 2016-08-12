#---NOTE: Ctrl + Shift + S runs the app

library(shiny)
library(lazyeval)
library(tidyr)
library(dplyr)
library(purrr)
library(broom)
library(ggplot2)
library(RColorBrewer)

shinyServer(function(input, output,session) {
  #---Specs
  Wp <- 120*6*9.8065; E <- 1e6; h_cruise <- 3000; h_ceil <- 3600
  Specifications <- data.frame(
    name = c(
      "Payload Mass (kg)",
      "Payload Weight (Wp) (N)",
      "Battery Specific Energy (E*) (J/kg)",
      "Battery Density (kg/m^3)",
      "Battery Charge Density (J/m^3)",
      "Runway Length (m)",
      "Screen Height (ft)",
      "Landing Speed (kt)",
      "Climb at 2nd Segment (%)",
      "Climb at Cruise (ft/min)",
      "Climb at Ceiling (ft/min)",
      "Altitude of Cruise (ft)",
      "Altitude of Ceiling (ft)",
      "Cruise Mach Number (M)",
      "Cruise Range (km)",
      "Load Factor Minimum",
      "Load Factor Maximum"
    ),
    value = c(120*6, Wp, E, 2750, 2750e6,
              1000, 35, 100, 100, 1.5, 300, 10000, 12000,
              0.5, 1000, -1.5, 3.5))
  output$specs <- renderTable(Specifications)
  
  #---Inputs
  observe({
    # Calculate required inputs
    if (!is.na(input$S) & !is.na(input$b)) updateNumericInput(session, "AR", value = input$b^2/input$S)
    inputvals <- data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                        Cd0 = input$Cd0, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
                        m = input$m, W = input$W, WS = input$WS,
                        P0eng = input$P0eng, P0 = input$P0
                        )
    output$input <- renderTable(inputvals)
    # Use cbind to combine into a dataframe
  })

  
  # output$distPlot <- renderPlot({
  # 
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # 
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # 
  # })
  
  output$table <- renderTable(iris)
  
  observe({
    updateTextInput(session, inputId = "myresults", value = input$mytext) 
    txt <- paste(input$mytext, sample(1:10000, 1))
    updateTextInput(session, inputId = "myresults", value = txt)
  })

})
