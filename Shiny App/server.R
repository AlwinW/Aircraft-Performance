#---NOTE: Ctrl + Shift + S runs the app

#---Load required libraries
library(shiny)
library(lazyeval)
library(tidyr)
library(dplyr)
library(purrr)
library(broom)
library(ggplot2)
library(RColorBrewer)

#-- Load required scripts
source("Helper Standard Atmosphere.R")
source("Helper Functions.R")

shinyServer(function(input, output,session) {
  #---Specs
  Wp <- 120*6*9.8065; E <- 1e6; h_cruise <- 3000; h_ceil <- 3600; M <- 0.5
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
              M, 1000, -1.5, 3.5))
  output$specs <- renderTable(Specifications)
  
  #---Inputs
  observe({
    # Calculate required inputs
    # inS <- input$S
    if (!is.na(input$S) & !is.na(input$b)) updateNumericInput(session, "AR", value = input$b^2/input$S)
    if (!is.na(input$e) & !is.na(input$AR)) updateNumericInput(session, "K", value = 1/(pi * input$AR * input$e))
    if (!is.na(input$m)) updateNumericInput(session, "W", value = input$m * 9.8065)
    if (!is.na(input$W) & !is.na(input$S)) updateNumericInput(session, "WS", value = input$W/input$S)
    inputvals <- data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                        Cd0 = input$Cd0, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
                        m = input$m, W = input$W, WS = input$WS,
                        P0eng = input$P0eng, P0 = input$P0
                        )
    output$input <- renderTable(inputvals)
    # Use cbind to combine into a dataframe
  })

  #--- Aerodynamic Properties
  observe({
    AeroParamsOut <- AeroParams(inputvals) %>%
      select(type, h, Vinf, qinf, Cl, Cd, ClCd, Clstar, Cd, Cdstar, ClCdstar, Vstar, Cl32, Cd32, ClCd32, V32)
    output$AeroParams <- renderTable(t(AeroParamsOut))
  })
  
  # Uncomment ONLYL for debugging - otherwise, the app will no longer be reactive
  # input <- data.frame(S = 54.4, b = 25.90, AR = 12.33, e = 0.8, K = 0.0323, Cd0 = 0.02, Clmax = 1.2, Clflaps = 0.80, Clhls = 1.20,
  #                     m = 15806, W = 155000, WS = 2850, P0eng = 1530000, P0 = 306000)
  
  output$table <- renderTable(iris)
  
})
