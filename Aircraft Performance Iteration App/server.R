## Initialisation ======================================================================
#--- Load required libraries
library(shiny)
library(lazyeval)
library(tidyr)
library(dplyr)
library(purrr)
library(broom)
library(ggplot2)
library(RColorBrewer)

#--- Set the initial values
source("Helper UI Functions.R")
source("Helper Standard Atmosphere.R")
source("Helper Main Functions.R")

#--- Set initial display options
theme_set(theme_linedraw())
options(scipen = 10)

## Server ======================================================================
shinyServer(function(session, input, output) {
## Non-Reactive ======================================================================
  output$SpecificationsTable <- renderDataTable({
    specifications[,2:3]
  })
  
## Input Values ======================================================================
  #--- Allow a user to upload their inputs
  inputdata <- reactive({
    infile <- input$uploadData
    if (is.null(infile)) return(NULL)
    read.csv(infile$datapath)
  })
  
  #--- Change ONLY when a new file is uploaded
  observe({
    #--- Update the input values
    inputdatavars <- inputdata()
    if (is.null(inputdatavars)) return(NULL)
    updateNumericInput(session, "S", value = inputdatavars$S)
    updateNumericInput(session, "b", value = inputdatavars$b)
    updateNumericInput(session, "AR", value = inputdatavars$AR)
    updateNumericInput(session, "e", value = inputdatavars$e)
    updateNumericInput(session, "K", value = inputdatavars$K)
    
    updateNumericInput(session, "Cd0", value = inputdatavars$Cd0)
    updateNumericInput(session, "Clclean", value = inputdatavars$Clclean)
    updateNumericInput(session, "Clflaps", value = inputdatavars$Clflaps)
    updateNumericInput(session, "Clhls", value = inputdatavars$Clhls)
    
    updateNumericInput(session, "m", value = inputdatavars$m)
    updateNumericInput(session, "W", value = inputdatavars$W)
    updateNumericInput(session, "WS", value = inputdatavars$WS)
    
    updateNumericInput(session, "P0eng", value = inputdatavars$P0eng)
    updateNumericInput(session, "P0", value = inputdatavars$P0)
    updateNumericInput(session, "Etatotal", value = inputdatavars$Etatotal)
    
    updateNumericInput(session, "ClG", value = inputdatavars$ClG)
    updateNumericInput(session, "Cd0G", value = inputdatavars$Cd0G)
    updateNumericInput(session, "hground", value = inputdatavars$hground)
    # End Observe
  })
  
  #--- Change whenever ANY input is changed
  observe({
    #--- Make calculations in the input boxes
    if (!is.na(input$S) & !is.na(input$AR) & input$S*input$AR != 0)
      updateNumericInput(session, "b", value = sqrt(input$AR * input$S))
    if (!is.na(input$e) & !is.na(input$AR) & input$e*input$AR != 0) 
      updateNumericInput(session, "K", value = 1/(pi * input$AR * input$e))
    if (!is.na(input$m) & input$m != 0) 
      updateNumericInput(session, "W", value = input$m * 9.8065)
    if (!is.na(input$W) & !is.na(input$S) & input$W * input$S != 0) 
      updateNumericInput(session, "WS", value = input$W/input$S)
    if (!is.na(input$P0eng) & input$P0eng != 0) 
      updateNumericInput(session, "P0", value = input$P0eng * 2)
    #--- Store the inputs as a dataframe
    inputvals <- 
      data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                 Cd0 = input$Cd0, Clclean = input$Clclean, Clflaps = input$Clflaps, Clhls = input$Clhls,
                 m = input$m, W = input$W, WS = input$WS,
                 P0eng = input$P0eng, P0 = input$P0, Etatotal = input$Etatotal,
                 ClG = input$ClG, Cd0G = input$Cd0G, hground = input$hground
      )
    #--- Allow a user to download their inputs
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(inputvals, file)
      }
    )
    # End Observe
  })

## Calculations ======================================================================
  #--- Change whenever ANY input is changed
  observe({
    #--- Store the inputs as a dataframe
    inputvals <- 
      data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                 Cd0 = input$Cd0, Clclean = input$Clclean, Clflaps = input$Clflaps, Clhls = input$Clhls,
                 m = input$m, W = input$W, WS = input$WS,
                 P0eng = input$P0eng, P0 = input$P0, Etatotal = input$Etatotal,
                 ClG = input$ClG, Cd0G = input$Cd0G, hground = input$hground
      )
    
    MainIterationOut <- suppressWarnings(MainIterationFunction(inputvals, specifications, out = "All"))
    MainGraphsOut <- suppressWarnings(MainGraphingFunction(inputvals, specifications))
    
## Summary ======================================================================
    output$SummaryTable <- renderDataTable({
      MainIterationOut$summary
    })
    
## AeroParams ======================================================================
    output$AeroParamsTable <- renderDataTable({
      MainIterationOut$AeroParamsTable
    })
    
    output$AeroParamsPlot <- renderPlot({
      slope = MainGraphsOut$AeroParamsPlotPoints$Cl[3]/MainGraphsOut$AeroParamsPlotPoints$Cd[3]
      ggplot(MainGraphsOut$AeroParamsPlotPoints,
             aes(x = Cd, y = Cl, colour = type)) +
        geom_abline(intercept = 0, slope = slope, colour = "green4") + 
        geom_line(data = MainGraphsOut$AeroParamsPlot,
                  aes(x = Cd, y = Cl, colour = "Drag Polar")) +
        geom_point() + 
        geom_text(aes(label = paste0(type, " Vinf = ", round(Vinf, 4))), hjust = 1, vjust = -0.5, show.legend = FALSE) + 
        scale_color_manual(values = c("Drag Polar" = "grey4", "Cruise" = "blue",
                                      "(L/D)*" = "green3", "L^(3/2)/D" = "purple",
                                      "Stall" = "red")) +
        expand_limits(x = 0, y = 0) +
        labs(list(title = "Drag Polar", x = "Coefficient of Drag", y = "Coefficient of Lift", colour = ""))
    })
    
    output$APP_info <- renderText({
      paste0("click: ", xy_str(input$APP_click), "hover: ", xy_str(input$APP_hover)
      )
    })
    
## Mission Analysis ======================================================================
    output$PowerSummary <- renderPlot({
      ggplot(mutate(MainIterationOut$BatteryFracs, type = factor(type, levels = type)), 
             aes(colour = type)) + 
        geom_bar(aes(type, weight = `%Wi/Wb`, colour = type)) +
        theme(axis.text.x = element_text(angle = -30, hjust = 0)) +
        labs(list(title = "Energy Usesage", x = "Mission Segment", y = "Percentage", colour = "Mission Segment"))
    })
    
    PlotPower <- MainIterationOut$Power %>% gather(key, value, -type, -R_total)
    PlotPower$type <- factor(PlotPower$type, levels = unique(PlotPower$type))
    PlotPower$key <- factor(PlotPower$key, levels = unique(PlotPower$key))
    
    output$PowerFacet <- renderPlot({
      ggplot(filter(PlotPower, key %in% c("Clmax", "Cl", "Cd", "ClCd", "theta", "Power")), 
             aes(x=R_total, colour = type, width = 2)) + 
        geom_line(aes(y = value)) + 
        facet_wrap(~key, scales = "free_y") +
        labs(list(title = "Mission Analysis", x = "Range", y = "", colour = "Mission Segment"))
    })
    
    
    #--- Allow a user to download power calcs
    output$downloadPower <- downloadHandler(
      filename = function() {
        paste(date()," Power Calcs", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(MainIterationOut$Power, file)
      }
    )
    
    output$PowerTable <- renderDataTable({
      MainIterationOut$PowerSummary
    })
    
    
  })
  
  
  
  # End shinyServer
})
