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
source("Helper Main Functions.R")

## Server ======================================================================
shinyServer(function(session, input, output) {
## Non-Reactive ======================================================================
  output$specs <- renderDataTable({
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
                 P0eng = input$P0eng, P0 = input$P0,
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
                 P0eng = input$P0eng, P0 = input$P0,
                 ClG = input$ClG, Cd0G = input$Cd0G, hground = input$hground
      )
    
    MainIterationOut <- MainIterationFunction(inputvals, specifications, out = "All")
    
    output$PowerTable <- renderDataTable({
      print.data.frame(MainIterationOut$Power)
    })
    
  })
  
  
  
  
  
  # End shinyServer
})
