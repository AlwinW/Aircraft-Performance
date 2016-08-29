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
library(reshape2)

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
    updateNumericInput(session, "alt_s", value = inputdatavars$alt_s)
    
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
                 P0eng = input$P0eng, P0 = input$P0, Etatotal = input$Etatotal, alt_s = input$alt_s,
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
                 P0eng = input$P0eng, P0 = input$P0, Etatotal = input$Etatotal, alt_s = input$alt_s,
                 ClG = input$ClG, Cd0G = input$Cd0G, hground = input$hground
      )
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Computing:", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    # Create a callback function to update progress.
    
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (20 - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
    
    MainIterationOut <- suppressWarnings(MainIterationFunction(inputvals, specifications, out = "All", updateProgress = updateProgress))
    
## Summary ======================================================================
    output$SummaryTable <- renderDataTable({
      MainIterationOut$summary
    })
    
## AeroParams ======================================================================
    AeroParams <- suppressWarnings(AeroParamsFunction(inputvals, specifications))
    
    output$AeroParamsTable <- renderDataTable({
      MainIterationOut$AeroParamsTable
    })
    
    output$AeroParamsPlot <- renderPlot({
      slope = AeroParams$AeroParamsPlotPoints$Cl[3]/AeroParams$AeroParamsPlotPoints$Cd[3]
      ggplot(AeroParams$AeroParamsPlotPoints,
             aes(x = Cd, y = Cl, colour = type)) +
        geom_abline(intercept = 0, slope = slope, colour = "green4") + 
        geom_line(data = AeroParams$AeroParamsPlot,
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
    
## Operating Window ======================================================================
    # Get required plotting parameters
    nh <- input$OW_nh
    nv <- input$OW_nv
    maxh <- input$OW_maxh
    maxv <- input$OW_maxv
    # Create the plotting window
    operatingwindow  <-
      ThrustPowerCurves(inputvals, specifications, 0, maxh, nh, 0, maxv, nv, 1, 250)
    # Find plotting limits
    OW_xlow <-
      operatingwindow %>% arrange(Pexc) %>% select(Vinf)
    OW_xupp <- head(OW_xlow, 1)[[1]] * 1.1
    OW_xlow <- tail(OW_xlow, 1)[[1]] * 0.9
    
    # Ouput a plot of the Excess Power
    output$OperatingWindowPowerPlot <- renderPlot({
      ggplot(operatingwindow) +
        geom_point(data = filter(operatingwindow, Pexc >= 0),
                   aes(x = Vinf, y = h, colour = Pexc)) +
        geom_path(aes(x = Vmin, y = h), colour = "red") +
        geom_path(aes(x = Vmin * 1.2, y = h), colour = "orange") +
        geom_path(aes(x = VmaxP, y = h), colour = "purple") +
        geom_path(aes(x = Vstar, y = h), colour = "green") +
        geom_path(aes(x = Vcruise, y = h), colour = "blue") +
        scale_colour_gradientn(colours = brewer.pal(3, "RdYlGn"),
                               guide = "colourbar",
                               name = "Excess Power") +
        xlim(0, OW_xupp)+
        labs(list(title = "Excess Power and Height", x = "Vinf (m/s)", y = "Altitude (m)", colour = "Excess Power"))
    })
    
    # Ouput a plot of the Velocities
    output$OperatingWindowPlot <- renderPlot({
      ggplot(operatingwindow) +
        geom_path(aes(x = Vmin, y = h, colour = "Stall Speed")) +
        geom_path(aes(x = Vmin * 1.2, y = h, colour = "Safety Factor 1.2")) +
        geom_path(aes(x = VmaxP, y = h, colour = "Maximum Speed")) +
        geom_path(aes(x = Vstar, y = h, colour = "(L/D)*")) +
        geom_path(aes(x = Vcruise, y = h, colour = "Cruise Specification")) +
        scale_color_manual(values = c("Stall Speed" = "red", "Safety Factor 1.2" = "orange",
                                      "Maximum Speed" = "purple", "Cruise Specification" = "blue",
                                      "(L/D)*" = "green")) +
        xlim(OW_xlow, OW_xupp) +
        labs(list(title = "Velocities and Height", x = "Vinf (m/s)", y = "Altitude (m)", colour = "Velocity"))
    })
    
## Climb ======================================================================
    heights <- data.frame(type = c("Sea Level", "2nd Seg", "2nd Seg OEI", "Cruise", "Ceiling"),
                          h = c(0, 35*0.3048, 35*0.3048, 10000*0.3048, 12000*0.3048),
                          Ne = c(2, 2, 1, 2, 2))
    Climb <- ClimbFunction(inputvals, specifications, heights)
    Climb$type <- factor(Climb$type, levels = heights$type, ordered = TRUE)
    
    # Graph of Percentage Gradients
    output$PerGradPlot <- renderPlot({
      ggplot(Climb, aes(x=Vinf, y=PerGrad, group = type, colour = type)) + 
        geom_path() + 
        geom_point(aes(shape = Vname, size = ifelse(Vname == "Vinf", 0, 1))) + 
        geom_hline(aes(yintercept = 1.5, colour = "2nd Seg OEI")) +
        geom_text(aes(x = min(Vinf), y = 1.5, colour = "2nd Seg OEI"), 
                  label = "Minimum 2nd Seg Climb OEI", hjust = 0, vjust = 1.5,
                  show.legend = FALSE) + 
        scale_size(range = c(0,3)) + 
        scale_shape_manual(values = c("Vcruise" = 1, "Vflaps" = 3, "Vinf" = 1, "Vsafe" = 0, "Vstall" = 2)) +
        guides(size = FALSE) +
        labs(list(title = "Percentage Graidents", x = "Vinf (m/s)", y = "Percentage Gradient (%)", 
                  colour = "Mission Segment", shape = "Velocity"))
    })
    
    output$ClimbRatePlot <- renderPlot({
      ggplot(Climb, aes(x=Vinf, ClimbRate / 0.3 * 60, group = type, colour = type)) + 
        geom_path() + 
        geom_point(aes(shape = Vname, size = ifelse(Vname == "Vinf", 0, 1))) + 
        geom_hline(aes(yintercept = 100, colour = "Ceiling")) +
        geom_text(aes(x = min(Vinf), y = 100, colour = "Ceiling"), 
                  label = "Minimum Ceiling Rate of Climb", hjust = 0, vjust = 1.5,
                  show.legend = FALSE) + 
        geom_hline(aes(yintercept = 300, colour = "Cruise")) +
        geom_text(aes(x = min(Vinf), y = 300, colour = "Cruise"), 
                  label = "Minimum Cruise Rate of Climb", hjust = 0, vjust = 1.5,
                  show.legend = FALSE) + 
        scale_size(range = c(0,3)) + 
        scale_shape_manual(values = c("Vcruise" = 1, "Vflaps" = 3, "Vinf" = 1, "Vsafe" = 0, "Vstall" = 2)) +
        guides(size = FALSE) +
        labs(list(title = "Climb Rates (Vv)", x = "Vinf (m/s)", y = "Climb Rate (ft/min)", 
                  colour = "Mission Segment", shape = "Velocity"))
    })
    
    output$ClimbAnglePlot<- renderPlot({
      ggplot(Climb, aes(x=Vinf, Theta, group = type, colour = type)) + 
        geom_path() + 
        geom_point(aes(shape = Vname, size = ifelse(Vname == "Vinf", 0, 1))) + 
        scale_size(range = c(0,3)) + 
        scale_shape_manual(values = c("Vcruise" = 1, "Vflaps" = 3, "Vinf" = 1, "Vsafe" = 0, "Vstall" = 2)) +
        guides(size = FALSE) +
        labs(list(title = "Climb Angle (Theta)", x = "Vinf (m/s)", y = "Theta (degrees)", 
                  colour = "Mission Segment", shape = "Velocity"))
    })
    
    heightsall <- data.frame(type = seq(0, 4000, 250),
                             h = seq(0, 4000, 250),
                             Ne = 2)
    Climball <- ClimbFunction(inputvals, specifications, heightsall)
    output$ClimbRateAllPlot <- renderPlot({
      ggplot(Climball, aes(x=Vinf, ClimbRate / 0.3 * 60, group = type, colour = type)) + 
        geom_point(aes(shape = Vname, size = ifelse(Vname == "Vinf", 0, 1))) + 
        scale_size(range = c(0,3)) + 
        scale_shape_manual(values = c("Vcruise" = 1, "Vflaps" = 3, "Vinf" = 1, "Vsafe" = 0, "Vstall" = 2)) +
        guides(size = FALSE) +
        labs(list(title = "Climb Rates At Various Altitudes (Vv)", x = "Vinf (m/s)", y = "Climb Rate (ft/min)", 
                  colour = "Mission Segment", shape = "Velocity"))
    })
    
    
## Takeoff ======================================================================
    Takeoff <- MainIterationOut[c("AccelerateStop","AccelerateContinue", "AccelerateLiftoff", "BFL")]
    BFL <- Takeoff$BFL
    Takeoff <- data.frame(
                     Vinf = Takeoff$AccelerateStop$Vinf, 
                     `AccelerateStop` = Takeoff$AccelerateStop$AccelerateStop,
                     `AccelerateContinue` = Takeoff$AccelerateContinue$AccelerateContinue, 
                     `AccelerateContinueGround` = Takeoff$AccelerateContinue$AccelerateContinue - Takeoff$AccelerateContinue$`Air Distance`,
                     `AccelerateLiftoff` = Takeoff$AccelerateLiftoff$AccelerateLiftoff)
    Takeoff <- Takeoff %>% gather(key, value, - Vinf)
    Takeoff <- Takeoff %>% mutate(dist = ifelse(key == "AccelerateContinueGround", "Ground", "Ground & Air"))
    # Takeoff <- filter(Takeoff, key!= "AccelerateLiftoff" & Vinf <= BFL$Vlof)  
    
    output$TakeoffFieldLengthPlot <- renderPlot({
      ggplot(Takeoff, aes(x = Vinf, y = value, colour = key)) +
        geom_line(aes(linetype = dist)) + 
        geom_text(aes(x = 0, y = as.double(tail(filter(Takeoff, key == "AccelerateLiftoff"),1)$value), 
                      colour = "AccelerateLiftoff"), 
                  label = "1.15 x Runway Distance for \nNormal Takeoff with 2 Engines", hjust = 0, vjust = 1.1,
                  show.legend = FALSE) +
        geom_hline(aes(yintercept = 1200, colour = "Maximum")) +
        geom_text(aes(x = 0, y = 1200, colour = "Maximum"), 
                  label = "Maximum Runway Length", hjust = 0, vjust = -0.5,
                  show.legend = FALSE) +
        geom_hline(aes(yintercept = BFL$BFL, colour = "BFL"), linetype = 3, show.legend = FALSE) + 
        geom_vline(aes(xintercept = BFL$Vinf, colour = "BFL"), linetype = 3, show.legend = FALSE) +
        geom_point(data = BFL, aes(x = Vinf, y = BFL, colour = "BFL")) +
        geom_text(aes(x = BFL$Vinf, y = 0, colour = "BFL"), 
                  label = "V1", hjust = 0.5, vjust = 0.5,
                  show.legend = FALSE) +
        geom_vline(aes(xintercept = BFL$Vlof, colour = "AccelerateLiftoff"), linetype = 6, show.legend = FALSE) +
        geom_text(aes(x = BFL$Vlof, y = 0, colour = "AccelerateLiftoff"), 
                  label = "V2", hjust = 0.5, vjust = -1.5,
                  show.legend = FALSE) +
        labs(list(title = "Takeoff Runway Balanced Field Length", 
                  x = "Velocity at Engine Loss (m/s)", y = "Runway Distance Required (m)", 
                  colour = "Scenario:", linetype = "Distance:")) +
        scale_linetype_manual(values = c("Ground" = 5, "Ground & Air" = 1)) + 
        ylim(0, NA) + 
        theme(legend.position = "bottom")
    })
    
    
## Mission Analysis ======================================================================
    output$PowerSummary <- renderPlot({
      ggplot(mutate(MainIterationOut$BatteryFracs, type = factor(type, levels = type)), 
             aes(colour = type)) + 
        geom_bar(aes(type, weight = `%Wi/Wb`, colour = type)) +
        theme(axis.text.x = element_text(angle = -30, hjust = 0)) +
        labs(list(title = "Energy Usesage", x = "Mission Segment", y = "Percentage", colour = "Mission Segment"))
    })
    
    PlotPower <- MainIterationOut$Power %>% mutate(`D x V` = Drag * Vinf) %>% gather(key, value, -type, -R_total)
    PlotPower$type <- factor(PlotPower$type, levels = unique(PlotPower$type))
    PlotPower$key <- factor(PlotPower$key, levels = unique(PlotPower$key))
    
    output$PowerFacet <- renderPlot({
      ggplot(filter(PlotPower, key %in% c("Clmax", "Cl", "Cd", "ClCd", "theta", "Power")), 
             aes(x=R_total, colour = type, width = 2)) + 
        geom_line(aes(y = value)) + 
        facet_wrap(~key, scales = "free_y") +
        labs(list(title = "Mission Analysis", x = "Range", y = "", colour = "Mission Segment"))
    })
    
    ## To do up better later ####
    output$MissionInput <- renderPlot({
      ggplot(filter(PlotPower, key %in% c("Vinf", "h")), 
             aes(x=R_total, colour = type, width = 2)) + 
        geom_line(aes(y = value)) + 
        facet_wrap(~key, scales = "free_y") +
        labs(list(title = "Input Values", x = "Range", y = "", colour = "Mission Segment"))
    })
    
    output$MissionParams <- renderPlot({
      ggplot(filter(PlotPower, key %in% c("Cl", "Cd", "ClCd", "theta","Drag", "D x V")), 
             aes(x=R_total, colour = type, width = 2)) + 
        geom_line(aes(y = value)) + 
        facet_wrap(~key, scales = "free_y", ncol = 2) +
        labs(list(title = "Calculated Parameters", x = "Range", y = "", colour = "Mission Segment"))
    })
    
    output$MissionOutput <- renderPlot({
      ggplot(filter(PlotPower, key %in% c("Power", "Wb_total")), 
             aes(x=R_total, colour = type, width = 2)) + 
        geom_line(aes(y = value)) + 
        facet_wrap(~key, scales = "free_y") +
        labs(list(title = "Power Usage and Total Battery Weight", x = "Range", y = "", colour = "Mission Segment"))
      
    })
    
    output$WeightFracs <- renderPlot({
      WeightFracs <- MainIterationOut$WeightFracs
      WeightFracs$Description <- factor(WeightFracs$Description, levels = WeightFracs$Description)
      ggplot(WeightFracs[1:3,1:2]) + geom_bar(aes(Description, weight = Value, colour = Description)) +
        geom_text(aes(x = Description, y = Value/2, label = round(Value, 4)), colour="white")  + 
        labs(list(title = "Weight Fractions", x = "Weight", y = "Fraction", colour = "Mission Segment"))
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
