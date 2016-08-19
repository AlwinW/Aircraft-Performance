#---NOTE: Ctrl + Shift + S runs the app

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

#---Load required scripts
source("Helper Standard Atmosphere.R")
source("Helper Calculation Functions.R")
source("Helper Numerical Methods.R")
source("Helper UI Functions.R")



shinyServer(function(input, output,session) {
  
## Specifications ======================================================================
  # Variables that may be called elsewhere
  Wp <- 120*6*9.8065; E <- 1e6; h_cruise <- 3000; h_ceil <- 3600; M <- 0.25
  # Data frame of specifications
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
  # Output the specifications as a table (this is not updated ever)
  output$specs <- renderTable(Specifications)
  
## Download Input Data ======================================================================
  observe({
    # Use the download handler in the UI
    output$downloadData <- downloadHandler(
      # Provide a filename of the data to be downloaded
      filename = function() {
        paste(date(), ".csv", sep = "")
      },
      # Provide the content of the file
      content = function(file) {
        write.csv(inputvals, file)
      }
    )
  })
  
## Upload Input Data ======================================================================
  # This *function* is repsonsible for loading in the selected file
  inputdata <- reactive({
    # Get the input object data
    infile <- input$uploadData
    if (is.null(infile)) {
      # Don't die if there is no file selected
      return(NULL)
    } else {
      # The function reads the csv of given file path
      read.csv(infile$datapath)
    }
  })
  
  # Update the current input values with the uploaded ones
  observe({
    inputdatavars <- inputdata()
    # Don't die if there is no file
    if (is.null(inputdatavars)) return(NULL)
    # Update inputs with new values
    updateNumericInput(session, "S", value = inputdatavars$S)
    updateNumericInput(session, "b", value = inputdatavars$b)
    updateNumericInput(session, "e", value = inputdatavars$e)
    updateNumericInput(session, "Cd0", value = inputdatavars$Cd0)
    updateNumericInput(session, "Clmax", value = inputdatavars$Clmax)
    updateNumericInput(session, "Clflaps", value = inputdatavars$Clflaps)
    updateNumericInput(session, "Clhls", value = inputdatavars$Clhls)
    updateNumericInput(session, "m", value = inputdatavars$m)
    updateNumericInput(session, "W", value = inputdatavars$W)
    updateNumericInput(session, "P0eng", value = inputdatavars$P0eng)
  })
  
## Calculate/Update Inputs ======================================================================
  observe({
    # Calculate required inputs
    # First make sure no errors are thrown when recalculating
    if (!is.na(input$S) & !is.na(input$b) & input$S*input$b != 0) 
      updateNumericInput(session, "AR", value = input$b^2/input$S)
    if (!is.na(input$e) & !is.na(input$AR) & input$e*input$AR != 0) 
      updateNumericInput(session, "K", value = 1/(pi * input$AR * input$e))
    if (!is.na(input$m) & input$m != 0) 
      updateNumericInput(session, "W", value = input$m * 9.8065)
    if (!is.na(input$W) & !is.na(input$S) & input$W * input$S != 0) 
      updateNumericInput(session, "WS", value = input$W/input$S)
    if (!is.na(input$P0eng) & input$P0eng != 0) 
      updateNumericInput(session, "P0", value = input$P0eng * 2)
    # Store the values as a data frame called inputvals
    inputvals <- data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                        Cd0 = input$Cd0, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
                        m = input$m, W = input$W, WS = input$WS,
                        P0eng = input$P0eng, P0 = input$P0
                        )
  })
## Aerodynmic Properties Table ======================================================================
  observe({
    # Get updated inputvals (reactive)
    inputvals <- data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                            Cd0 = input$Cd0, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
                            m = input$m, W = input$W, WS = input$WS,
                            P0eng = input$P0eng, P0 = input$P0
    )
    # Create the data using AeroParams in "Helper Calculation Functions.R"
    # Note: This requires h_ceil and h_cruise to be global variables
    AeroParamsTable <- AeroParams(inputvals) %>%
      select(type, h, Vinf, qinf, Cl, Cd, ClCd, Clstar, Cd, Cdstar, ClCdstar, Vstar, Cl32, Cd32, ClCd32, V32)
    # Output the table
    output$AeroParamsTable <- renderTable(t(AeroParamsTable))
    
    # Create/Update the data for a plot of the drag polar
    AeroParamsPlot <- data.frame(Cl = seq(from=0, to=inputvals$Clmax, by=0.1))
    AeroParamsPlot <- mutate(AeroParamsPlot, Cd = Cd(inputvals$Cd0, inputvals$K, Cl))
    # Output the plot of the data
    output$AeroParamsPlot <- renderPlot({
      ggplot(data = filter(AeroParamsTable, type == "Sea Level")) + 
        geom_abline(intercept = 0, slope = AeroParamsTable$Clstar/AeroParamsTable$Cdstar[1], colour = "green4") +
        geom_path(data = AeroParamsPlot, aes(x = Cd, y = Cl)) +
        geom_point(aes(x = Cdstar, y = Clstar), colour = "green4") +
        geom_text(aes(x = Cdstar, y = Clstar, label = "(L/D)*"), hjust = 1, vjust = -0.5, colour = "green4") + 
        geom_point(aes(x = Cd32, y = Cl32), colour = "blue") +
        geom_text(aes(x = Cd32, y = Cl32, label = "(L^(3/2)/D)"), hjust = 0.25, vjust = 2, colour = "blue") + 
        geom_point(data = tail(AeroParamsPlot,1), aes(x = Cd, y = Cl), colour = "red") +
        geom_text(data = tail(AeroParamsPlot,1), aes(x = Cd, y = Cl), label = "Stall", hjust = 0.5, vjust = -1, colour = "red") + 
        expand_limits(x = 0, y = 0)
      })
    # Return values for user clicks, etc
    output$APP_info <- renderText({
      paste0(
        "click: ", xy_str(input$APP_click),
        "dblclick: ", xy_str(input$APP_dblclick),
        "hover: ", xy_str(input$APP_hover),
        "brush: ", xy_range_str(input$APP_brush)
      )
    })
  })
  
## Operating Window ======================================================================
  observe({
    # Get required plotting parameters
    nh <- input$OW_nh
    nv <- input$OW_nv
    maxh <- input$OW_maxh
    maxv <- input$OW_maxv
    # Create the plotting window
    operatingwindow  <-
      ThrustPowerCurves(input, 0, maxh, nh, 0, maxv, nv, 1, 250)
    # Find plotting limits
    OW_xlow <-
      operatingwindow %>% filter(Pexc >= 0) %>% arrange(Pexc) %>% select(Vinf)
    OW_xupp <- head(OW_xlow, 1)[[1]] * 1.2
    OW_xlow <- tail(OW_xlow, 1)[[1]] * 0.8
    
    # Ouput a plot of the Velocities
    output$OWP_plot <- renderPlot({
      ggplot(operatingwindow) +
        geom_path(aes(x = Vmin, y = h, colour = "Stall Speed")) +
        geom_path(aes(x = Vmin * 1.2, y = h, colour = "Safety Factor"
        )) +
        geom_path(aes(x = VmaxP, y = h, colour = "Maximum Speed")) +
        geom_path(aes(x = Vcruise, y = h, colour = "Cruise Target")) +
        scale_color_manual(values = c("Stall Speed" = "red", "Safety Factor" = "orange",
                                      "Maximum Speed" = "purple", "Cruise Target" = "blue")) +
        xlim(OW_xlow, OW_xupp)
    })
    # Return values for user clicks, etc
    output$OWP_info <- renderText({
      paste0(
        "click: ", xy_str(input$OWP_click),
        "dblclick: ", xy_str(input$OWP_dblclick),
        "hover: ", xy_str(input$OWP_hover),
        "brush: ", xy_range_str(input$OWP_brush)
      )
    })
    
    # Ouput a plot of the Excess Power
    output$OWV_plot <- renderPlot({
      ggplot(operatingwindow) +
        geom_point(data = filter(operatingwindow, Pexc >= 0),
                   aes(x = Vinf, y = h, colour = Pexc)) +
        geom_path(aes(x = Vmin, y = h), colour = "red") +
        geom_path(aes(x = Vmin * 1.2, y = h), colour = "orange") +
        geom_path(aes(x = VmaxP, y = h), colour = "purple") +
        geom_path(aes(x = Vcruise, y = h), colour = "blue") +
        scale_colour_gradientn(colours = brewer.pal(5, "RdYlGn"),
                               guide = "colourbar",
                               name = "Excess Power") +
        xlim(0, OW_xupp)
    })
    # Return values for user clicks, etc
    output$OWV_info <- renderText({
      paste0(
        "click: ", xy_str(input$OWV_click),
        "dblclick: ", xy_str(input$OWV_dblclick),
        "hover: ", xy_str(input$OWV_hover),
        "brush: ", xy_range_str(input$OWV_brush)
      )
    })
  })
  
## Power and Thrust Curves ======================================================================
  observe({
    # Get required plotting parameters
    minh <- input$PT_minh
    maxh <- input$PT_maxh
    nh <- input$PT_nh
    minv <- input$PT_minv
    maxv <- input$PT_maxv
    nv <- input$PT_nv
    # Create powerthrustcurves
    powerthrustcurves  <-
      ThrustPowerCurves(input, minh, maxh, nh, minv, maxv, nv, minv*0.8, maxv*1.2) # %>%
      # mutate(h = as.factor(h))
    
    # Ouput a plot of the Power Excess
    output$Pe_plot <- renderPlot({
      ggplot(powerthrustcurves,
             aes(x = Vinf, y = Pexc, group = h, colour = h)) +
        geom_path() + ggtitle("Power Excess") +
        geom_point(aes(x = V32, y = PA - PRmin), shape = 1) +
        geom_point(aes(x = Vstar, y = PA - TRmin * Vstar), shape = 2) +
        geom_point(aes(x = Vmin, y = Pexc(PA(sigma, P0), PR(Vmin, rho, W, S, Cd0, K))), colour="red") + 
        geom_point(aes(x = Vmin * 1.2, y = Pexc(PA(sigma, P0), PR(Vmin * 1.2, rho, W, S, Cd0, K))), colour="orange") 
    })
    # Return values for user clicks, etc
    output$Pe_info <- renderText({
      paste0(
        "click: ", xy_str(input$Pe_click),
        "hover: ", xy_str(input$Pe_hover)
      )
    })
    
    # Ouput a plot of the Thrust Excess
    output$Te_plot <- renderPlot({
      ggplot(powerthrustcurves,
             aes(x = Vinf, y = Texc, group = h, colour = h)) +
        geom_path() + ggtitle("Thrust Excess") 
    })
    # Return values for user clicks, etc
    output$Te_info <- renderText({
      paste0(
        "click: ", xy_str(input$Te_click),
        "hover: ", xy_str(input$Te_hover)
      )
    })
    
    # Ouput a plot of the Power Required
    output$PR_plot <- renderPlot({
      ggplot(powerthrustcurves, 
             aes(x = Vinf, y = PR, group = h, colour = h)) +
        geom_path() + ggtitle("Power Required") +
        geom_point(aes(x = V32, y = PRmin), shape = 1) +
        geom_point(aes(x = Vstar, y = TRmin * Vstar), shape = 2) +
        geom_point(aes(x = Vmin, y = PR(Vmin, rho, W, S, Cd0, K)), colour="red") + 
        geom_point(aes(x = Vmin * 1.2, y = PR(Vmin * 1.2, rho, W, S, Cd0, K)), colour="orange") 
    })
    # Return values for user clicks, etc
    output$PR_info <- renderText({
      paste0(
        "click: ", xy_str(input$PR_click),
        "hover: ", xy_str(input$PR_hover)
      )
    })
    
    # Ouput a plot of the Thrust Required
    output$TR_plot <- renderPlot({
      ggplot(powerthrustcurves,
             aes(x = Vinf, y = TR, group = h, colour = h)) + 
        geom_path() + ggtitle("Thrust Required") +
        geom_point(aes(x = V32, y = PRmin / V32), shape = 1) +
        geom_point(aes(x = Vstar, y = TRmin), shape = 2)
    })
    # Return values for user clicks, etc
    output$TR_info <- renderText({
      paste0(
        "click: ", xy_str(input$TR_click),
        "hover: ", xy_str(input$TR_hover)
      )
    })
    
    
  })
## Climb ======================================================================
  observe({
    # Get updated inputvals (reactive)
    inputvals <- data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                            Cd0 = input$Cd0, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
                            m = input$m, W = input$W, WS = input$WS,
                            P0eng = input$P0eng, P0 = input$P0)
    # 2nd Segment
    climbsegtwo <- inputvals
    climbsegtwo$h = 400*0.3
    climbsegtwo <- StandardAtomsphere(climbsegtwo)
    climb_Vmin <- Vmin(climbsegtwo$rho, climbsegtwo$W, climbsegtwo$S, climbsegtwo$Clmax)
    
    climbsegtwo <- cbind(climbsegtwo, Vinf = seq(climb_Vmin, 150,length.out = 51))
    
    climbsegtwo <- climbsegtwo %>%
      mutate(qinf = qinf(rho, Vinf),
             Cl = Cl(W, qinf, S),
             Cd = Cd(Cd0, K, Cl),
             D = D(qinf, S, Cd),
             PA = PA(sigma, P0),
             TA = TA(PA, Vinf),
             sint = (TA - D)/W,
             theta = asin(sint) * 180/pi,
             percentagegradient = sint*100
             )
    
    ggplot(climbsegtwo, aes(x=Vinf, y=percentagegradient)) + geom_path() + geom_hline(yintercept = 1.5)
    
    climb <- inputvals
    climb <- cbind(climb, 
                   h = rep(c(0, 120, 3000, 3600), each=51), 
                   Vinf = rep(seq(climb_Vmin*0.9, 150, length.out = 51),times=4))
    climb <- climb %>%
      StandardAtomsphere(.) %>%
      mutate(qinf = qinf(rho, Vinf),
             Cl = Cl(W, qinf, S),
             Cd = Cd(Cd0, K, Cl),
             D = D(qinf, S, Cd),
             Vmin = Vmin(rho, W, S, Clmax),
             Vstar = Vstar(rho, W, S, K, Cd0),
             PA = PA(sigma, P0),
             TA = TA(PA, Vinf),
             ClimbRate = (PA - D*Vinf)/W
             ) %>%
      mutate(h = as.factor(h))
    
    ggplot(climb, aes(x=Vinf, y = ClimbRate / 0.3 * 60, group = h, colour = h)) + geom_path() +
      geom_point(aes(x=Vstar*0.76, y = 1000)) +
      geom_point(aes(x=Vmin*1.2, y = 1000), shape = 2)
    })

  
  
  # # Uncomment ONLYL for debugging - otherwise, the app will no longer be reactive
  # input <- data.frame(S = 54.4, b = 25.90, AR = 12.33, e = 0.8, K = 0.0323, Cd0 = 0.02, Clmax = 1.5, Clflaps = 0.80, Clhls = 1.20,
  #                     m = 15806, W = 155000, WS = 2850, P0eng = 1530000, P0 = 3060000,
  #                     OW_nv = 51, OW_nh = 51, OW_maxh = 12500, OW_maxv = 200,
  #                     PT_minh = 0, PT_maxh = 4000, PT_nh = 11, PT_minv = 40, PT_maxv = 100, PT_nv = 51)
})
