#---NOTE: Ctrl + Shift + S runs the app
#---NOTE: make conversions from m to ft more accurate

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
              1000, 35, 100, 1.5, 300, 100, 10000, 12000,
              M, 1000, -1.5, 3.5))
  # Output the specifications as a table (this is not updated ever)
  output$specs <- renderDataTable(Specifications)
  
## Download Input Data ======================================================================
  observe({
    # Get updated inputvals (reactive)
    inputvals <- 
      data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                 Cd0 = input$Cd0, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
                 m = input$m, W = input$W, WS = input$WS,
                 P0eng = input$P0eng, P0 = input$P0,
                 ClG = input$ClG, Cd0G = input$Cd0G, hground = input$hground
      )
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
    # if (!is.na(input$S) & !is.na(input$b) & input$S*input$b != 0) 
    #   updateNumericInput(session, "AR", value = input$b^2/input$S)
    
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
    # Store the values as a data frame called inputvals
    inputvals <- 
      data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                 Cd0 = input$Cd0, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
                 m = input$m, W = input$W, WS = input$WS,
                 P0eng = input$P0eng, P0 = input$P0,
                 ClG = input$ClG, hground = input$hground
      )
  })
  
## Aerodynmic Properties Table ======================================================================
  observe({
    # Get updated inputvals (reactive)
    inputvals <- 
      data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                 Cd0 = input$Cd0, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
                 m = input$m, W = input$W, WS = input$WS,
                 P0eng = input$P0eng, P0 = input$P0,
                 ClG = input$ClG, Cd0G = input$Cd0G, hground = input$hground
      )
    # Create the data using AeroParams in "Helper Calculation Functions.R"
    # Note: This requires h_ceil and h_cruise to be global variables
    AeroParamsTable <- AeroParams(inputvals) %>%
      select(type, h, Vinf, Vstall, Vsafe, qinf, Cl, Cd, ClCd, Clstar, Cd, Cdstar, ClCdstar, Vstar, Cl32, Cd32, ClCd32, V32)
    # Output the table
    output$AeroParamsTable <- renderDataTable((AeroParamsTable))
    
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
    inputvals <- 
      data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                 Cd0 = input$Cd0, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
                 m = input$m, W = input$W, WS = input$WS,
                 P0eng = input$P0eng, P0 = input$P0,
                 ClG = input$ClG, Cd0G = input$Cd0G, hground = input$hground
      )
    
    # Climb in general
    heights <- data.frame(type = c("Sea Level", "2nd Seg Climb", "3rd Seg Accel", "Cruise", "Ceiling"),
                          h = c(0, 35*0.3, 400*0.3, h_cruise, h_ceil))
    climb <- ClimbRates(inputvals, heights)
    
    # Graph of Percentage Gradients
    output$PG_plot <- renderPlot({
      ggplot(climb, aes(x=Vinf, y=PercentageGradient, group = type, colour = type)) + 
        geom_path() + 
        geom_point(aes(shape = Vname, size = ifelse(Vname == "Vinf", 0, 1))) + 
        geom_hline(aes(yintercept = 1.5, colour = "2nd Seg Climb")) +
        geom_text(aes(x = 80, y = 1.5, colour = "2nd Seg Climb"), 
                  label = "Minimum 2nd Seg Climb", hjust = 0, vjust = -0.5,
                  show.legend = FALSE) + 
        scale_size(range = c(0,3)) + 
        scale_shape_manual(values = c("Vcruise" = 1, "Vflaps" = 3, "Vinf" = 1, "Vsafe" = 0, "Vstall" = 2)) +
        guides(size = FALSE)
    })
    
    # Graph of Climb Rates in ft per minute
    output$CA_plot <- renderPlot({
      ggplot(climb, aes(x=Vinf, Theta, group = type, colour = type)) + 
        geom_path() + 
        geom_point(aes(shape = Vname, size = ifelse(Vname == "Vinf", 0, 1))) + 
        scale_size(range = c(0,3)) + 
        scale_shape_manual(values = c("Vcruise" = 1, "Vflaps" = 3, "Vinf" = 1, "Vsafe" = 0, "Vstall" = 2)) +
        guides(size = FALSE)
    })
    
    # Graph of Climb Rates in ft per minute
    output$CR_plot <- renderPlot({
      ggplot(climb, aes(x=Vinf, ClimbRate / 0.3 * 60, group = type, colour = type)) + 
        geom_path() + 
        geom_point(aes(shape = Vname, size = ifelse(Vname == "Vinf", 0, 1))) + 
        geom_hline(aes(yintercept = 100, colour = "Ceiling")) +
        geom_text(aes(x = 80, y = 100, colour = "Ceiling"), 
                  label = "Minimum Ceiling Rate of Climb", hjust = 0, vjust = -0.5,
                  show.legend = FALSE) + 
        geom_hline(aes(yintercept = 300, colour = "Cruise")) +
        geom_text(aes(x = 80, y = 300, colour = "Cruise"), 
                  label = "Minimum Cruise Rate of Climb", hjust = 0, vjust = -0.5,
                  show.legend = FALSE) + 
        scale_size(range = c(0,3)) + 
        scale_shape_manual(values = c("Vcruise" = 1, "Vflaps" = 3, "Vinf" = 1, "Vsafe" = 0, "Vstall" = 2)) +
        guides(size = FALSE)
    })
    
    # Climb in general
    heightsall <- data.frame(type = as.factor(seq(0, 4000, 250)),
                             h = seq(0, 4000, 250))
    climball <- ClimbRates(inputvals, heightsall)
      
    # Graph of Climb Rates in ft per minute
    output$CRa_plot <- renderPlot({
      ggplot(climball, aes(x=Vinf, ClimbRate / 0.3 * 60, group = type, colour = type)) + 
        # geom_path() + 
        geom_point(aes(shape = Vname, size = ifelse(Vname == "Vinf", 0, 1))) + 
        scale_size(range = c(0,3)) + 
        scale_shape_manual(values = c("Vcruise" = 1, "Vflaps" = 3, "Vinf" = 1, "Vsafe" = 0, "Vstall" = 2)) +
        guides(size = FALSE)
    })
  })

  
  observe({
    inputvals <- 
      data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                 Cd0 = input$Cd0, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
                 m = input$m, W = input$W, WS = input$WS,
                 P0eng = input$P0eng, P0 = input$P0,
                 ClG = input$ClG, Cd0G = input$Cd0G, hground = input$hground
      )
    
    #----------------------------
    #--- Takeoff and Landing
    #============================
    # Using generalised methods
    
    groundmu <- data.frame(names = c("Dry Concrete", "Wet Concrete", "Icy Concrete"),
                           brakesoff = c(0.04, 0.05, 0.02), #NB: 0.03-0.05 for dry
                           brakeson_min = c(0.3, 0.15, 0.06),
                           brakeson_max = c(0.5, 0.3, 0.10)) %>%
      mutate(brakeson = (brakeson_min + brakeson_max)/2)
    
## Balanced Field Length (BFL) ======================================================================
    # Lift off speed: 1.1 * Vstall (min)
    # Transition speed: 1.15 * Vstall (approx)
    # Clim speed: 1.2 * Vstall (min)
    
    # Ground Effect
    Keff <- function(K, h, b)
      (33 * (h/b)^1.5) / (1 + 33 * (h/b)^1.5)  * K
    
    # Air Distance Function
    AirDist <- function(Vstall, g, T, D, W, hobs) {
      R <- (1.15 * Vstall) ^ 2 / (0.2 * 9.81)
      gamma = asin((T - D) / W)
      hTR <- R * (1 - cos(gamma))
      ST = R * ((T - D) / W)
      SC = (hobs) / tan(gamma)
      return(c(gamma, 0, SC, hTR))
    }
    
    # Requires airplane data + various velocities
    # Create a dataframe of the inital takeoff values and 3 cases
    takeoff <- data.frame(sapply(inputvals, rep.int, times = 3))
    takeoff$type <- c("All Engines", "One Engine Down", "Rejected Take-Off")
    takeoff$Ne <- c(2, 1, 0) # Change the last one from 0 to say -0.5 if reverse availalbe
    takeoff$mu <- c(as.double(filter(groundmu,names == "Dry Concrete") %>% select(brakesoff)),
                    as.double(filter(groundmu,names == "Dry Concrete") %>% select(brakesoff)),
                    as.double(filter(groundmu,names == "Dry Concrete") %>% select(brakeson)))
    takeoff <- takeoff %>%
      mutate(h = 0) %>%
      StandardAtomsphere(.) %>%
      mutate(Vstall = Vmin(rho, W, S, Clmax + Clflaps),
             Vlof = 1.1 * Vstall)
    
    # Need to find the upper 1.2 Vstall
    velocities <- seq(0.0001,takeoff$Vlof[1], length.out = 50)
    takeoff <- takeoff[rep(row.names(takeoff),each=length(velocities)),1:length(takeoff)]
    rownames(takeoff) <- NULL
    takeoff$Vinf <- rep(velocities,3)
    
    takeoff$ClG <-  inputvals$ClG
    takeoff$Keff <-  Keff(takeoff$K, inputvals$hground, takeoff$b)
    takeoff <- takeoff %>%
      mutate(Mach = Vinf/a, PA = PA(sigma, P0eng * Ne), TA = TA(PA, Vinf),
             Cd = Cd(Cd0G, Keff, ClG), qinf = qinf(rho, Vinf),
             D = D(qinf, S, Cd), L = L(qinf, S, ClG),
             Ff = (W - L) * mu, Fnet = TA - D - Ff, accel = Fnet/(W/g),
             arecip = 1/(2*accel), Vsq = Vinf^2) %>%
      group_by(type) %>%
      mutate(Area = 1/2 * (arecip + lag(arecip,1)) * (Vsq - lag(Vsq,1)),
             Area = ifelse(is.na(Area), 0, Area),
             Area = cumsum(Area),
             Area = ifelse(is.na(Area), 0, Area))
    
    # Find the distance flying through the air
    AirDistVals <- inputvals[rep(row.names(inputvals), each=2), 1:length(inputvals)]
    rownames(AirDistVals) <- NULL
    AirDistVals$type <- c("All Engines", "One Engine Down")
    AirDistVals$Ne <- c(2, 1)
    AirDistVals <- AirDistVals %>%
      mutate(h = 0) %>%
      StandardAtomsphere(.) %>%
      mutate(
        ClTR = Clmax + Clflaps,
        Vstall = Vmin(rho, W, S, ClTR),
        VTR = Vstall * 1.15,
        PA = PA(sigma, P0eng * Ne),
        TA = TA(PA, VTR),
        Cd = Cd(Cd0G, K, ClTR),
        qinf = qinf(rho, VTR),
        D = D(qinf, S, Cd),
        L = L(qinf, S, ClTR)
      ) %>%
      rowwise() %>%
      mutate(gamma = AirDist(Vstall, g, TA, D, W, 35*0.3)[1],
             ST = AirDist(Vstall, g, TA, D, W, 35*0.3)[2],
             SC = AirDist(Vstall, g, TA, D, W, 35*0.3)[3],
             hTC = AirDist(Vstall, g, TA, D, W, 35*0.3)[4]) %>% 
      ungroup()
    
    # Find the distance required to accelerate then stop after V1
    AccelerateStop <- takeoff %>%
      select(Vinf, type, Area) %>%
      spread(type, Area) %>%
      mutate(AccelerateStop = `All Engines` - `Rejected Take-Off`)
    
    # Find the distance required to accelerate then continue after failure at V1
    AccelerateContinue <- takeoff %>%
      select(Vinf, type, Area) %>%
      spread(type, Area) %>%
      mutate(`One Engine Down` = max(`One Engine Down`) - `One Engine Down`)
    AccelerateContinue$`Air Distance` = sum(filter(AirDistVals, type == "One Engine Down") %>% select(ST, SC))
    AccelerateContinue <- mutate(AccelerateContinue, AccelerateContinue = `All Engines` + `One Engine Down` + `Air Distance`)
    
    # Find the distannce required to take off with all engines * 1.15
    AccelerateLiftoff <- takeoff %>%
      select(Vinf, type, Area) %>%
      spread(type, Area)
    AccelerateLiftoff <- tail(AccelerateLiftoff,1)
    AccelerateLiftoff$`Air Distance` = sum(filter(AirDistVals, type == "All Engines") %>% select(ST, SC))
    AccelerateLiftoff <- mutate(AccelerateLiftoff, AccelerateLiftoff = (`All Engines` + `Air Distance`) * 1.15)
    
    # Find the value of BFL
    BFL <- AccelerateStop %>% select(Vinf, AccelerateStop)
    BFL$AccelerateContinue <- AccelerateContinue$AccelerateContinue
    BFL <- mutate(BFL, diff = AccelerateContinue - AccelerateStop,
                  root = sign(diff*lag(diff,1)),
                  root = root*lead(root,1)) %>%
      filter(root == -1) %>%
      arrange(abs(diff))
    
    
    output$TFLout <- renderDataTable({
      BFL
    })
    
    output$TFL_plot <- renderPlot({
      ggplot() + 
        geom_path(data = AccelerateStop, aes(x = Vinf, y = AccelerateStop, colour = "Accelerate-Stop")) +
        geom_path(data = AccelerateContinue, aes(x = Vinf, y = AccelerateContinue, colour = "Accelerate-Continue")) +
        geom_point(data = AccelerateLiftoff, aes(x = Vinf, y = AccelerateLiftoff, colour = "Accelerate-Liftoff")) +
        geom_hline(aes(yintercept = AccelerateLiftoff$AccelerateLiftoff, colour = "Accelerate-Liftoff")) +
        geom_hline(aes(yintercept = 1200, colour = "Maximum"))
    })
      
      # In the future iteratively decrease the vstep until a solution is found for the curve intersections
  })
  
  observe({
    inputvals <- 
      data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                 Cd0 = input$Cd0, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
                 m = input$m, W = input$W, WS = input$WS,
                 P0eng = input$P0eng, P0 = input$P0,
                 ClG = input$ClG, Cd0G = input$Cd0G, hground = input$hground
      )
    
    #----------------------------
    #--- Power Calcs
    #============================
    
    # Take-off 
    
    # Segment 1
    # should be covered in takeoff
    
    # Segment 2
    # Flying at climb speed V2 with landing gear up and flaps in takeoff position
    Pseg2num <- 10
    Pseg2Heights <- seq(35*0.3, 400*0.3, length.out = Pseg2num)
    
    Pseg2 <- cbind(inputvals, h = Pseg2Heights, type = "2nd Segment Climb") %>%
      StandardAtomsphere(.) %>%
      mutate(Clmax = Clmax + Clflaps,
             Cd0 = Cd0,
             Vstall = Vmin(rho, W, S, Clmax),
             Vinf = Vstall * 1.2,
             qinf = qinf(rho, Vinf),
             PA = PA(sigma, P0)) %>%
      rowwise() %>%
      mutate(theta = ClimbRatesFunction(PA, Cd0, rho, Vinf, S, K, W),
             Vv = Vinf * sin(theta * pi / 180),
             Vh = Vinf * cos(theta * pi / 180),
             Cl = W * cos(theta * pi / 180) / (qinf  * S),
             Cd = Cd0 + K*Cl^2,
             ClCd = Cl/Cd) %>%
      ungroup() %>%
      mutate(duration = 2 * (h - lag(h,1)) / (Vv + lag(Vv,1)),
             duration = ifelse(is.na(duration), 0, duration),
             t = cumsum(duration)) %>%
      mutate(Eduration = 1/2 * (PA + lag(PA,1)) * duration,
             Eduration = ifelse(is.na(Eduration), 0, Eduration),
             Eeng = cumsum(Eduration),
             Wb100 = Eeng / 1e6,
             Rduration = 1/2 * (Vh + lag(Vh,1)) * duration,
             Rduration = ifelse(is.na(Rduration), 0, Rduration),
             R = cumsum(Rduration)) %>%
      select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, t, Eeng, Wb100, R, duration, Eduration, Rduration)
    
    # Segment 3
    # Flying at constant altitude accelerating from V2 to VFS or 1.25 VS with flaps in TO
    Pseg3num <- 15
    Pseg3Velocities <- seq(tail(Pseg2$Vinf, 1), as.double(filter(AeroParamsTable, type == "Cruise") %>% select(Vinf)), length.out = Pseg3num)
    
    Pseg3 <- cbind(inputvals, Vinf = Pseg3Velocities)
    
    Pseg3 <- Pseg3 %>%
      mutate(h = tail(Pseg2$h,1), type = "3nd Segment Acceleration") %>%
      StandardAtomsphere(.) %>%
      mutate(Clmax = Clmax + Clflaps,
             Cd0 = Cd0,
             Vstall = Vmin(rho, W, S, Clmax),
             qinf = qinf(rho, Vinf),
             PA = PA(sigma, P0),
             TA = TA(PA, Vinf)) %>%
      mutate(Cl = W/(qinf * S),
             Cd = Cd0 + K * Cl^2,
             ClCd = Cl/Cd,
             D = D(qinf, S, Cd),
             accel =  (TA - D)/m) %>%
      mutate(duration = (Vinf - lag(Vinf,1)) / accel,
             duration = ifelse(is.na(duration), 0, duration),
             t = cumsum(duration)) %>%
      mutate(Eduration = 1/2 * (PA + lag(PA,1)) * duration,
             Eduration = ifelse(is.na(Eduration), 0, Eduration),
             Eeng = cumsum(Eduration),
             Wb100 = Eeng / 1e6,
             Rduration = 1/2 * (Vinf + lag(Vinf,1)) * duration,
             Rduration = ifelse(is.na(Rduration), 0, Rduration),
             R = cumsum(Rduration)) %>%
      select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, t, Eeng, Wb100, R, duration, Eduration, Rduration)
    
    # Segment 4
    # Flying at climb speed Vcruise with landing gear up and flaps retracted
    Pseg4num <- 15
    Pseg4Heights <- seq(tail(Pseg3$h, 1), h_cruise, length.out = Pseg4num)
    
    Pseg4 <- cbind(inputvals, h = Pseg4Heights, type = "4th Segment Climb") %>%
      StandardAtomsphere(.) %>%
      mutate(Clmax = Clmax,
             Cd0 = Cd0,
             Vstall = Vmin(rho, W, S, Clmax),
             Vinf = tail(Pseg3$Vinf, 1),
             qinf = qinf(rho, Vinf),
             PA = PA(sigma, P0)) %>%
      rowwise() %>%
      mutate(theta = ClimbRatesFunction(PA, Cd0, rho, Vinf, S, K, W),
             Vv = Vinf * sin(theta * pi / 180),
             Vh = Vinf * cos(theta * pi / 180),
             Cl = W * cos(theta * pi / 180) / (qinf  * S),
             Cd = Cd0 + K*Cl^2,
             ClCd = Cl/Cd) %>%
      ungroup() %>%
      mutate(duration = 2 * (h - lag(h,1)) / (Vv + lag(Vv,1)),
             duration = ifelse(is.na(duration), 0, duration),
             t = cumsum(duration)) %>%
      mutate(Eduration = 1/2 * (PA + lag(PA,1)) * duration,
             Eduration = ifelse(is.na(Eduration), 0, Eduration),
             Eeng = cumsum(Eduration),
             Wb100 = Eeng / 1e6,
             Rduration = 1/2 * (Vh + lag(Vh,1)) * duration,
             Rduration = ifelse(is.na(Rduration), 0, Rduration),
             R = cumsum(Rduration)) %>%
      select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, t, Eeng, Wb100, R, duration, Eduration, Rduration)
    
    
    Power <- rbind(Pseg2, Pseg3, Pseg4) %>%
      mutate(t_total = cumsum(duration),
             Eeng_total = cumsum(Eduration),
             R_total = cumsum(Rduration),
             Wb84_total = Eeng_total/(1e6 * 0.84))
    
    output$RoughPower <- renderPrint({
      print.data.frame(Power)
    })
    
  })
  
  
  # # Uncomment ONLYL for debugging - otherwise, the app will no longer be reactive
  # input <- data.frame(S = 54.4, b = 25.90, AR = 12.33, e = 0.8, K = 0.0323,
  #                     Cd0 = 0.02, Clmax = 1.5, Clflaps = 0.80, Clhls = 1.20,
  #                     m = 15806, W = 155000, WS = 2850, P0eng = 1530000, P0 = 3060000,
  #                     OW_nv = 51, OW_nh = 51, OW_maxh = 12500, OW_maxv = 200,
  #                     PT_minh = 0, PT_maxh = 4000, PT_nh = 11, PT_minv = 40, PT_maxv = 100, PT_nv = 51,
  #                     ClG = 0.25, Cd0G = 0.03, hground = 2.0)
  
  ## General Weight Fractions ======================================================================
  observe({
    Wpp <- data.frame(WppW = 720 / input$m)
    
    Wb <- filter(AeroParamsTable, type == "Cruise") %>%
      StandardAtomsphere(.) %>%
      mutate(D = D(qinf, input$S, Cd)) %>%
      select(type, h, Vinf,  rho, qinf, Cl, Cd, ClCd, Clstar, Cdstar,
             ClCdstar, Vstar, D) %>%
      mutate(Eeng = D * 1200e3,
             eta = 0.84,
             Wb = Eeng/(1e6 * eta),
             Vb = Wb/2.75e3,
             WbW = Wb/input$m)
    
    output$GenWeightFracs1 <- renderPrint({
      print.data.frame(Wpp)
      print.data.frame(Wb)
      print(1 - Wpp$WppW - Wb$WbW)
    })
    
  })
  
})
