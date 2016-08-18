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
source("Helper Numerical Methods.R")

xy_str <- function(e) {
  if(is.null(e)) return("NULL\n")
  paste0("x=", round(e$x, 4), " y=", round(e$y, 4), "\n")
}
xy_range_str <- function(e) {
  if(is.null(e)) return("NULL\n")
  paste0("xmin=", round(e$xmin, 4), " xmax=", round(e$xmax, 4), 
         " ymin=", round(e$ymin, 4), " ymax=", round(e$ymax, 4))
}

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
  
  observe({
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(inputvals, file)
      }
    )
  })
  
  # This function is repsonsible for loading in the selected file
  inputvarsdata <- reactive({
    infile <- input$uploadData
    if (is.null(infile)) {
      return(NULL)
    }
    else {
      read.csv(infile$datapath)
    }
    # output$specs <- renderTable(inputvalsdata)
  })
  
  observe({
    df <- inputvarsdata()
    if (is.null(df)) return(NULL)
    
    output$testout <- renderText(df$AR)
  })
  

  
  
  
  # updateNumericInput(session, "AR", value = inputvarsdata$AR)
  
  
  
  
  #---Inputs
  observe({
    # Calculate required inputs
    # inS <- input$S
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
    inputvals <- data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                        Cd0 = input$Cd0, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
                        m = input$m, W = input$W, WS = input$WS,
                        P0eng = input$P0eng, P0 = input$P0
                        )
    
    # Use cbind to combine into a dataframe
  })

  #---Aerodynamic Properties
  observe({
    inputvals <- data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
                            Cd0 = input$Cd0, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
                            m = input$m, W = input$W, WS = input$WS,
                            P0eng = input$P0eng, P0 = input$P0
    )
    
    AeroParamsTable <- AeroParams(inputvals) %>%
      select(type, h, Vinf, qinf, Cl, Cd, ClCd, Clstar, Cd, Cdstar, ClCdstar, Vstar, Cl32, Cd32, ClCd32, V32)
    output$AeroParamsTable <- renderTable(t(AeroParamsTable))
    
    AeroParamsPlot <- data.frame(Cl = seq(from=0, to=inputvals$Clmax, by=0.1))
    AeroParamsPlot <- mutate(AeroParamsPlot, Cd = Cd(inputvals$Cd0, inputvals$K, Cl))
    output$AeroParamsPlot <- renderPlot({
      ggplot(AeroParamsPlot, aes(x=Cd, y=Cl)) + geom_path() +
        geom_point(aes(x=Cdstar(inputvals$Cd0), y=Clstar(inputvals$Cd0, inputvals$K))) +
        expand_limits(x = 0, y = 0)
      })
    output$APP_info <- renderText({
      paste0(
        "click: ", xy_str(input$APP_click),
        "dblclick: ", xy_str(input$APP_dblclick),
        "hover: ", xy_str(input$APP_hover),
        "brush: ", xy_range_str(input$APP_brush)
      )
    })
  })
  
  #---Operating Window
  observe({
    nh <- input$OW_nh
    nv <- input$OW_nv
    maxh <- input$OW_maxh
    maxv <- input$OW_maxv
    
    operatingwindow  <-
      data.frame(h = rep(seq(0, maxh, length.out = nh), each = nv),
                 Vinf = rep(seq(0, maxv, length.out = nv), times = nh))
    operatingwindow <- operatingwindow %>%
      mutate(
        W = input$W,
        S = input$S,
        Cd0 = input$Cd0,
        P0 = input$P0,
        Clmax = input$Clmax
      )
    operatingwindow$K = input$K 
    operatingwindow <- operatingwindow %>%
      StandardAtomsphere(.) %>%
      group_by(h) %>%
      mutate(
        qinf = qinf(rho, Vinf),
        Cl = Cl(W, qinf, S),
        Cd = Cd(Cd0, K, Cl),
        ClCd = ClCd(Cl, Cd),
        ClCdstar = ClCdstar(Cd0, K),
        Vmin = Vmin(rho, W, S, Clmax),
        PRmin = PRmin(rho, W, S, Cd0, K),
        PR = PR(Vinf, rho, W, S, Cd0, K),
        TRmin = TRmin(W, ClCdstar),
        TR = TR(W, ClCd),
        Vstar = Vstar(rho, W, S, K, Cd0),
        V32 = V32(Vstar),
        PA = PA(sigma, P0),
        Pexc = Pexc(PA, PR),
        TA = TA(PA, Vinf),
        Texc = Texc(TA, TR)
      ) %>%
      rowwise() %>%
      mutate(VmaxP = VmaxP(PA, rho, W, S, Cd0, K, 1, 250))
    
    # Find plotting limits
    OW_xlow <- operatingwindow %>% filter(Pexc >= 0) %>% arrange(-Pexc) %>% filter(row_number() == 1) %>% select(Vinf)
    OW_xlow <- OW_xlow[[1]] * 0.8
    OW_xupp<- operatingwindow %>% filter(Pexc >= 0) %>% arrange(Pexc) %>% filter(row_number() == 1) %>% select(Vinf)
    OW_xupp <- OW_xupp[[1]] * 1.2
    
    # Velocities
    output$OWP_plot <- renderPlot({
      ggplot(operatingwindow) +
        geom_path(aes(x = Vmin, y = h, colour = "Stall Speed")) +
        geom_path(aes(x = Vmin * 1.2, y = h, colour = "Safety Factor")) +
        geom_path(aes(x = VmaxP, y = h, colour = "Maximum Speed")) +
        scale_color_manual(values=c("Stall Speed"="red", "Safety Factor"="orange",
                                    "Maximum Speed"="purple")) +
        xlim(OW_xlow,OW_xupp)
    })
    output$OWP_info <- renderText({
      paste0(
        "click: ", xy_str(input$OWP_click),
        "dblclick: ", xy_str(input$OWP_dblclick),
        "hover: ", xy_str(input$OWP_hover),
        "brush: ", xy_range_str(input$OWP_brush)
      )
    })
    
    
    # Excess Power
    output$OWV_plot <- renderPlot({
      ggplot(operatingwindow) +
        geom_point(data=filter(operatingwindow,Pexc>=0), aes(x=Vinf, y=h, colour=Pexc)) +
        geom_path(aes(x = Vmin, y = h), colour = "red") +
        geom_path(aes(x = Vmin * 1.2, y = h), colour = "orange") +
        geom_path(aes(x = VmaxP, y = h), colour = "purple") +
        scale_colour_gradientn(colours = brewer.pal(5, "RdYlGn"),
                               guide = "colourbar",
                               name = "Excess Power") +
        xlim(0,OW_xupp)
    })
    output$OWV_info <- renderText({
      paste0(
        "click: ", xy_str(input$OWV_click),
        "dblclick: ", xy_str(input$OWV_dblclick),
        "hover: ", xy_str(input$OWV_hover),
        "brush: ", xy_range_str(input$OWV_brush)
      )
    })
    
  })
  
  #---Power and Thrust Curves
  observe({
      nh <- input$PT_nh
      nv <- input$PT_nv
      powerthrustcurves  <-
        data.frame(h = rep(seq(0, 5000, length.out = nh), each = nv),
                   Vinf = rep(seq(40, 120, length.out = nv), times = nh))
      powerthrustcurves <- powerthrustcurves %>%
        mutate(
          W = input$W,
          S = input$S,
          Cd0 = input$Cd0,
          P0 = input$P0,
          Clmax = input$Clmax
        )
      powerthrustcurves$K = input$K 
      powerthrustcurves <- powerthrustcurves%>%
        StandardAtomsphere(.) %>%
        group_by(h) %>%
        mutate(
          qinf = qinf(rho, Vinf),
          Cl = Cl(W, qinf, S),
          Cd = Cd(Cd0, K, Cl),
          ClCd = ClCd(Cl, Cd),
          ClCdstar = ClCdstar(Cd0, K),
          Vmin = Vmin(rho, W, S, Clmax),
          PRmin = PRmin(rho, W, S, Cd0, K),
          PR = PR(Vinf, rho, W, S, Cd0, K),
          TRmin = TRmin(W, ClCdstar),
          TR = TR(W, ClCd),
          Vstar = Vstar(rho, W, S, K, Cd0),
          V32 = V32(Vstar),
          PA = PA(sigma, P0),
          Pexc = Pexc(PA, PR),
          TA = TA(PA, Vinf),
          Texc = Texc(TA, TR)
        ) %>%
        rowwise() %>%
        mutate(VmaxP = VmaxP(PA, rho, W, S, Cd0, K, 50, 200),
               h = as.factor(h)) 
      
      # POWER REQUIRED
      output$PR_plot <- renderPlot({
        ggplot(powerthrustcurves,
               aes(x = Vinf, y = PR, group = h, colour = h)) +
          geom_path() + ggtitle("Power Required") +
          geom_point(aes(x = V32, y = PRmin), shape = 1) +
          geom_point(aes(x = Vstar, y = TRmin * Vstar), shape = 2) +
          expand_limits(x = 0, y = 0)
      })
      output$PR_info <- renderText({
        paste0(
          "click: ", xy_str(input$PR_click),
          "hover: ", xy_str(input$PR_hover)
        )
      })
      
      # THRUST REQUIRED
      output$TR_plot <- renderPlot({
        ggplot(powerthrustcurves,
               aes(x = Vinf, y = TR, group = h, colour = h)) + 
          geom_path() + ggtitle("Thrust Required") +
          geom_point(aes(x = V32, y = PRmin / V32), shape = 1) +
          geom_point(aes(x = Vstar, y = TRmin), shape = 2) +
          expand_limits(x = 0, y = 0)
      })
      output$TR_info <- renderText({
        paste0(
          "click: ", xy_str(input$TR_click),
          "hover: ", xy_str(input$TR_hover)
        )
      })
      
      # POWER EXCESS
      output$Pe_plot <- renderPlot({
        ggplot(powerthrustcurves,
               aes(x = Vinf, y = Pexc, group = h, colour = h)) +
          geom_path() + ggtitle("Power Excess") +
          geom_point(aes(x = V32, y = PA - PRmin), shape = 1) +
          geom_point(aes(x = Vstar, y = PA - TRmin * Vstar), shape = 2) +
          expand_limits(x = 0, y = 0)
      })
      output$Pe_info <- renderText({
        paste0(
          "click: ", xy_str(input$Pe_click),
          "hover: ", xy_str(input$Pe_hover)
        )
      })
      
      # THRUST EXCESS
      output$Te_plot <- renderPlot({
        ggplot(powerthrustcurves,
               aes(x = Vinf, y = Texc, group = h, colour = h)) +
          geom_path() +
          expand_limits(x = 0, y = 0)
      })
      output$Te_info <- renderText({
        paste0(
          "click: ", xy_str(input$Te_click),
          "hover: ", xy_str(input$Te_hover)
        )
      })
  })

  
  
  #---Climb
  observe({
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
  # input <- data.frame(S = 54.4, b = 25.90, AR = 12.33, e = 0.8, K = 0.0323, Cd0 = 0.02, Clmax = 1.2, Clflaps = 0.80, Clhls = 1.20,
  #                     m = 15806, W = 155000, WS = 2850, P0eng = 1530000, P0 = 3060000,
  #                     OW_nv = 51, OW_nh = 51, OW_maxh = 12500, OW_maxv = 200)
  
  
})
