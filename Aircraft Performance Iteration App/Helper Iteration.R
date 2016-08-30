#----------------------------
#--- Main Iteration Solver
#============================
# This is the main iteration method in order to determine values
# N.B. > Will need to trim this HEAPS later lel

## Initialise  ======================================================================

input_initial <- data.frame(
  S = 24.5, b = 22.1359, 
  AR = 20, e = 0.8, K = 0.01989, Cd0 = 0.015,
  Clclean = 1.5, Clflaps = 1.2, Clhls = 1.5,
  m = 6500, W = 63742, WS = 2601.72,
  P0eng = 180000, P0 = 360000, Etatotal = 0.80, alt_s = 0, 
  ClG = 0.25, Cd0G = 0.025, hground = 2.5
)

inputvals <- input_initial

specifications <- data.frame(
  Variable = c("Mp", "Wp", "E", "Dens", "Srun", "Hobs", "Vappmax", "PerGrad2Seg", "ClimbCruise", "ClimbCeil", "AltCruise", "AltCeil", 
               "Mach", "Range", "LoadMax", "LoadMin"),
  Description = c("Payload Mass (kg)","Payload Weight (N)","Battery Specific Energy (J/kg)","Battery Density (kg/m^3)",
                  "Runway Length (m)","Screen Height (m)","Maximum Landing Speed (m/s)","Climb Gradient 2nd Segment (%)",
                  "Climb at Cruise (m/s)","Climb at Ceiling (m/s)","Altitude of Cruise (m)","Altitude of Ceiling (m)",
                  "Cruise Mach Number (M)","Range (m)","Load Factor Max (n)","Load Factor Min (n)"),
  Value = c(120 * 6, 120 * 6 * 9.8065, 1e6, 2750, 1200, 35 * 0.3048, 100 * 0.5144, 1.5, 300 * 0.3048 / 60,
            100 * 0.3048 / 60, 10000 * 0.3048, 12000 * 0.3048, 0.25, 1000e3, 3.5, -1.5)
)

## Small Functions ======================================================================
#--- Minimum velocity for some density, wing loading and max Cl
Vmin <- function(rho, WS, Clmax) 
  sqrt(2/rho * WS * 1/Clmax)

#--- Power derating function
PA <- function(P0, sigma)
  P0 * sigma^inputvals$alt_s

#--- Effective K due to ground effect
Keff <- function(K, h, b)
  (33 * (h/b)^1.5) / (1 + 33 * (h/b)^1.5)  * K

#--- Values of mu for the friction on a runway
groundmu <- data.frame(names = c("Dry Concrete", "Wet Concrete", "Icy Concrete"),
                       brakesoff = c(0.04, 0.05, 0.02), #NB: 0.03-0.05 for dry
                       brakeson_min = c(0.3, 0.15, 0.06),
                       brakeson_max = c(0.5, 0.3, 0.10)) %>%
  mutate(brakeson = (brakeson_min + brakeson_max)/2)

#--- Function to determine the climb rates for a given power and configuration
ClimbRatesFunction <- function(P, Cd0, rho, V, S, K, W) {
  # Coefficients to simplify the calculation
  a = P - 1/2 * Cd0 * rho * V^3 * S
  b = (2 * K * W^2) / (rho * V * S)
  c = W * V
  # Solution to the quadratic
  sintheta = c((c - sqrt(-4*a*b + 4*b^2 + c^2)) / (2*b),
               (c + sqrt(-4*a*b + 4*b^2 + c^2)) / (2*b))
  # Determine which angle to accept
  sintheta = sintheta[sintheta < 0.5 & sintheta > -0.5]
  # Return a meaningful result
  if (length(sintheta) != 1) {
    return(data.frame(Theta = NA, SinTheta = NA, PerGrad = NA, ClimbRate = NA))
  }
  else {
    theta = asin(sintheta)
    return(data.frame(
      Theta = theta * 180 / pi, 
      SinTheta = sintheta, 
      PerGrad = tan(theta)*100, 
      ClimbRate = sintheta * V))
  }
}

#--- Update the other parameters based on m, e, etc (functionalise)
UpdateParams <- function(input) {
  input$b <- sqrt(input$AR * input$S)
  input$K <- 1/(pi * input$AR * input$e)
  input$W <- input$m * 9.8065
  input$WS <- input$W / input$S
  input$P0 <- input$P0eng * 2
  return(input)
}

#--- Calculate the climb in the 2nd segment OEI
seg2oei <- function(out3) {
  out3$type <- c("2nd Seg OEI Climb")
  out3$Ne <- c(1)
  out3$h <- c(out3$Hobs)
  out3$Clmax <- out3$Clclean + c(out3$Clflaps)
  # Determine the climb rates for each scenario
  out3 <- StandardAtomsphere(out3) %>%
    mutate(Vinf = Mach * a,
           Vstall = Vmin(rho, WS, Clmax),
           Vsafe = 1.2 * Vstall)
  out3$Vinf <- c(out3$Vsafe[1])
  out3 <- mutate(out3,
                 qinf = 1/2 * rho * Vinf^2,
                 Cl = W / (qinf * S),
                 Cd = Cd0 + K * Cl^2,
                 PA = PA(P0eng, sigma) * Ne) %>%
    rowwise() %>%
    do(data.frame(., ClimbRatesFunction(.$PA, .$Cd0, .$rho, .$Vinf, .$S, .$K, .$W)))
  return(data.frame(out3))
}

## Begin Calculations ======================================================================
#--- Target
target_We <- 0.40
#--- Manipulate the data into a meaningful form
inp  <- t(specifications["Value"])
colnames(inp) <- t(specifications["Variable"])
inp <- cbind(inputvals, inp)

#--- Variables to be applied
var_m <- seq(6000, 6500, 500)
var_e <- seq(0.8, 0.85, 0.05)
var_Cd0 <-  seq(0.015, 0.020, 0.005)
var_WS <-  seq(1800, 2200, 200)

#--- Apply the variables and update the dataframe
iterationvals <- inp %>%
  rowwise() %>%
  do(data.frame(., var_m = var_m)) %>%
  do(data.frame(., var_e = var_e)) %>%
  do(data.frame(., var_Cd0 = var_Cd0)) %>%
  do(data.frame(., var_WS = var_WS)) %>%
  ungroup() 
iterationvals  <- data.frame(iterationvals) %>%
  mutate(m = var_m, e = var_e, Cd0 = var_Cd0, WS = var_WS) %>%
  mutate(S = m*9.8065 / var_WS) %>%
  select(-var_m, -var_e, -var_Cd0, -var_WS) %>%
  UpdateParams(.)

## For Loop ======================================================================
for (i in 1:nrow(iterationvals))  {
  
  iv0 <- iterationvals[i,]
  
## Determine AR from Empty Weight ======================================================================
  AR <- iv0
  ARr <- AR %>%
    mutate(fx = 10)
  del <- 0.01
  xr <- 15
  xrold <- 10
  
  while(abs(ARr$fx) > 0.001 & abs(xr - xrold) > 0.001) {
    #--- Initial Value Calculations
    AR0 <- AR
    AR0$AR <- xr
    AR0 <- UpdateParams(AR0)
    AR0 <- suppressWarnings(MainIterationFunction(AR0, out = "Iteration", oneinput = TRUE)) %>%
      filter(Description == "Empty Weight") %>%
      select(Iteration) %>%
      mutate(xr = xr, fx = Iteration - target_We)
    #--- Step Value Calculations
    AR1 <- AR
    AR1$AR <- xr + del
    AR1 <- UpdateParams(AR1)
    AR1 <- suppressWarnings(MainIterationFunction(AR1, out = "Iteration", oneinput = TRUE)) %>%
      filter(Description == "Empty Weight") %>%
      select(Iteration) %>%
      mutate(xr = xr + del, fx = Iteration - target_We)
    #--- New xr
    xrold <- xr
    xr <- xrold - (del * AR0$fx) / (AR1$fx - AR0$fx)
    #--- New Value Calculations
    ARr <- AR
    ARr$AR <- xr + del
    ARr <- UpdateParams(ARr)
    ARr <- suppressWarnings(MainIterationFunction(ARr, out = "Iteration", oneinput = TRUE)) %>%
      filter(Description == "Empty Weight") %>%
      select(Iteration) %>%
      mutate(xr = xr + del, fx = Iteration - target_We)
  }
  
  iv0$AR <- xr
  iv0 <- UpdateParams(iv0)
  
  
## Determine Clhls from Vapp ======================================================================
  #--- Initialise the while loop
  Clhls <- iv0 %>%
    mutate(h = 50*0.3048) %>%
    StandardAtomsphere(.) %>%
    select(Clclean, Clhls, rho, WS, Vappmax) %>%
    mutate(xr = 2/rho * WS * (1.3/Vappmax)^2 - Clclean)
  
  #--- Check if Clhls is reasonable
  if (Clhls$xr < 0) Clhls$xr = 0
  #--- Return the result
  iv0$Clhls <- Clhls$xr
  iv0 <- UpdateParams(iv0)
  
## Determine AR from Sland ======================================================================
  # Usually always met lol
  
## Determine P0end from Cruise and Ceiling Climb ======================================================================
  cruise <- iv0
  cruiser <- cruise %>%
    mutate(fx = 10)
  del <- 1
  xr <- 180000
  xrold <- 150000
  
  while((abs(cruiser$fx) > 0.001 & abs(xr - xrold) > 10) | cruiser$fx < 0) {
    #--- Initial Value Calculations
    cruise0 <- cruise
    cruise0$P0eng <- xr
    cruise0 <- UpdateParams(cruise0)
    out3 <- cruise0
    #
    out3$type <- c("Cruise")
    out3$Ne <- c(2)
    out3$h <- c(cruise$AltCruise)
    out3$Clmax <- cruise$Clclean
    out3 <- StandardAtomsphere(out3) %>%
      mutate(Vinf = Mach * a,
             Vstall = Vmin(rho, WS, Clmax),
             Vsafe = 1.2 * Vstall)
    out3$Vinf <- c(out3$Vinf[1])
    out3 <- mutate(out3,
                   qinf = 1/2 * rho * Vinf^2,
                   Cl = W / (qinf * S),
                   Cd = Cd0 + K * Cl^2,
                   PA = PA(P0eng, sigma) * Ne) %>%
      rowwise() %>%
      do(data.frame(., ClimbRatesFunction(.$PA, .$Cd0, .$rho, .$Vinf, .$S, .$K, .$W)))
    #
    cruise0$fx <- out3$ClimbRate - cruise$ClimbCruise
    #--- Step Value Calculations
    cruise1 <- cruise
    cruise1$P0eng <- xr + del
    cruise1 <- UpdateParams(cruise1)
    out3 <- cruise1
    #
    out3$type <- c("Cruise")
    out3$Ne <- c(2)
    out3$h <- c(cruise$AltCruise)
    out3$Clmax <- cruise$Clclean
    out3 <- StandardAtomsphere(out3) %>%
      mutate(Vinf = Mach * a,
             Vstall = Vmin(rho, WS, Clmax),
             Vsafe = 1.2 * Vstall)
    out3$Vinf <- c(out3$Vinf[1])
    out3 <- mutate(out3,
                   qinf = 1/2 * rho * Vinf^2,
                   Cl = W / (qinf * S),
                   Cd = Cd0 + K * Cl^2,
                   PA = PA(P0eng, sigma) * Ne) %>%
      rowwise() %>%
      do(data.frame(., ClimbRatesFunction(.$PA, .$Cd0, .$rho, .$Vinf, .$S, .$K, .$W)))
    #
    cruise1$fx <- out3$ClimbRate - cruise$ClimbCruise
    #--- New xr
    xrold <- xr
    xr <- xrold - (del * cruise0$fx) / (cruise1$fx - cruise0$fx)
    #--- New Value Calculations
    cruiser <- cruise
    cruiser$P0eng <- xr + del
    cruiser <- UpdateParams(cruiser)
    out3 <- cruiser
    #
    out3$type <- c("Cruise")
    out3$Ne <- c(2)
    out3$h <- c(cruise$AltCruise)
    out3$Clmax <- cruise$Clclean
    out3 <- StandardAtomsphere(out3) %>%
      mutate(Vinf = Mach * a,
             Vstall = Vmin(rho, WS, Clmax),
             Vsafe = 1.2 * Vstall)
    out3$Vinf <- c(out3$Vinf[1])
    out3 <- mutate(out3,
                   qinf = 1/2 * rho * Vinf^2,
                   Cl = W / (qinf * S),
                   Cd = Cd0 + K * Cl^2,
                   PA = PA(P0eng, sigma) * Ne) %>%
      rowwise() %>%
      do(data.frame(., ClimbRatesFunction(.$PA, .$Cd0, .$rho, .$Vinf, .$S, .$K, .$W)))
    #
    cruiser$fx <- out3$ClimbRate - cruise$ClimbCruise
  }
  
  #
  
  ceil <- iv0
  ceilr <- ceil %>%
    mutate(fx = 10)
  del <- 1
  xr <- 180000
  xrold <- 150000
  
  while((abs(ceilr$fx) > 0.001 & abs(xr - xrold) > 10) | ceilr$fx < 0) {
    #--- Initial Value Calculations
    ceil0 <- ceil
    ceil0$P0eng <- xr
    ceil0 <- UpdateParams(ceil0)
    out3 <- ceil0
    #
    out3$type <- c("Ceil")
    out3$Ne <- c(2)
    out3$h <- c(ceil$AltCeil)
    out3$Clmax <- ceil$Clclean
    out3 <- StandardAtomsphere(out3) %>%
      mutate(Vinf = Mach * a,
             Vstall = Vmin(rho, WS, Clmax),
             Vsafe = 1.2 * Vstall)
    out3$Vinf <- c(out3$Vinf[1])
    out3 <- mutate(out3,
                   qinf = 1/2 * rho * Vinf^2,
                   Cl = W / (qinf * S),
                   Cd = Cd0 + K * Cl^2,
                   PA = PA(P0eng, sigma) * Ne) %>%
      rowwise() %>%
      do(data.frame(., ClimbRatesFunction(.$PA, .$Cd0, .$rho, .$Vinf, .$S, .$K, .$W)))
    #
    ceil0$fx <- out3$ClimbRate - ceil$ClimbCeil
    #--- Step Value Calculations
    ceil1 <- ceil
    ceil1$P0eng <- xr + del
    ceil1 <- UpdateParams(ceil1)
    out3 <- ceil1
    #
    out3$type <- c("Ceil")
    out3$Ne <- c(2)
    out3$h <- c(ceil$AltCeil)
    out3$Clmax <- ceil$Clclean
    out3 <- StandardAtomsphere(out3) %>%
      mutate(Vinf = Mach * a,
             Vstall = Vmin(rho, WS, Clmax),
             Vsafe = 1.2 * Vstall)
    out3$Vinf <- c(out3$Vinf[1])
    out3 <- mutate(out3,
                   qinf = 1/2 * rho * Vinf^2,
                   Cl = W / (qinf * S),
                   Cd = Cd0 + K * Cl^2,
                   PA = PA(P0eng, sigma) * Ne) %>%
      rowwise() %>%
      do(data.frame(., ClimbRatesFunction(.$PA, .$Cd0, .$rho, .$Vinf, .$S, .$K, .$W)))
    #
    ceil1$fx <- out3$ClimbRate - ceil$ClimbCeil
    #--- New xr
    xrold <- xr
    xr <- xrold - (del * ceil0$fx) / (ceil1$fx - ceil0$fx)
    #--- New Value Calculations
    ceilr <- ceil
    ceilr$P0eng <- xr + del
    ceilr <- UpdateParams(ceilr)
    out3 <- ceilr
    #
    out3$type <- c("Ceil")
    out3$Ne <- c(2)
    out3$h <- c(ceil$AltCeil)
    out3$Clmax <- ceil$Clclean
    out3 <- StandardAtomsphere(out3) %>%
      mutate(Vinf = Mach * a,
             Vstall = Vmin(rho, WS, Clmax),
             Vsafe = 1.2 * Vstall)
    out3$Vinf <- c(out3$Vinf[1])
    out3 <- mutate(out3,
                   qinf = 1/2 * rho * Vinf^2,
                   Cl = W / (qinf * S),
                   Cd = Cd0 + K * Cl^2,
                   PA = PA(P0eng, sigma) * Ne) %>%
      rowwise() %>%
      do(data.frame(., ClimbRatesFunction(.$PA, .$Cd0, .$rho, .$Vinf, .$S, .$K, .$W)))
    #
    ceilr$fx <- out3$ClimbRate - ceil$ClimbCeil
  }
  
  #
  
  #--- Determine which P0eng to use
  P0eng <- max(cruiser$P0eng, ceilr$P0eng)
  #--- Return the new P0eng
  iv0$P0eng <- P0eng
  iv0 <- UpdateParams(iv0)
  
## Determine Clflaps from seg 2 OEI ======================================================================
  #--- Maximum available Clflaps
  a = 0; b = 3
  gr <- (sqrt(5) + 1)/2
  c = b - (b - a) / gr
  d = a + (b - a) /gr
  while (abs(c - d) > 0.0001) {
    # Test which side has the max
    if (seg2oei(mutate(iv0, Clflaps = c))$PerGrad > 
        seg2oei(mutate(iv0, Clflaps = d))$PerGrad)
    {b = d} 
    else {a = c}
    c = b - (b - a) / gr
    d = a + (b - a) /gr
  }
  Clflapspeak <- (c + d) / 2
  
  # Determine if there is a turning point before flaps = hls
  if (iv0$Clhls > Clflapspeak) {
    hlsclimb <- seg2oei(mutate(iv0, Clflaps = Clflapspeak))$PerGrad
  } else {
    hlsclimb <- seg2oei(mutate(iv0, Clflaps = Clhls))$PerGrad
  }
  
  #--- Determine if additional power will be required
  if (hlsclimb < iv0$PerGrad2Seg) {
    # Increase P0eng based on Clflaps = Clhls
    pg <- mutate(iv0, Clflaps = Clhls)
    fx <- 10
    xr <- pg$P0eng
    xrold <- 100000
    del <- 1
    
    while ((abs(fx) > 0.001 & abs(xr - xrold) > 10) | fx < 0) {
      # Initial Value Calcs
      pg0 <- mutate(pg, P0eng = xr)
      pg0 <- pg$PerGrad2Seg - seg2oei(pg0)$PerGrad
      # Step Value Calcs
      pg1 <- mutate(pg, P0eng = xr + del)
      pg1 <- pg$PerGrad2Seg -seg2oei(pg1)$PerGrad
      # New xr
      xrold <- xr
      xr <- xrold - (del * pg0) / (pg1 - pg0)
      # New Value Calcs
      pgr <- mutate(pg, P0eng = xr) #<--- I've noticed this calc is repeated twice per loop!!
      fx <- pg$PerGrad2Seg -seg2oei(pgr)$PerGrad
    }
    
    #--- Return the new P0eng
    iv0$Clflaps <- pg$Clflaps
    iv0$P0eng <- xr
    iv0 <- UpdateParams(iv0)
  } else {
    # Increase Clflaps until just enough
    pg <- mutate(iv0)
    fx <- 10
    xr <- 0.1
    xrold <- 0
    del <- 0.001
    
    while ((abs(fx) > 0.001 & abs(xr - xrold) > 0.0001) | fx < 0) {
      # Initial Value Calcs
      pg0 <- mutate(pg, Clflaps = xr)
      pg0 <- pg$PerGrad2Seg - seg2oei(pg0)$PerGrad
      # Step Value Calcs
      pg1 <- mutate(pg, Clflaps = xr + del)
      pg1 <- pg$PerGrad2Seg -seg2oei(pg1)$PerGrad
      # New xr
      xrold <- xr
      xr <- xrold - (del * pg0) / (pg1 - pg0)
      if (xr <= 0) {
        xr = 0; break
      }
      # New Value Calcs
      pgr <- mutate(pg, Clflaps = xr) #<--- I've noticed this calc is repeated twice per loop!!
      fx <- pg$PerGrad2Seg -seg2oei(pgr)$PerGrad
    }
    
    #--- Return the new P0eng
    iv0$Clflaps <- xr
    iv0 <- UpdateParams(iv0)
  }
  

  
}



