#----------------------------
#--- Main Iteration Solver
#============================
# This is the main iteration method in order to determine values

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

## Begin Calculations ======================================================================
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
  iv0$Clhls <- xr
  
## Determine AR from Sland ======================================================================
  AR <- iv0
  AR$fr <- 10
  del <- 0.1
  xr <- 20
  resolution <- 10
  
  while (abs(fr) > 0.01 | fr < 0) {
    #--- Initial Value 0
    AirDistLD <- AR
    AirDistLD$AR <- xr
    AirDistLD <- UpdateParams(AirDistLD)
    out4 <- AR ## NOTE: Delete this from below!
    out4$AR <- xr
    out4 <- UpdateParams(out4)
    #--- Initial Value Calculations
    AirDistLD$type <- "All Engines"
    AirDistLD$Ne <- 2
    AirDistLD <- AirDistLD %>%
      mutate(h = 50*0.3048) %>%
      StandardAtomsphere(.) %>%
      mutate(
        Clmax = Clclean + Clhls,
        Vstall = Vmin(rho, WS, Clmax),
        Vapp = 1.3* Vstall,
        qinf = 1/2 * rho * Vapp^2,
        gamma = max(-3, ClimbRatesFunction(0.01*P0, Cd0G, rho, Vapp, S, K, W)[[1]]),
        L = W * cos(gamma * pi / 180),
        Cl = L / (qinf * S),
        Cd = Cd0G + K * Cl^2,
        D = qinf * S * Cd,
        TR = D - W * sin(gamma * pi / 180),
        PR = TR * Vapp,
        R = Vapp^2 / (0.2 * g),
        SF = R * sin(gamma * pi / 180),
        hF = R * (1 - cos(gamma * pi / 180)),
        SA = (50*0.3048 - hF) / tan(gamma*pi/180),
        Sair = ifelse(SA >0, SA + SF, sqrt(R^2 - (R-50*0.3048)^2)))
    # Create a dataframe with important parameters
    out4$type <- "Engines off"
    out4$Ne <- 0
    out4$mu <- as.double(filter(groundmu, names == "Dry Concrete") %>% select(brakeson))
    out4 <- mutate(out4, h = 0) %>%
      StandardAtomsphere(.) %>%
      mutate(Vstall = Vmin(rho, WS, Clclean + Clhls),
             Vtd = 1.15 * Vstall)
    # Determine the various velocities
    velocities <- seq(out4$Vtd[1], 1e-5, length.out = resolution * 5)
    out4 <- out4[rep(row.names(out4),each=length(velocities)),1:length(out4)]
    out4$Vinf <- velocities
    # Determine the distances for landing on the ground
    out4 <- mutate(out4, Clmax = Clclean + Clhls, Keff = Keff(K, hground, b)) %>%
      mutate(Mach = Vinf/a, PA = PA(P0eng * Ne, sigma), TA = PA / Vinf,
             Cl = ClG, Cd = Cd0G + Keff * Cl^2, qinf = 1/2 * rho * Vinf^2,
             D =  qinf * S * Cd, L = qinf * S * Cl, 
             Ff = (W - L) * mu, Fnet = TA - D - Ff, accel = Fnet / m,
             accelrecip = 1/(2*accel), Vsq = Vinf^2) %>%
      group_by(type) %>%
      mutate(AreaDur = 1/2 * (accelrecip + lag(accelrecip,1)) * (Vsq - lag(Vsq,1)),
             AreaDur = ifelse(is.na(AreaDur), 0, AreaDur),
             Area = cumsum(AreaDur),
             Area = ifelse(is.na(Area), 0, Area))
    #--- Distance required to land
    DeccelerateLand <- select(out4, Vinf, type, Area) %>%
      spread(type, Area)
    DeccelerateLand <- head(DeccelerateLand,1)
    DeccelerateLand$`Air Distance` = sum(filter(AirDistLD, type == "All Engines") %>% select(Sair))
    DeccelerateLand$`Free Roll` = as.double(out4$Vstall[1]*1.15 * 2)
    DeccelerateLand <- mutate(DeccelerateLand, `Deccelerate-Land` = (`Engines off` + `Free Roll` + `Air Distance`) * 1.67)
    #--- Return the result
    AR0 <- data.frame(xr = AirDistLD$AR, fr = AR$Srun - DeccelerateLand$`Deccelerate-Land`)
    
    #--- Secant Value
    AirDistLD <- AR
    AirDistLD$AR <- xr + del
    AirDistLD <- UpdateParams(AirDistLD)
    out4 <- AR ## NOTE: Delete this from below!
    out4$AR <- xr + del
    out4 <- UpdateParams(out4)
    
    #--- Secant Value Calculations
    AirDistLD$type <- "All Engines"
    AirDistLD$Ne <- 2
    AirDistLD <- AirDistLD %>%
      mutate(h = 50*0.3048) %>%
      StandardAtomsphere(.) %>%
      mutate(
        Clmax = Clclean + Clhls,
        Vstall = Vmin(rho, WS, Clmax),
        Vapp = 1.3* Vstall,
        qinf = 1/2 * rho * Vapp^2,
        gamma = max(-3, ClimbRatesFunction(0.01*P0, Cd0G, rho, Vapp, S, K, W)[[1]]),
        L = W * cos(gamma * pi / 180),
        Cl = L / (qinf * S),
        Cd = Cd0G + K * Cl^2,
        D = qinf * S * Cd,
        TR = D - W * sin(gamma * pi / 180),
        PR = TR * Vapp,
        R = Vapp^2 / (0.2 * g),
        SF = R * sin(gamma * pi / 180),
        hF = R * (1 - cos(gamma * pi / 180)),
        SA = (50*0.3048 - hF) / tan(gamma*pi/180),
        Sair = ifelse(SA >0, SA + SF, sqrt(R^2 - (R-50*0.3048)^2)))
    # Create a dataframe with important parameters
    out4$type <- "Engines off"
    out4$Ne <- 0
    out4$mu <- as.double(filter(groundmu, names == "Dry Concrete") %>% select(brakeson))
    out4 <- mutate(out4, h = 0) %>%
      StandardAtomsphere(.) %>%
      mutate(Vstall = Vmin(rho, WS, Clclean + Clhls),
             Vtd = 1.15 * Vstall)
    # Determine the various velocities
    velocities <- seq(out4$Vtd[1], 1e-5, length.out = resolution * 5)
    out4 <- out4[rep(row.names(out4),each=length(velocities)),1:length(out4)]
    out4$Vinf <- velocities
    # Determine the distances for landing on the ground
    out4 <- mutate(out4, Clmax = Clclean + Clhls, Keff = Keff(K, hground, b)) %>%
      mutate(Mach = Vinf/a, PA = PA(P0eng * Ne, sigma), TA = PA / Vinf,
             Cl = ClG, Cd = Cd0G + Keff * Cl^2, qinf = 1/2 * rho * Vinf^2,
             D =  qinf * S * Cd, L = qinf * S * Cl, 
             Ff = (W - L) * mu, Fnet = TA - D - Ff, accel = Fnet / m,
             accelrecip = 1/(2*accel), Vsq = Vinf^2) %>%
      group_by(type) %>%
      mutate(AreaDur = 1/2 * (accelrecip + lag(accelrecip,1)) * (Vsq - lag(Vsq,1)),
             AreaDur = ifelse(is.na(AreaDur), 0, AreaDur),
             Area = cumsum(AreaDur),
             Area = ifelse(is.na(Area), 0, Area))
    #--- Distance required to land
    DeccelerateLand <- select(out4, Vinf, type, Area) %>%
      spread(type, Area)
    DeccelerateLand <- head(DeccelerateLand,1)
    DeccelerateLand$`Air Distance` = sum(filter(AirDistLD, type == "All Engines") %>% select(Sair))
    DeccelerateLand$`Free Roll` = as.double(out4$Vstall[1]*1.15 * 2)
    DeccelerateLand <- mutate(DeccelerateLand, `Deccelerate-Land` = (`Engines off` + `Free Roll` + `Air Distance`) * 1.67)
    #--- Return the result
    AR1 <- data.frame(xr = AirDistLD$AR, fr = AR$Srun - DeccelerateLand$`Deccelerate-Land`)
    
    #--- Calculate the new xr
    xr <- xr - (del * AR0$fr) / (AR1$fr - AR0$fr)
    # RATHER than rerunning this, test the convergence using the previous fr value
    fr <- AR0$fr
    if (xr <=0) break
  }
  
  #--- Check for reasonableness
  if (xr <= 0 | xr >= 30) xr = 15
  #--- Return the result
  iv0$AR <- xr
  
}



