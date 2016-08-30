#----------------------------
#--- Main Function
#============================
# These are the main functions that will run on the server

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

# Iteration: Minimise - Minimise the differece

## Main Function ======================================================================
# THIS IS FOR CALCULATING SINGLE VALUES THAT WILL THEN GO INTO THE ITERATION FUNCTION
# Do not put graphing functions here!!

MainIterationFunction <- function(inputvals, specifications, resolution = 10, out = "Iteration", 
                                  updateProgress = NULL, oneinput = FALSE) {
  #--- Manipulate the data into a meaningful form
  if (oneinput == FALSE) {
    inp  <- t(specifications["Value"])
    colnames(inp) <- t(specifications["Variable"])
    inp <- cbind(inputvals, inp)
  } else {
    inp <- inputvals
  }
  #--- Initialise the summary data frame
  summary <- data.frame()
  
  #--- Initialise the iteration data frame
  iteration <- data.frame()
  
## AeroParams ======================================================================
  #--- Aerodynamic Parameters at various stages of the mission
  # Create the initial data frame with starting conditions
  out1 <-  inp[rep(row.names(inp), each = 5), 1:length(inp)]
  out1$type <- c("Sea Level", "Cruise", "Ceiling", "Takeoff", "Landing")
  out1$h <- c(0, inp$AltCruise, inp$AltCeil, 0, 0)
  out1$Clmax <- inp$Clclean + c(0, 0, 0, inp$Clflaps, inp$Clhls)
  # Transform the dataframe to find the various aerodynamic properties
  out1 <- StandardAtomsphere(out1) %>%
    mutate(Vinf = Mach * a,
           Vstall = Vmin(rho, WS, Clmax))
  out1$Vinf <- c(out1$Vinf[1], out1$Vinf[2], out1$Vinf[3], out1$Vstall[4] * 1.2, out1$Vstall[5] * 1.3)
  out1 <- out1 %>%
    mutate(Vsafe = 1.2 * Vstall,
           qinf = 1/2 * rho * Vinf^2,
           Cl = W / (qinf * S),
           Cd = Cd0 + K * Cl^2,
           ClCd = Cl/Cd,
           Clstar = sqrt(Cd0 / K),
           Cdstar = 2 * Cd0,
           ClCdstar = 1 / sqrt(4 * Cd0 * K),
           Vstar = Vmin(rho, WS, Clstar),
           Cl32 = sqrt(3) * Clstar,
           Cd32 = 2 * Cdstar,
           ClCd32 = sqrt(3/4) * ClCdstar,
           V32 = (1/3)^(1/4) * Vstar
    )
  
  #--- Create a meaningful output to be returned  to a user
  AeroParamsTable <- select(out1,
    type, h, rho, Vinf, Vstall, Vsafe, Vstar, V32, 
    Cl, Clstar, Cl32, Clmax, Cd, Cdstar, Cd32, 
    ClCd, ClCdstar, ClCd32)
  
  #--- SUMMARY
  summary <- rbind(
    summary,
     data.frame(
       Description = c(
         "Cruise Vinf vs Vstar",
         "Cruise Vinf vs Vstall",
         "Cruise Vinf vs Vsafe",
         "Cruise Cl vs Clstar",
         "Cruise Cl/Cd vs (L/D)*",
         "Landing Vstall * 1.3"
          ),
       Value = c(
         filter(out1, type == "Cruise")$Vstar,
         filter(out1, type == "Cruise")$Vstall,
         filter(out1, type == "Cruise")$Vsafe,
         filter(out1, type == "Cruise")$Cl,
         filter(out1, type == "Cruise")$ClCd,
         filter(out1, type == "Landing")$Vstall * 1.3
          ),
       Target = c(
         filter(out1, type == "Cruise")$Vinf,
         filter(out1, type == "Cruise")$Vinf,
         filter(out1, type == "Cruise")$Vinf,
         filter(out1, type == "Cruise")$Clstar,
         filter(out1, type == "Cruise")$ClCdstar,
         inp$Vappmax
          )
     ))
  
  iteration <- rbind(
    iteration,
    data.frame(
      Description = c(
        "Cruise near Vstar",
        "Cruise above 1.2 Vstall",
        "Landing below 100kt"
      ),
      Iteration = c(
        filter(out1, type == "Cruise")$Vstar,
        filter(out1, type == "Cruise")$Vsafe,
        filter(out1, type == "Landing")$Vstall * 1.3
      ),
      Specification = c(
        filter(out1, type == "Cruise")$Vinf,
        filter(out1, type == "Cruise")$Vinf,
        inp$Vappmax
      ),
      Minimise = c(1, 0, 1),
      Under = c(1, 0, 0),
      Over = c(0, 1, 0)
    )
  )
  
  
  if (is.function(updateProgress)) {
    text <- paste0("Aerodynamic Parameters")
    updateProgress(detail = text, value = 1)
  }
  
## Takeoff ======================================================================
  #--- Determine the distance required for takeoff
  # Initialise a data frame to apply functions to
  out2 <- inp[rep(row.names(inp), each = 3), 1:length(inp)]
  out2$type <- c("All Engines", "One Engine Down", "Rejected Take-Off")
  out2$Ne <- c(2, 1, 0)
  out2$mu <- c(
    as.double(filter(groundmu,names == "Dry Concrete") %>% select(brakesoff)),
    as.double(filter(groundmu,names == "Dry Concrete") %>% select(brakesoff)),
    as.double(filter(groundmu,names == "Dry Concrete") %>% select(brakeson))
  )
  # Determine the important velocities in order to generate a sequence
  out2 <- mutate(out2, h = 0) %>%
    StandardAtomsphere(.) %>%
    mutate(Vstall = Vmin(rho, WS, Clclean + Clflaps),
           Vlof = 1.1 * Vstall)
  # Create a seqence of velocities to bind to the data frame
  Vlof <- out2$Vlof[1]
  velocities <- c(seq(1e-1,Vlof, length.out = resolution * 5), seq(Vlof*1.01, Vlof*1.2, length.out = resolution * 2 + 1))
  out2 <- out2[rep(row.names(out2),each=length(velocities)),1:length(out2)]
  rownames(out2) <- NULL
  out2$Vinf <- rep(velocities,3)
  # Determine the distance travelled in each interval
  out2 <- mutate(out2, Clmax = Clclean + Clflaps, Keff = Keff(K, hground, b)) %>%
    mutate(M = Vinf/a, PA = PA(P0eng, sigma) * Ne, TA = PA / Vinf,
           Cl = ClG, Cd = Cd0G + Keff * Cl^2, qinf = 1/2 * rho * Vinf^2,
           D =  qinf * S * Cd, L = qinf * S * Cl, 
           Ff = (W - L) * mu, Fnet = TA - D - Ff, accel = Fnet / m,
           accelrecip = 1/(2*accel), Vsq = Vinf^2) %>%
    group_by(type) %>%
    mutate(AreaDur = 1/2 * (accelrecip + lag(accelrecip,1)) * (Vsq - lag(Vsq,1)),
           AreaDur = ifelse(is.na(AreaDur), 0, AreaDur),
           Area = cumsum(AreaDur),
           Area = ifelse(is.na(Area), 0, Area))
  # Distance in the air during Take-off
  AirDistTO <- inp[rep(row.names(inp), each=2), 1:length(inp)]
  rownames(AirDistTO) <- NULL
  AirDistTO$type <- c("All Engines", "One Engine Down")
  AirDistTO$Ne <- c(2, 1)
  AirDistTO <- AirDistTO %>%
    mutate(h = 0) %>%
    StandardAtomsphere(.) %>%
    mutate(ClTR = Clclean + Clflaps, 
      Vstall = Vmin(rho, WS, ClTR), VTR = Vstall * 1.15,
      PA = PA(P0eng * Ne, sigma), TA = PA / VTR,
      qinf = 1/2 * rho * VTR^2, Cd = Cd0 + K * ClTR^2,
      D = qinf * S * Cd, L = qinf * S * ClTR) %>%
    rowwise() %>%
    mutate(
      R = (VTR) ^ 2 / (0.2 * g),
      # gamma = asin((TA - D) / W),
      gamma = ClimbRatesFunction(PA, Cd0, rho, VTR, S, K, W)[[1]] * pi / 180,
      hTR = R * (1 - cos(gamma)),
      ST = R * (sin(gamma)),
      SC = (Hobs - hTR) / tan(gamma),
      Sair = ifelse(SC >0, ST + SC, sqrt(R^2 - (R-hTR)^2))
    )%>%
    ungroup()
  #--- Distance required to stop after engine failure at V1
  AccelerateStop <- select(out2, Vinf, type, Area) %>%
    spread(type, Area) %>%
    mutate(AccelerateStop = `All Engines` - `Rejected Take-Off`)
  #--- Distance required to continue takeoff after engine failure at V1
  maxOEI <- as.double(filter(out2, Vinf == Vlof, type == "One Engine Down")$Area)
  AccelerateContinue <- select(out2, Vinf, type, Area) %>%
    spread(type, Area) %>%
    mutate(`One Engine Down` = maxOEI - `One Engine Down`)
  AccelerateContinue$`Air Distance` = sum(filter(AirDistTO, type == "One Engine Down") %>% select(Sair))
  AccelerateContinue <- mutate(AccelerateContinue, AccelerateContinue = `All Engines` + `One Engine Down` + `Air Distance`)
  #--- Distance required to take off with all engines operational PLUS a 1.15 safety margin
  AccelerateLiftoff <- select(out2, Vinf, type, Area) %>%
    spread(type, Area)
  AccelerateLiftoff <- filter(AccelerateLiftoff, Vinf == Vlof)
  AccelerateLiftoff$`Air Distance` = sum(filter(AirDistTO, type == "All Engines") %>% select(Sair))
  AccelerateLiftoff <- mutate(AccelerateLiftoff, AccelerateLiftoff = (`All Engines` + `Air Distance`) * 1.15)
  #--- Find the value of BFL
  BFL <- AccelerateStop %>% select(Vinf, AccelerateStop)
  BFL$AccelerateContinue <- AccelerateContinue$AccelerateContinue
  BFL <- mutate(BFL, 
                Vlof = out2$Vlof[1],
                diff = AccelerateContinue - AccelerateStop,
                root = sign(diff*lag(diff,1)),
                root = root*lead(root,1),
                root = ifelse(is.na(root),1,root)) %>%
    arrange(root, abs(diff))
  # VERY ROUGH (too be fixed later)
  BFL <- head(BFL,1) %>%
    mutate(BFL = min(AccelerateStop, AccelerateContinue))
  
  #--- SUMMARY
  summary <- rbind(
    summary,
      data.frame(
       Description = c(
         "Accelerate-Liftoff", 
         "BFL-AccelerateStop", 
         "BFL-AccelerateContinue", 
         "V1",
         "V2"),
       Value = c(
         AccelerateLiftoff$AccelerateLiftoff,
         BFL$AccelerateStop,
         BFL$AccelerateContinue,
         BFL$Vinf,
         AccelerateLiftoff$Vinf),
       Target = c(
         inp$Srun,
         NA,
         NA,
         NA,
         NA)
      ))
  
  iteration <- rbind(
    iteration,
    data.frame(
      Description = c(
        "Normal Takeoff",
        "Estimated BFL"
      ),
      Iteration = c(
        AccelerateLiftoff$AccelerateLiftoff,
        BFL$BFL
      ),
      Specification = c(
        inp$Srun, 
        inp$Srun
      ),
      Minimise = c(1, 1),
      Under = c(1, 1),
      Over = c(0, 0)
    )
  )
  
  if (is.function(updateProgress)) {
    text <- paste0("Takeoff")
    updateProgress(detail = text, value = 2)
  }
  
## Climb ======================================================================
  #--- Determine the various climb rates required
  # Create a data frame of the three scenarios
  out3 <-  inp[rep(row.names(inp), each = 3), 1:length(inp)]
  out3$type <- c("2nd Seg OEI Climb", "Cruise", "Ceiling")
  out3$Ne <- c(1, 2, 2)
  out3$h <- c(inp$Hobs, inp$AltCruise, inp$AltCeil)
  out3$Clmax <- inp$Clclean + c(inp$Clflaps, 0, 0)
  # Determine the climb rates for each scenario
  out3 <- StandardAtomsphere(out3) %>%
    mutate(Vinf = Mach * a,
           Vstall = Vmin(rho, WS, Clmax),
           Vsafe = 1.2 * Vstall)
  out3$Vinf <- c(out3$Vsafe[1], out3$Vinf[1], out3$Vinf[1])
  out3 <- mutate(out3,
           qinf = 1/2 * rho * Vinf^2,
           Cl = W / (qinf * S),
           Cd = Cd0 + K * Cl^2,
           PA = PA(P0eng, sigma) * Ne
    ) %>%
    rowwise() %>%
    do(data.frame(., ClimbRatesFunction(.$PA, .$Cd0, .$rho, .$Vinf, .$S, .$K, .$W)))
  #--- SUMMARY
  summary <- rbind(
    summary,
    data.frame(
      Description = c(
        "2nd Segment PerGrad",
        "Cruise Climb Rate",
        "Ceiling Climb Rate"
      ),
      Value = c(
        filter(out3, type == "2nd Seg OEI Climb")$PerGrad,
        filter(out3, type == "Cruise")$ClimbRate,
        filter(out3, type == "Ceiling")$ClimbRate
      ),
      Target = c(
        inp$PerGrad2Seg,
        inp$ClimbCruise,
        inp$ClimbCeil
      )
    ))
  
  iteration <- rbind(
    iteration,
    data.frame(
      Description = c(
        "2nd Segment PerGrad",
        "Cruise Climb Rate",
        "Ceiling Climb Rate"
      ),
      Iteration = c(
        filter(out3, type == "2nd Seg OEI Climb")$PerGrad,
        filter(out3, type == "Cruise")$ClimbRate,
        filter(out3, type == "Ceiling")$ClimbRate
      ),
      Specification = c(
        inp$PerGrad2Seg,
        inp$ClimbCruise,
        inp$ClimbCeil
      ),
      Minimise = c(0, 0, 0),
      Under = c(0, 0, 0),
      Over = c(1, 1, 1)
    )
  )
  
  if (is.function(updateProgress)) {
    text <- paste0("Climb")
    updateProgress(detail = text, value = 3)
  }
  
## Landing ======================================================================
  #--- Determine the distance required for landing
  AirDistLD <- inp
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
  out4 <- inp
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
  #--- SUMMARY
  summary <- rbind(summary,
                   data.frame(
                     Description = c("Deccelerate Land"),
                     Value = c(DeccelerateLand$`Deccelerate-Land`),
                     Target = c(inp$Srun)
                   ))
  
  iteration <- rbind(
    iteration,
    data.frame(
      Description = c(
        "Landing Dist",
        "Approach Speed"
      ),
      Iteration = c(
        DeccelerateLand$`Deccelerate-Land`,
        AirDistLD$Vapp
      ),
      Specification = c(
        inp$Srun,
        inp$Vappmax
      ),
      Minimise = c(1, 1),
      Under = c(1, 1),
      Over = c(0, 0)
    )
  )
  
  if (is.function(updateProgress)) {
    text <- paste0("Landing")
    updateProgress(detail = text, value = 4)
  }
  
## Power ======================================================================
  #--- Take-off 
  # Ground Roll 
  PTOgr <- filter(out2, type == "All Engines") %>%
    ungroup %>%
    mutate(type = "Take-off Ground Roll",
           theta = 0,
           ClCd = Cl/Cd,
           duration = 2*AreaDur/(Vinf + lag(Vinf, 1)),
           duration = ifelse(is.na(duration), 0, duration)) %>%
    mutate(t = cumsum(duration),
           Eduration = 1/2 * (PA + lag(PA,1)) * duration,
           Eduration = ifelse(is.na(Eduration), 0, Eduration),
           Eeng = cumsum(Eduration),
           Wb100 = Eeng / 1e6,
           Rduration = AreaDur,
           Rduration = ifelse(is.na(Rduration), 0, Rduration),
           R = cumsum(Rduration),
           Power = PA,
           Drag = D) %>%
    select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, Power, Drag)
  # Transition
  PTOtr <- filter(AirDistTO, type == "All Engines") %>%
    ungroup() %>%
    mutate(type = "Take-off Transition",
           theta = ifelse(SC > 0, atan(hTR / ST), atan(Hobs / Sair)) * 180 / pi,
           Clmax = Clclean + Clflaps,
           Vinf = VTR,
           Cl = Clmax/(1.15^2),
           ClCd = Cl/Cd,
           Vv = Vinf * sin(theta * pi/ 180),
           Vh = Vinf * cos(theta * pi/ 180),
           duration = ifelse(SC > 0, hTR / Vv, Hobs / Vv),
           h = ifelse(SC > 0, hTR, Hobs))
  PTOtr <- data.frame(PTOtr)
  PTOtr <- PTOtr[rep(row.names(PTOtr), each = 2), 1:length(PTOtr)]
  PTOtr$duration <- c(0, PTOtr$duration[2])
  PTOtr$h <- c(0, PTOtr$h[2])
  PTOtr <- PTOtr %>%
    mutate(t = cumsum(duration),
           Eduration = 1/2 * (PA + lag(PA,1)) * duration,
           Eduration = ifelse(is.na(Eduration), 0, Eduration),
           Eeng = cumsum(Eduration),
           Wb100 = Eeng / 1e6,
           Rduration = 1/2 * (Vh + lag(Vh,1)) * duration,
           Rduration = ifelse(is.na(Rduration), 0, Rduration),
           R = cumsum(Rduration),
           Power = PA,
           Drag = qinf * Cd * S) %>%
    select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, Power, Drag)
  
  if (is.function(updateProgress)) {
    text <- paste0("Power - Ground Roll")
    updateProgress(detail = text, value = 5)
  }
  
  #--- Climb Segments
  # Segment 1
  # Flying at climb speed VTR with landing gear up and flaps in takeoff position to clear Hobs if not already
  Pseg1 <- filter(AirDistTO, type == "All Engines") %>%
    ungroup() %>%
    mutate(type = "Segment 1 - Climb",
           theta = ifelse(SC > 0, atan((Hobs - hTR) / SC), 0) * 180 / pi,
           Clmax = Clclean + Clflaps,
           Vinf = VTR,
           Cl = Clmax/(1.15^2),
           ClCd = Cl/Cd,
           Vv = Vinf * sin(theta* pi/ 180),
           Vh = Vinf * cos(theta* pi/ 180),
           duration = ifelse(SC > 0, (Hobs - hTR) / Vv, 0),
           h = ifelse(SC > 0, hTR, Hobs))
  Pseg1 <- data.frame(Pseg1)
  Pseg1 <- Pseg1[rep(row.names(Pseg1), each = 2), 1:length(Pseg1)]
  Pseg1$duration <- c(0, Pseg1$duration[2])
  Pseg1$h <- c(Pseg1$h[2], Pseg1$Hobs[1])
  Pseg1 <- Pseg1 %>%
    mutate(t = cumsum(duration),
           Eduration = 1/2 * (PA + lag(PA,1)) * duration,
           Eduration = ifelse(is.na(Eduration), 0, Eduration),
           Eeng = cumsum(Eduration),
           Wb100 = Eeng / 1e6,
           Rduration = 1/2 * (Vh + lag(Vh,1)) * duration,
           Rduration = ifelse(is.na(Rduration), 0, Rduration),
           R = cumsum(Rduration),
           Power = PA,
           Drag = qinf * Cd * S) %>%
    select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, Power, Drag)
  
  if (is.function(updateProgress)) {
    text <- paste0("Power - Segment 1")
    updateProgress(detail = text, value = 6)
  }
  
  # Segment 2
  # Flying at climb speed V2 with landing gear up and flaps in takeoff position
  Pseg2num <- resolution * 3
  Pseg2Heights <- seq(inp$Hobs, 400*0.3048, length.out = Pseg2num)
  Pseg2 <- cbind(inp, h = Pseg2Heights, type = "2nd Segment Climb") %>%
    StandardAtomsphere(.) %>%
    mutate(Clmax = Clclean + Clflaps,
           Cd0 = Cd0,
           Vstall = Vmin(rho, WS, Clmax),
           Vinf = Vstall * 1.2,
           qinf = 1/2 * rho * Vinf^2,
           PA = PA(P0, sigma)) %>%
    rowwise() %>%
    mutate(theta = ClimbRatesFunction(PA, Cd0, rho, Vinf, S, K, W)[[1]],
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
           R = cumsum(Rduration),
           Power = PA,
           Drag = qinf * Cd * S) %>%
    select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, Power, Drag)
  
  if (is.function(updateProgress)) {
    text <- paste0("Power - Segment 2")
    updateProgress(detail = text, value = 7)
  }
  
  # Segment 3
  # Flying at constant altitude accelerating from V2 to VFS or 1.25 VS with flaps in TO
  Pseg3num <- resolution * 5
  Pseg3Velocities <- seq(tail(Pseg2$Vinf, 1), as.double(filter(AeroParamsTable, type == "Cruise") %>% select(Vinf)), length.out = Pseg3num)
  Pseg3 <- cbind(inp, Vinf = Pseg3Velocities)
  Pseg3 <- Pseg3 %>%
    mutate(h = tail(Pseg2$h,1), type = "3nd Segment Acceleration") %>%
    StandardAtomsphere(.) %>%
    mutate(Clmax = Clclean + Clflaps,
           Cd0 = Cd0,
           Vstall = Vmin(rho, WS, Clmax),
           qinf = 1/2 *rho * Vinf^2,
           PA = PA(P0, sigma),
           TA = PA / Vinf,
           theta = 0) %>%
    mutate(Cl = W/(qinf * S),
           Cd = Cd0 + K * Cl^2,
           ClCd = Cl/Cd,
           D = qinf * S * Cd,
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
           R = cumsum(Rduration),
           Power = PA,
           Drag = D) %>%
    select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, Power, Drag)
  
  if (is.function(updateProgress)) {
    text <- paste0("Power - Segment 3")
    updateProgress(detail = text, value = 8)
  }
  
  # Segment 4
  # Flying at climb speed Vcruise with landing gear up and flaps retracted
  Pseg4num <- resolution * 8
  Pseg4Heights <- seq(tail(Pseg3$h, 1), inp$AltCruise, length.out = Pseg4num)
  Pseg4 <- cbind(inp, h = Pseg4Heights, type = "4th Segment Climb") %>%
    StandardAtomsphere(.) %>%
    mutate(Clmax = Clclean,
           Cd0 = Cd0,
           Vstall = Vmin(rho, WS, Clmax),
           Vinf = tail(Pseg3$Vinf, 1),
           qinf = 1/2 * rho * Vinf^2,
           PA = PA(P0, sigma)) %>%
    rowwise() %>%
    mutate(theta = ClimbRatesFunction(PA, Cd0, rho, Vinf, S, K, W)[[1]],
           Vv = Vinf * sin(theta * pi / 180),
           Vh = Vinf * cos(theta * pi / 180),
           Cl = W * cos(theta * pi / 180) / (qinf * S),
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
           R = cumsum(Rduration),
           Power = PA,
           Drag = qinf * Cd * S) %>%
    select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, Power, Drag)
  
  if (is.function(updateProgress)) {
    text <- paste0("Power - Segment 4")
    updateProgress(detail = text, value = 9)
  }
  
  #--- Descent
  Pdes4num = resolution * 8
  PdesHeights <- seq(inp$AltCruise, 50*0.3048, length.out = Pdes4num)
  Vcruise <- as.double(filter(AeroParamsTable, type == "Cruise") %>% select(Vinf))
  PdesVelocities <- c(
    seq(Vcruise, 3/8 * Vcruise + 5/8* AirDistLD$Vapp, length.out = ceiling(Pdes4num/8)),
    seq(3/8 * Vcruise + 5/8* AirDistLD$Vapp, AirDistLD$Vapp, length.out = Pdes4num - ceiling(Pdes4num/8))
  )
    # seq(Vcruise, AirDistLD$Vapp, length.out = Pdes4num)
  Pcruise <- as.double(filter(out1, type == "Cruise") %>% mutate(D = qinf * S * Cd, PR = D * Vinf) %>% select(PR))
  PdesPowers <- c(
    seq(Pcruise, 1/8 * Pcruise + 7/8* 0.05*inp$P0, length.out = ceiling(Pdes4num/8)),
    seq(1/8 * Pcruise + 7/8* 0.05*inp$P0, 0.05*inp$P0, length.out = Pdes4num - ceiling(Pdes4num/8))
  )
  # seq(1, 0, length.out = Pdes4num)^(6) * (Pcruise - 0.1*inp$P0) + 0.1*inp$P0
  Pdes <- inp
  Pdes$type <- "Descent"
  Pdes$Ne <- 2
  Pdes <- Pdes[rep(row.names(Pdes), each = Pdes4num), 1:length(Pdes)]
  Pdes$h <- PdesHeights
  Pdes$Vinf <- PdesVelocities
  Pdes$Pthrot <- PdesPowers
  Pdes <- Pdes %>%
    StandardAtomsphere(.) %>%
    rowwise() %>%
    mutate(
      Clmax = Clclean + Clhls,
      Vstall = Vmin(rho, WS, Clmax),
      qinf = 1/2 * rho * Vinf^2,
      theta = max(-3, ClimbRatesFunction(Pthrot, Cd0, rho, Vinf, S, K, W)[[1]]),
      L = W * cos(theta * pi / 180),
      Cl = L / (qinf * S),
      Cd = Cd0 + K * Cl^2,
      D = qinf * S * Cd,
      TR = D + W * sin(theta * pi / 180),
      PR = TR * Vinf) %>%
    mutate(
      Vv = Vinf * sin(theta * pi/ 180),
      Vh = Vinf * cos(theta* pi/ 180),
      Cl = W * cos(theta * pi / 180) / (qinf * S),
      Cd = Cd0 + K*Cl^2,
      ClCd = Cl/Cd) %>% 
    ungroup() %>%
    mutate(duration = 2 * (h - lag(h,1)) / (Vv + lag(Vv,1)),
           duration = ifelse(is.na(duration), 0, duration),
           t = cumsum(duration)) %>%
    mutate(Eduration = 1/2 * (PR + lag(PR,1)) * duration,
           Eduration = ifelse(is.na(Eduration), 0, Eduration),
           Eeng = cumsum(Eduration),
           Wb100 = Eeng / 1e6,
           Rduration = 1/2 * (Vh + lag(Vh,1)) * duration,
           Rduration = ifelse(is.na(Rduration), 0, Rduration),
           R = cumsum(Rduration),
           Power = PR,
           Drag = D) %>%
    select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, Power, Drag)
  
  if (is.function(updateProgress)) {
    text <- paste0("Power - Descent")
    updateProgress(detail = text, value = 10)
  }
  
  #--- Landing
  # Flare
  PLDfl <- AirDistLD %>%
    ungroup() %>%
    mutate(type = "Landing Flare",
           theta = atan(50*0.3048 / Sair) * 180/ pi,
           Clmax = Clclean + Clhls,
           Vinf = Vapp,
           ClCd = Cl/Cd,
           Vv = Vinf * sin(theta * pi/ 180),
           Vh = Vinf * cos(theta* pi/ 180),
           duration = 50*0.3048 / Vv,
           h = 50*0.3048)
  PLDfl <- PLDfl[rep(row.names(PLDfl), each = 2), 1:length(PLDfl)]
  PLDfl$duration <- c(0, PLDfl$duration[2])
  PLDfl$h <- c(PLDfl$h[2], 0)
  PLDfl <- PLDfl %>%
    mutate(t = cumsum(duration),
           Eduration = 1/2 * (PR + lag(PR,1)) * duration,
           Eduration = ifelse(is.na(Eduration), 0, Eduration),
           Eeng = cumsum(Eduration),
           Wb100 = Eeng / 1e6,
           Rduration = 1/2 * (Vh + lag(Vh,1)) * duration,
           Rduration = ifelse(is.na(Rduration), 0, Rduration),
           R = cumsum(Rduration),
           Power = PR,
           Drag = D) %>%
    select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, Power, Drag)
  
  if (is.function(updateProgress)) {
    text <- paste0("Power - Flare")
    updateProgress(detail = text, value = 11)
  }
  
  # Ground Roll 
  PLDgr <- out4 %>%
    ungroup %>%
    mutate(type = "Landing Ground Roll",
           theta = 0,
           ClCd = Cl/Cd,
           duration = 2*AreaDur/(Vinf + lag(Vinf, 1)),
           duration = ifelse(is.na(duration), 0, duration)) %>%
    mutate(t = cumsum(duration),
           Eduration = 1/2 * (PA + lag(PA,1)) * duration,
           Eduration = ifelse(is.na(Eduration), 0, Eduration),
           Eeng = cumsum(Eduration),
           Wb100 = Eeng / 1e6,
           Rduration = AreaDur,
           Rduration = ifelse(is.na(Rduration), 0, Rduration),
           R = cumsum(Rduration),
           Power = PA,
           Drag = D) %>%
    select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, Power, Drag)
  
  if (is.function(updateProgress)) {
    text <- paste0("Power - Ground Roll")
    updateProgress(detail = text, value = 12)
  }
  
  #--- Cruise
  Scruise <- inp$Range - tail(PTOgr$R,1) - tail(PTOtr$R,1) -
    tail(Pseg1$R,1) - tail(Pseg2$R,1) - tail(Pseg3$R,1) - tail(Pseg4$R,1) -
    tail(Pdes$R,1) - tail(PLDfl$R,1) - tail(PLDgr$R,1)
  Pcr <- filter(out1, type == "Cruise") %>%
    mutate(D = qinf * S * Cd,
           PR = D * Vinf,
           theta = 0,
           Vh = Vinf,
           duration = Scruise/Vinf)
  row.names(Pcr) <- NULL
  Pcr <- Pcr[rep(row.names(Pcr), each = 2), 1:length(Pcr)]
  Pcr$duration <- c(0, Pcr$duration[2])
  Pcr <- Pcr %>%
    mutate(t = cumsum(duration),
           Eduration = 1/2 * (PR + lag(PR,1)) * duration,
           Eduration = ifelse(is.na(Eduration), 0, Eduration),
           Eeng = cumsum(Eduration),
           Wb100 = Eeng / 1e6,
           Rduration = 1/2 * (Vh + lag(Vh,1)) * duration,
           Rduration = ifelse(is.na(Rduration), 0, Rduration),
           R = cumsum(Rduration),
           Power = PR,
           Drag = D) %>%
    select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, Power, Drag)

  if (is.function(updateProgress)) {
    text <- paste0("Power - Cruise")
    updateProgress(detail = text, value = 13)
  }
  
  #--- Total
  Power <- rbind(PTOgr, PTOtr, Pseg1, Pseg2, Pseg3, Pseg4, Pcr, Pdes, PLDfl, PLDgr) %>%
    mutate(t_total = cumsum(duration),
           Eeng_total = cumsum(Eduration),
           R_total = cumsum(Rduration),
           Wb_total = Eeng_total/(inp$E * inp$Etatotal),
           Wb_total = Wb_total * 1.05,
           Vb = Wb_total/inp$Dens)
  PowerSummary <- Power %>%
    select(type, h, Vinf, Vstall, Clmax, Cl, Cd, ClCd, theta, Drag, Power, t_total, Eeng_total, Wb_total, Vb)
  
  if (is.function(updateProgress)) {
    text <- paste0("Power - Total")
    updateProgress(detail = text, value = 14)
  }
  
  #--- Battery Fracs
  BatteryFracs <- Power %>%
    group_by(type) %>%
    top_n(1, t) %>%
    ungroup() %>%
    mutate(`%Wi/Wb` = Eeng/max(Eeng_total) * 100,
           `Wi/W` = Wb100 / inp$Etatotal * 1.05 / inp$W)
  Wb <- max(BatteryFracs$Wb_total)
  Wb_est <- filter(out1, type == "Cruise") %>%
    mutate(D = qinf * Cd * S, Wb = D / inp$Etatotal * 1.05 * 1.10) %>%
    ungroup() %>%
    select(Wb)
  Wb_est <- as.numeric(Wb_est)
  WeightFracs <- data.frame(
    Description = c("Payload", "Batteries", "Empty Weight", "110% * Initial Range Accuracy"),
    Value = c(inp$Mp/inp$m, Wb/inp$m, 1 - inp$Mp/inp$m - Wb/inp$m,
              (Wb_est - Wb)/Wb),
    Target = c(NA, NA, 0.40, 0)
  )
  
  if (is.function(updateProgress)) {
    text <- paste0("Power - Fractions")
    updateProgress(detail = text, value = 15)
  }
  
  #--- SUMMARY
  summary <- rbind(summary,
                   WeightFracs)
  
  iteration <- rbind(
    iteration,
    data.frame(
      Description = WeightFracs[1:3,1],
      Iteration = WeightFracs[1:3,2],
      Specification = c(NA, NA, 0.4),
      Minimise = c(0, 0, 0),
      Under = c(0, 0, 0),
      Over = c(0, 0,0)
    )
  )
  
  iteration <- rbind(
    iteration,
    data.frame(
      Description = c("m", "S", "Cd0", "e",
                      "W", "WS", "AR", "Clflaps", "Clhls", "P0eng"),
      Iteration = c(inp$m, inp$S, inp$Cd0, inp$e,
                    inp$W, inp$WS, inp$AR, inp$Clflaps, inp$Clhls, inp$P0eng),
      Specification = NA,
      Minimise = NA,
      Under = NA,
      Over = NA
    )
  )
  
## Return the result(s) ======================================================================
  if (out == "Summary")
    return(summary)
  else if (out == "Iteration")
    return(iteration)
  else if (out == "All")
    return(list(iteration = iteration, summary = summary, out1 = out1, AeroParamsTable = AeroParamsTable,
                out2 = out2, AccelerateStop = AccelerateStop, AccelerateContinue = AccelerateContinue, 
                AccelerateLiftoff = AccelerateLiftoff, BFL = BFL,
                out3 = out3, out4 = out4, DeccelerateLand = DeccelerateLand,
                WeightFracs = WeightFracs, BatteryFracs = BatteryFracs, Power = Power, PowerSummary = PowerSummary))
  
  if (is.function(updateProgress)) {
    text <- paste0("Done")
    updateProgress(detail = text, value = 20)
  }
}
## Plotting Functions ======================================================================
AeroParamsFunction <- function(inputvals, specifications){
  #--- Manipulate the data into a meaningful form
  inp  <- t(specifications["Value"])
  colnames(inp) <- t(specifications["Variable"])
  inp <- cbind(inputvals, inp)
  
  #--- AeroParams Plot
  AeroParamsPlot <- data.frame(Cl = seq(from=0, to=inp$Clclean, by=0.1)) %>%
    mutate(Cd = inp$Cd0 + inp$K * Cl^2)
  AeroParamsPlotPoints <- StandardAtomsphere(mutate(inp, h = as.double(inp$AltCruise))) %>%
    mutate(
      Clmax = Clclean,
      Vinf = Mach * a,
      Vstall = Vmin(rho, WS, Clmax),
      Vsafe = 1.2 * Vstall,
      Cdstall = Cd0 + K * Clmax^2,
      qinf = 1/2 * rho * Vinf^2,
      Cl = W / (qinf * S),
      Cd = Cd0 + K * Cl^2,
      Clstar = sqrt(Cd0 / K),
      Cdstar = 2 * Cd0,
      Vstar = Vmin(rho, WS, Clstar),
      Cl32 = sqrt(3) * Clstar,
      Cd32 = 2 * Cdstar,
      V32 = (1/3)^(1/4) * Vstar
    )
  AeroParamsPlotPoints <- data.frame(
    type = c("Cruise", "Stall", "(L/D)*", "L^(3/2)/D"),
    Vinf = c(AeroParamsPlotPoints$Vinf, AeroParamsPlotPoints$Vstall, 
             AeroParamsPlotPoints$Vstar, AeroParamsPlotPoints$V32),
    Cl = c(AeroParamsPlotPoints$Cl, AeroParamsPlotPoints$Clmax,
           AeroParamsPlotPoints$Clstar, AeroParamsPlotPoints$Cl32),
    Cd = c(AeroParamsPlotPoints$Cd, AeroParamsPlotPoints$Cdstall,
           AeroParamsPlotPoints$Cdstar, AeroParamsPlotPoints$Cd32)
  )
  
  #---Power and Thrust
  
  return(list(AeroParamsPlot = AeroParamsPlot, 
              AeroParamsPlotPoints = AeroParamsPlotPoints))
}

ClimbFunction <- function(inputvals, specifications, heights) {
  #--- Manipulate the data into a meaningful form
  inp  <- t(specifications["Value"])
  colnames(inp) <- t(specifications["Variable"])
  inp <- cbind(inputvals, inp)
  # Add in the various interested heights
  out <- inp[rep(row.names(inp), each = nrow(heights)), 1:length(inp)]
  out$type <- heights$type
  out$Ne <- heights$Ne
  out$h <- heights$h
  # Add in the standard atmosphere values
  out <- StandardAtomsphere(out) %>%
    mutate(
      Vflaps = Vmin(rho, WS, Clclean + Clflaps),
      Vmin = Vmin(rho, WS, Clclean), 
      Vcruise = Mach*a) %>%
    ungroup() %>%
    rowwise() %>%
    do(data.frame(.,
        Vinf = c(
          seq(.$Vflaps, .$Vmin, length.out = 10),
          seq(.$Vmin, .$Vmin * 1.2, length.out = 10),
          seq(.$Vmin * 1.2, .$Vcruise, length.out = 10),
          seq(.$Vcruise, .$Vcruise * 1.5, length.out = 20)),
        Vname = c(
          "Vflaps", rep("Vinf", 8), "Vstall",
          "Vstall", rep("Vinf", 8),"Vsafe",
          "Vsafe", rep("Vinf", 8), "Vcruise",
          "Vcruise", rep("Vinf", 19))
    )) %>%
    arrange(h, Vinf) %>%
    distinct(.keep_all = TRUE) %>%
    select(-Vmin,-Vcruise) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      qinf = 1/2 * rho * Vinf^2,
      Cl = W / (qinf * S),
      Cd = Cd0 + K * Cl^2,
      D = qinf * S * Cd,
      PA = PA(P0eng, sigma) * Ne,
      TA = PA / Vinf) %>%
    do(data.frame(., ClimbRatesFunction(.$PA, .$Cd0, .$rho, .$Vinf, .$S, .$K, .$W))) %>%
    ungroup() %>%
    group_by(type)
}

SecantRootUnivariate <- function(func, x1, x2, info = FALSE) {
  # reduce number of function calls by storing results as vars
  fx1 <-  func(x1); fx2 <- func(x2)
  fxr <- 10; loop = 1
  while (abs(fxr) > 0.00001 & loop < 50) {
    xr <- x2 - (fx2*(x1 - x2))/(fx1 - fx2)
    fxr <- func(xr)
    if (fx1 * fxr < 0) {
      x2 <- xr; fx2 <-  fxr
    } else {
      x1 <- xr; fx1 <- fxr
    }
    loop <- loop + 1
  }
  if (info == TRUE) {
    return(c(xr, fxr, loop))
  } else {
    return(xr)
  }
}

PR <- function(Vinf, rho, W, S, Cd0, K) {
  W * sqrt(2 / rho * W / S *
             (Cd0 + K * (2 / rho * W / S * 1 / (Vinf ^ 2)) ^ 2) ^ 2 /
             ((2 / rho * W / S * 1 / (Vinf ^ 2)) ^ 3))
}

PRmin <- function(rho, W, S, Cd0, K)
  (256 / 27) ^ 0.25 * (2 / rho * W / S) ^ 0.5 * (Cd0 * K ^ 3) ^ 0.25 * W

VmaxP <- function(PA, rho, W, S, Cd0, K, x1, x2, info = FALSE) {
  SecantRootUnivariate(function(Vinf)
    PA - PR(Vinf, rho, W, S, Cd0, K), x1, x2, info)
}

# minh=0;maxh=8000;nh=51;minv=10;maxv=150;nv=51;VmaxP1=1;VmaxP2=250

ThrustPowerCurves <- function(inputvals, specifications, minh, maxh, nh, minv, maxv, nv, VmaxP1, VmaxP2) {
  #--- Manipulate the data into a meaningful form
  inp  <- t(specifications["Value"])
  colnames(inp) <- t(specifications["Variable"])
  inp <- cbind(inputvals, inp)
  
  out <-  inp[rep(row.names(inp), each = nv*nh), 1:length(inp)]
  out$h = rep(seq(minh, maxh, length.out = nh), each = nv)
  out$Vinf = rep(seq(minv, maxv, length.out = nv), times = nh)
  out <- out %>%
    StandardAtomsphere(.) %>%
    group_by(h) %>%
    mutate(
      Clmax = Clclean,
      qinf = 1/2 * rho * Vinf,
      Cl = W * qinf * S,
      Cd = Cd0 + K * Cl^2,
      ClCd = Cl/Cd,
      ClCdstar = 1 / sqrt(4 * Cd0 * K),
      Vmin = Vmin(rho, WS, Clmax),
      Vstar = sqrt(2 / rho * W / S) * (K / Cd0) ^ 0.25,
      V32 = (1 / 3) ^ (1 / 4) * Vstar,
      Vcruise = Mach * a,
      PRmin = PRmin(rho, W, S, Cd0, K),
      PR = PR(Vinf, rho, W, S, Cd0, K),
      TRmin = W / ClCdstar,
      TR = W / ClCd,
      PA = PA(P0, sigma),
      Pexc = PA - PR,
      TA = PA/Vinf,
      Texc = TA-TR
    ) %>%
    filter(Pexc >= 0) %>%
    rowwise() %>%
    mutate(VmaxP = VmaxP(PA, rho, W, S, Cd0, K, VmaxP1, VmaxP2)) %>%
    ungroup()
}

