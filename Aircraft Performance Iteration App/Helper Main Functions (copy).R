#----------------------------
#--- Main Function
#============================
# These are the main functions that will run on the server

Vmin <- function(rho, WS, Clmax) 
  sqrt(2/rho * WS * 1/Clmax)

PA <- function(P0, sigma)
  P0 * sigma^0.7

Keff <- function(K, h, b)
  (33 * (h/b)^1.5) / (1 + 33 * (h/b)^1.5)  * K

groundmu <- data.frame(names = c("Dry Concrete", "Wet Concrete", "Icy Concrete"),
                       brakesoff = c(0.04, 0.05, 0.02), #NB: 0.03-0.05 for dry
                       brakeson_min = c(0.3, 0.15, 0.06),
                       brakeson_max = c(0.5, 0.3, 0.10)) %>%
  mutate(brakeson = (brakeson_min + brakeson_max)/2)

ClimbRatesFunction <- function(P, Cd0, rho, V, S, K, W) {
  a = P - 1/2 * Cd0 * rho * V^3 * S
  b = (2 * K * W^2) / (rho * V * S)
  c = W * V
  sintheta = c((c - sqrt(-4*a*b + 4*b^2 + c^2)) / (2*b),
               (c + sqrt(-4*a*b + 4*b^2 + c^2)) / (2*b))
  sintheta = sintheta[sintheta < 0.5 & sintheta > -0.5]
  if (length(sintheta) != 1) {
    return(data.frame(Theta = NA, SinTheta = NA, PerGrad = NA, ClimbRate = NA))
  }
  else {
    theta = asin(sintheta)
    return(data.frame(Theta = theta * 180 / pi, SinTheta = sintheta, PerGrad = sintheta*100, ClimbRate = sintheta * V))
    
  }
}

## Main Function ======================================================================
# THIS IS FOR CALCULATING SINGLE VALUES THAT WILL THEN GO INTO THE ITERATION FUNCTION
# Do not put graphing functions here!!
inp  <- t(specifications["Value"])
colnames(inp) <- t(specifications["Variable"])
inp <- cbind(inputvals, inp)

summary <- data.frame()

## AeroParams ======================================================================
out1 <-  inp[rep(row.names(inp), each = 5), 1:length(inp)]
out1$type <- c("Sea Level", "Cruise", "Ceiling", "Takeoff", "Landing")
out1$h <- c(0, inp$AltCruise, inp$AltCeil, 0, 0)
out1$Clmax <- inp$Clclean + c(0, 0, 0, inp$Clflaps, inp$Clhls)

out1 <- StandardAtomsphere(out1) %>%
  mutate(Vinf = Mach * a,
         Vstall = Vmin(rho, WS, Clmax),
         Vsafe = 1.2 * Vstall,
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
       filter(out1, type == "Cruise")$Vinf,
       filter(out1, type == "Cruise")$Vinf,
       filter(out1, type == "Cruise")$Vinf,
       filter(out1, type == "Cruise")$Cl,
       filter(out1, type == "Cruise")$ClCd,
       filter(out1, type == "Landing")$Vstall * 1.3
        ),
     Target = c(
       filter(out1, type == "Cruise")$Vstar,
       filter(out1, type == "Cruise")$Vstall,
       filter(out1, type == "Cruise")$Vsafe,
       filter(out1, type == "Cruise")$Clstar,
       filter(out1, type == "Cruise")$ClCdstar,
       inp$Vappmax
        )
   ))

## Takeoff ======================================================================
out2 <- inp[rep(row.names(inp), each = 3), 1:length(inp)]
out2$type <- c("All Engines", "One Engine Down", "Rejected Take-Off")
out2$Ne <- c(2, 1, 0)
out2$mu <- c(
  as.double(filter(groundmu,names == "Dry Concrete") %>% select(brakesoff)),
  as.double(filter(groundmu,names == "Dry Concrete") %>% select(brakesoff)),
  as.double(filter(groundmu,names == "Dry Concrete") %>% select(brakeson))
)

out2 <- mutate(out2, h = 0) %>%
  StandardAtomsphere(.) %>%
  mutate(Vstall = Vmin(rho, WS, Clclean + Clflaps),
         Vlof = 1.1 * Vstall)

velocities <- seq(1e-5,out2$Vlof[1], length.out = 50)
out2 <- out2[rep(row.names(out2),each=length(velocities)),1:length(out2)]
rownames(out2) <- NULL
out2$Vinf <- rep(velocities,3)

out2 <- mutate(out2, Clmax = Clclean + Clflaps, Keff = Keff(K, hground, b)) %>%
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
    qinf = 1/2 * rho * VTR^2, Cd = Cd0G + K * ClTR^2,
    D = qinf * S * Cd, L = qinf * S * ClTR
  ) %>%
  mutate(
    R = (VTR) ^ 2 / (0.2 * g),
    gamma = asin((TA - D) / W),
    hTR = R * (1 - cos(gamma)),
    ST = R * ((TA - D) / W),
    SC = (Hobs - hTR) / tan(gamma),
    Sair = ifelse(SC >0, ST + SC, sqrt(R^2 - (R-hTR)^2))
  )

AccelerateStop <- select(out2, Vinf, type, Area) %>%
  spread(type, Area) %>%
  mutate(`Accelerate-Stop` = `All Engines` - `Rejected Take-Off`)

AccelerateContinue <- select(out2, Vinf, type, Area) %>%
  spread(type, Area) %>%
  mutate(`One Engine Down` = max(`One Engine Down`) - `One Engine Down`)
AccelerateContinue$`Air Distance` = sum(filter(AirDistTO, type == "One Engine Down") %>% select(Sair))
AccelerateContinue <- mutate(AccelerateContinue, `Accelerate-Continue` = `All Engines` + `One Engine Down` + `Air Distance`)

AccelerateLiftoff <- select(out2, Vinf, type, Area) %>%
  spread(type, Area)
AccelerateLiftoff <- tail(AccelerateLiftoff,1)
AccelerateLiftoff$`Air Distance` = sum(filter(AirDistTO, type == "All Engines") %>% select(Sair))
AccelerateLiftoff <- mutate(AccelerateLiftoff, `Accelerate-Liftoff` = (`All Engines` + `Air Distance`) * 1.15)

#--- SUMMARY
summary <- rbind(
  summary,
  data.frame(
    Description = c(
      "Accelerate-Liftoff"
    ),
    Value = c(
      AccelerateLiftoff$`Accelerate-Liftoff`
    ),
    Target = c(
      inp$Srun
    )
  ))

## Climb ======================================================================
out3 <-  inp[rep(row.names(inp), each = 3), 1:length(inp)]
out3$type <- c("2nd Segment Climb", "Cruise", "Ceiling")
out3$Ne <- c(1, 2, 2)
out3$h <- c(inp$Hobs, inp$AltCruise, inp$AltCeil)
out3$Clmax <- inp$Clclean + c(inp$Clflaps, 0, 0)

out3 <- StandardAtomsphere(out3) %>%
  mutate(Vinf = Mach * a,
         Vstall = Vmin(rho, WS, Clmax),
         Vsafe = 1.2 * Vstall)
out3$Vinf <- c(out3$Vsafe[1], out3$Vinf[1], out3$Vinf[1])
out3 <- mutate(out3,
         qinf = 1/2 * rho * Vinf^2,
         Cl = W / (qinf * S),
         Cd = Cd0 + K * Cl^2,
         PA = PA(P0eng * Ne, sigma)
  ) %>%
  rowwise() %>%
  do(data.frame(., ClimbRatesFunction(.$PA, .$Cd0, .$rho, .$Vinf, .$S, .$K, .$W)))
#--- SUMMARY
summary <- rbind(
  summary,
  data.frame(
    Description = c(
      "2nd Segment Percentage Gradient",
      "Cruise Climb Rate",
      "Ceiling Climb Rate"
    ),
    Value = c(
      filter(out3, type == "2nd Segment Climb")$PerGrad,
      filter(out3, type == "Cruise")$ClimbRate,
      filter(out3, type == "Ceiling")$ClimbRate
    ),
    Target = c(
      inp$PerGrad2Seg,
      inp$ClimbCruise,
      inp$ClimbCeil
    )
  ))

## Landing ======================================================================
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
    TR = D + W * sin(gamma * pi / 180),
    PR = TR * Vapp,
    R = Vapp^2 / (0.2 * g),
    SF = R * sin(gamma * pi / 180),
    hF = R * (1 - cos(gamma * pi / 180)),
    SA = (50*0.3048 - hF) / tan(gamma*pi/180),
    Sair = ifelse(SA >0, SA + SF, sqrt(R^2 - (R-50*0.3048)^2)))

out4 <- inp
out4$type <- "Engines off"
out4$Ne <- 0
out4$mu <- as.double(filter(groundmu, names == "Dry Concrete") %>% select(brakeson))

out4 <- mutate(out4, h = 0) %>%
  StandardAtomsphere(.) %>%
  mutate(Vstall = Vmin(rho, WS, Clclean + Clhls),
         Vtd = 1.15 * Vstall)

velocities <- seq(out4$Vtd[1], 1e-5, length.out = 50)
out4 <- out4[rep(row.names(out4),each=length(velocities)),1:length(out4)]
out4$Vinf <- velocities

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

DeccelerateLand <- select(out4, Vinf, type, Area) %>%
  spread(type, Area)
DeccelerateLand <- head(DeccelerateLand,1)
DeccelerateLand$`Air Distance` = sum(filter(AirDistLD, type == "All Engines") %>% select(Sair))
DeccelerateLand <- mutate(DeccelerateLand, `Deccelerate-Land` = (`Engines off` + `Air Distance`) * 1.33)

#--- SUMMARY
summary <- rbind(
  summary,
  data.frame(
    Description = c(
      "Deccelerate Land"
    ),
    Value = c(
      DeccelerateLand$`Deccelerate-Land`
    ),
    Target = c(
      inp$Srun
    )
  ))

## Power ======================================================================
## DONT FORGET THE 1.05% SAFETY MARGIN!!!!! #########################################################
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
         R = cumsum(Rduration)) %>%
  select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, PA)
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
         R = cumsum(Rduration)) %>%
  select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, PA)

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
         R = cumsum(Rduration)) %>%
  select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, PA)

# Segment 2
# Flying at climb speed V2 with landing gear up and flaps in takeoff position
Pseg2num <- 10
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
         R = cumsum(Rduration)) %>%
  select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, PA)

# Segment 3
# Flying at constant altitude accelerating from V2 to VFS or 1.25 VS with flaps in TO
Pseg3num <- 15
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
         R = cumsum(Rduration)) %>%
  select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, PA)

# Segment 4
# Flying at climb speed Vcruise with landing gear up and flaps retracted
Pseg4num <- 15
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
         R = cumsum(Rduration)) %>%
  select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, PA)

#--- Descent
Pdes4num = 20
PdesHeights <- seq(inp$AltCruise, 50*0.3048, length.out = Pdes4num)
Vcr <- as.double(filter(AeroParamsTable, type == "Cruise") %>% select(Vinf))
PdesVelocities <- seq(Vcr, AirDistLD$Vapp, length.out = Pdes4num)
Pcr <- as.double(
  filter(out1, type == "Cruise") %>%
  mutate(D = qinf * S * Cd,
         PR = D * Vinf) %>%
  select(PR)
)
PdesPowers <- c(
  seq(Pcr, 1/8 * Pcr + 7/8* 0.05*inp$P0, length.out = ceiling(Pdes4num/8)),
  seq(1/8 * Pcr + 7/8* 0.05*inp$P0, 0.05*inp$P0, length.out = Pdes4num - ceiling(Pdes4num/8))
)
  # seq(1, 0, length.out = Pdes4num)^(6) * (Pcr - 0.1*inp$P0) + 0.1*inp$P0

Pdes <- inp
Pdes$type <- "Descent"
Pdes$Ne <- 2
Pdes <- Pdes[rep(row.names(Pdes), each = Pdes4num), 1:length(Pdes)]
rownames(Pdes) <- NULL
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
         PA = PR) %>%
  select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, PA)

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
         PA = PR) %>%
  select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, PA)

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
         R = cumsum(Rduration)) %>%
  select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, PA)

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
         PA = PR) %>%
  select(type, h, rho, Vinf, Vstall, a, Clmax, Cl, Cd, ClCd, theta, t, Eeng, Wb100, R, duration, Eduration, Rduration, PA)

#--- Total
Power <- rbind(PTOgr, PTOtr, Pseg1, Pseg2, Pseg3, Pseg4, Pcr, Pdes, PLDfl, PLDgr) %>%
  mutate(t_total = cumsum(duration),
         Eeng_total = cumsum(Eduration),
         R_total = cumsum(Rduration),
         Wb80_total = Eeng_total/(inp$E * 0.80),
         Wb80_total = Wb80_total * 1.05,
         Vb = Wb80_total/inp$Dens)

#--- Battery Fracs
BatteryFracs <- Power %>%
  group_by(type) %>%
  top_n(1, t) %>%
  ungroup() %>%
  mutate(`%Wi/Wb` = Eeng/max(Eeng_total)*100,
         `Wi/W` = Wb100 / 0.80 * 1.05 / inp$W)
Wb <- max(BatteryFracs$Wb80_total)
Wb_est <- filter(out1, type == "Cruise") %>%
  mutate(D = qinf * Cd * S, Wb = D / 0.80 * 1.05 * 1.1) %>%
  ungroup() %>%
  select(Wb)
Wb_est <- as.numeric(Wb_est)
WeightFracs <- data.frame(
  Description = c("Payload", "Batteries", "Empty Weight", "120% * Initial Range Accuracy"),
  Value = c(inp$Mp/inp$m, Wb/inp$m, 1 - inp$Mp/inp$m - Wb/inp$m,
            (Wb_est - Wb)/Wb),
  Target = c(NA, NA, 0.4, 0)
)

#--- SUMMARY
summary <- rbind(
  summary,
  WeightFracs
  )
#--- SUMMARY
summary <- rbind(
  summary,
  data.frame(
    Description = BatteryFracs$type,
    Value = BatteryFracs$`%Wi/Wb`,
    Target = NA))

## Graphing Functions ======================================================================


AeroParamsPlot <- data.frame(Cl = seq(from=0, to=inp$Clclean, by=0.1)) %>%
  mutate(Cd = inp$Cd0 + inp$K * Cl^2)

Power$type <- factor(Power$type, levels = Power$type)
ggplot(Power) + geom_line(aes(x=R_total, y = Eeng_total, colour = type))
ggplot(Power) + geom_line(aes(x=R_total, y = h, colour = type))
ggplot(Power) + geom_line(aes(x=R_total, y = theta, colour = type))

BatteryFracs$type <- factor(BatteryFracs$type, levels = BatteryFracs$type)
ggplot(BatteryFracs) + geom_bar(aes(type, weight = `%Wi/Wb`, colour = type))

WeightFracs$Description <- factor(WeightFracs$Description, levels = WeightFracs$Description)
ggplot(WeightFracs[1:3,1:2]) + geom_bar(aes(Description, weight = Value, colour = Description))
