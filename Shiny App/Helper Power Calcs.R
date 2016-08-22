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

