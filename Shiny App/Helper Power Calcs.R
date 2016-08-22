#----------------------------
#--- Power Calcs
#============================

# Take-off 

# Segment 1
# should be covered in takeoff

# Segment 2
Pseg2num <- 3
Pseg2Heights <- seq(35*0.3, 400*0.3, length.out = Pseg2num)
Pseg2 <- cbind(inputvals, h = Pseg2Heights) %>%
  StandardAtomsphere(.) %>%
  mutate(Clmax = Clmax + Clflaps,
         Cd0 = Cd0G,
         Vinf = Vmin(rho, W, S, Clmax) * 1.2,
         qinf = qinf(rho, Vinf),
         PA = PA(sigma, P0)) %>%
  mutate(theta = 3) %>%
  mutate(Vv = Vinf * sin(theta *pi/180 )) %>%
  mutate(duration = 2 * (h - lag(h,1)) / (Vv + lag(Vv,1))) %>%
  mutate(duration = ifelse(is.na(duration), 0, duration),
         t = cumsum(duration)) %>%
  mutate(Pduration = 1/2 * (PA + lag(PA,1)) * duration,
         Pduration = ifelse(is.na(Pduration), 0, Pduration),
         P = cumsum(Pduration),
         Wb = P / 1e6)





