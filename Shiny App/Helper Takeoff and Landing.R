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

AirDist <- function(Vstall, g, T, D, W, hobs) {
  R <- (1.15 * Vstall) ^ 2 / (0.2 * 9.81)
  gamma = asin((T - D) / W)
  hTR <- R * (1 - cos(gamma))
  ST = R * ((T - D) / W)
  SC = (hobs) / tan(gamma)
  return(c(gamma, 0, SC, hTR))
}

# Requires airplane data + various velocities
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
  rowwise() %>%
  mutate(gamma = ifelse(Vinf== Vlof, AirDist(Vstall, g, TA, D, W, 35*0.3)[1], NA),
         ST = ifelse(Vinf== Vlof, AirDist(Vstall, g, TA, D, W, 35*0.3)[2], NA),
         SC = ifelse(Vinf== Vlof, AirDist(Vstall, g, TA, D, W, 35*0.3)[3], NA),
         hTC = ifelse(Vinf== Vlof, AirDist(Vstall, g, TA, D, W, 35*0.3)[4], NA)) %>% 
  ungroup() %>%
  group_by(type) %>%
  mutate(Area = 1/2 * (arecip + lag(arecip,1)) * (Vsq - lag(Vsq,1)),
         Area = ifelse(is.na(Area), 0, Area),
         Area = cumsum(Area),
         Area = ifelse(is.na(Area), 0, Area))

AccelerateStop <- takeoff %>%
  select(Vinf, type, Area) %>%
  spread(type, Area) %>%
  mutate(AccelerateStop = `All Engines` - `Rejected Take-Off`)

AccelerateContinue <- takeoff %>%
  select(Vinf, type, Area) %>%
  spread(type, Area) %>%
  mutate(`One Engine Down` = max(`One Engine Down`) - `One Engine Down`)
AccelerateContinue$`Air Distance` = sum(tail(filter(takeoff, type == "One Engine Down") %>% ungroup() %>% select(ST, SC),1))
AccelerateContinue <- mutate(AccelerateContinue, AccelerateContinue = `All Engines` + `One Engine Down` + `Air Distance`)

AccelerateLiftoff <- takeoff %>%
  select(Vinf, type, Area) %>%
  spread(type, Area)
AccelerateLiftoff <- tail(AccelerateLiftoff,1)
AccelerateLiftoff$`Air Distance` = sum(tail(filter(takeoff, type == "All Engines") %>% ungroup() %>% select(ST, SC),1))
AccelerateLiftoff <- mutate(AccelerateLiftoff, AccelerateLiftoff = (`All Engines` + `Air Distance`) * 1.15)
# print.data.frame(AccelerateStop)

ggplot() + 
  geom_path(data = AccelerateStop, aes(x = Vinf, y = AccelerateStop, colour = "Accelerate-Stop")) +
  geom_path(data = AccelerateContinue, aes(x = Vinf, y = AccelerateContinue, colour = "Accelerate-Continue")) +
  geom_point(data = AccelerateLiftoff, aes(x = Vinf, y = AccelerateLiftoff, colour = "Accelerate-Liftoff")) +
  geom_hline(aes(yintercept = AccelerateLiftoff$AccelerateLiftoff, colour = "Accelerate-Liftoff")) +
  geom_hline(aes(yintercept = 1200, colour = "Maximum"))
  # In the future iteratively decrease the vstep until a solution is found for the curve intersections