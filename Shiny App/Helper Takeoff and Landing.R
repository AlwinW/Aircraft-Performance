#----------------------------
#--- Takeoff and Landing
#============================
# Using generalised methods
# Need to have a more general Air Dist based on TRUE climb and Cl


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


ggplot() + 
  geom_path(data = AccelerateStop, aes(x = Vinf, y = AccelerateStop, colour = "Accelerate-Stop")) +
  geom_path(data = AccelerateContinue, aes(x = Vinf, y = AccelerateContinue, colour = "Accelerate-Continue")) +
  geom_point(data = AccelerateLiftoff, aes(x = Vinf, y = AccelerateLiftoff, colour = "Accelerate-Liftoff")) +
  geom_hline(aes(yintercept = AccelerateLiftoff$AccelerateLiftoff, colour = "Accelerate-Liftoff")) +
  geom_hline(aes(yintercept = 1200, colour = "Maximum"))
  # In the future iteratively decrease the vstep until a solution is found for the curve intersections





## Landing ======================================================================
landing <- inputvals
landing$type <- c("All Engines")
landing$Ne <- c(0) # Change the last one from 0 to say -0.5 if reverse availalbe
landing$mu <- c(as.double(filter(groundmu,names == "Dry Concrete") %>% select(brakeson)))
landing <- landing %>%
  mutate(h = 0) %>%
  StandardAtomsphere(.) %>%
  mutate(Vstall = Vmin(rho, W, S, Clmax + Clhls),
         Vapp = 1.3 * Vstall,
         Vfla = 1.15 * Vstall)

# Need to find the upper 1.2 Vstall
velocities <- seq(landing$Vfla[1], 0.0001, length.out = 50)
landing <- landing[rep(row.names(landing),each=length(velocities)),1:length(landing)]
rownames(landing) <- NULL
landing$Vinf <- rep(velocities,1)

landing$ClG <-  landing$ClG
landing$Keff <-  Keff(landing$K, inputvals$hground, landing$b)
landing <- landing %>%
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

AirDistVals <- inputvals
rownames(AirDistVals) <- NULL
AirDistVals$type <- c("All Engines")
AirDistVals$Ne <- c(2)
AirDistVals <- AirDistVals %>%
  mutate(h = 50*0.3) %>%
  StandardAtomsphere(.) %>%
  mutate(
    Vstall = Vmin(rho, W, S, Clmax + Clhls),
    Vinf = 1.3 * Vstall,
    qinf = qinf(rho, Vinf),
    gamma = 3,
    L = W * cos(gamma * pi / 180),
    Cl = L / (qinf * Vinf),
    Cd = Cd0G + K * Cl^2,
    D = D(qinf, S, Cd),
    TR = D - W * sin(gamma * pi / 180),
    PR = TR * Vinf,
    R = Vinf^2 / (0.2 * g),
    SF = R * sin(gamma * pi / 180),
    hF = R * (1 - cos(gamma * pi / 180)),
    SA = (50*0.3 - hF) / tan(gamma*pi/180))


DeccelerateLand <- landing %>%
  select(Vinf, Vapp, Vfla, type, Area) %>%
  spread(type, Area)
DeccelerateLand <- head(DeccelerateLand,1)
DeccelerateLand$`Air Distance` = sum(AirDistVals %>% select(SA, SF))
DeccelerateLand <- mutate(DeccelerateLand, DeccelerateLand = (`All Engines` + `Air Distance`) * 1.33)


