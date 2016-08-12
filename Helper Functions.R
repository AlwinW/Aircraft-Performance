#----------------------------
#--- Functions
#============================

#--- General
# K
K <- function(AR, e)
  1 / (pi * AR * e)
# Free stream velocity give Mach number and speed of sound
Vinf <- function(mach, a)
  mach * a
# Bernoulli value given free stream velocity and air density
qinf <- function(rho, Vinf)
  1 / 2 * rho * Vinf ^ 2
# Coefficient of lift at Vinf
Cl <- function(W, qinf, S)
  W / (qinf * S)
# Coefficient of drag from Cd0, K, Cl
Cd <- function(Cd0, K, Cl)
  Cd0 + K * Cl ^ 2
# Lift Force
L <- function(qinf, S, Cl)
  qinf * S * Cl
# Draf Force
D <- function(qinf, S, Cd)
  qinf * S * Cd
# Lift on Drag
ClCd <- function(L, D)
  L / D
# Stall
Vmin <- function(rho, W, S, Clmax)
  sqrt(2 / rho * W / S * 1 / Clmax)

#---Thrust i.e. minimum thrust for (L/D)max
#   for prop, this gives the best range
#   for jet, this gives the best endurance
# Cl*
Clstar <- function(Cd0, K)
  sqrt(Cd0 / K)
# Cd*
Cdstar <- function(Cd0)
  2 * Cd0
# (L/D)*
ClCdstar <- function(Cd0, K)
  1 / sqrt(4 * Cd0 * K)
# V*
Vstar <-
  function(rho, W, S, K, Cd0)
    sqrt(2 / rho * W / S) * (K / Cd0) ^ 0.25
# u, dimensionless airspeed
U <- function(Vinf, Vstar)
  Vinf / Vstar

#---Power i.e. minimum thrust for (L^(3/2)/D)max
#   for prop, this gives the best endurance
# Cl
Cl32 <- function(Clstar)
  sqrt(3) * Clstar
# Cd
Cd32 <- function(Cdstar)
  2 * Cdstar # Double Check
# (L/D)
ClCd32 <- function(ClCdstar)
  sqrt(3 / 4) * ClCdstar
# V
V32 <- function(Vstar)
  (1 / 3) ^ (1 / 4) * Vstar

#--- Power Required
# Minimum power i.e. (L^(3/2)/D)max @ V32
PRmin <-
  function(rho, W, S, Cd0, K)
    (256 / 27) ^ 0.25 * (2 / rho * W / S) ^ 0.5 * (Cd0 * K ^ 3) ^ 0.25 * W
# Power required @ any airspeed
PR <- function(Vinf, rho, W, S, Cd0, K) {
  W * sqrt(2 / rho * W / S *
             (Cd0 + K * (2 / rho * W / S * 1 / (Vinf ^ 2)) ^ 2) ^ 2 /
             ((2 / rho * W / S * 1 / (Vinf ^ 2)) ^ 3))
}

#--- Thrust Required
TRmin <- function(W, ClCdstar)
  W / ClCdstar
TR <- function(W, ClCd)
  W / ClCd

#--- Altitude Effect
# Altitude constants
alt_r = 0.5
alt_s = 0.7
# Power Available
PA <- function(sigma, P0)
  sigma ^ 0.7 * P0
# Excess Power
Pexc <- function(PA, PR)
  PA - PR
# Thrust Available
TA <- function(PA, Vinf)
  PA / Vinf
# Excess Thrust
Texc <- function(TA, TR)
  TA - TR

#---Maximum speed @ altitude
VmaxP <- function(PA, rho, W, S, Cd0, K, x1, x2, info = FALSE) {
  SecantRootUnivariate(function(Vinf)
    PA - PR(Vinf, rho, W, S, Cd0, K), x1, x2, info)
}
# VmaxP(3060e3, 1.225, 155e3, 54.4, 0.02, 0.0323, 100, 200)



# Dummy power in level flight curve
nh <- 6
nv <- 51
powerthrustcurves  <-
  data.frame(h = rep(seq(0, 5000, length.out = nh), each = nv),
             Vinf = rep(seq(40, 120, length.out = nv), times = nh))
powerthrustcurves <- powerthrustcurves %>%
  mutate(
    W = 155e3,
    S = 54.4,
    K = 0.0323,
    Cd0 = 0.02,
    P0 = 3060e3,
    Clmax = 1.2
  ) %>%
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
    Pexc = Pexc(PA, PR)
  ) %>%
  rowwise() %>%
  mutate(VmaxP = VmaxP(PA, rho, W, S, Cd0, K, 50, 200))
# POWER REQUIRED
ggplot(powerthrustcurves,
       aes(
         x = Vinf,
         y = PR,
         group = h,
         colour = as.factor(h)
       )) +
  geom_path() +
  geom_point(aes(x = V32, y = PRmin), shape = 1) +
  geom_point(aes(x = Vstar, y = TRmin * Vstar), shape = 2) +
  expand_limits(x = 0, y = 0)
# THRUST REQUIRED
ggplot(powerthrustcurves,
       aes(
         x = Vinf,
         y = TR,
         group = h,
         colour = as.factor(h)
       )) +
  geom_path() +
  geom_point(aes(x = V32, y = PRmin / V32), shape = 1) +
  geom_point(aes(x = Vstar, y = TRmin), shape = 2) +
  expand_limits(x = 0, y = 0)
# POWER EXCESS
ggplot(powerthrustcurves,
       aes(
         x = Vinf,
         y = Pexc,
         group = h,
         colour = as.factor(h)
       )) +
  geom_path() +
  geom_point(aes(x = V32, y = PA - PRmin), shape = 1) +
  geom_point(aes(x = Vstar, y = PA - TRmin * Vstar), shape = 2) +
  expand_limits(x = 0, y = 0)

# Operating Window
nh <- 51
nv <- 51
operatingwindow  <-
  data.frame(h = rep(seq(0, 12500, length.out = nh), each = nv),
             Vinf = rep(seq(0, 200, length.out = nv), times = nh))
operatingwindow <- operatingwindow %>%
  mutate(
    W = 155e3,
    S = 54.4,
    K = 0.0323,
    Cd0 = 0.02,
    P0 = 3060e3,
    Clmax = 1.2
  ) %>%
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
    Pexc = Pexc(PA, PR)
  ) %>%
  rowwise() %>%
  mutate(VmaxP = VmaxP(PA, rho, W, S, Cd0, K, 1, 250))

# Velocities
ggplot(operatingwindow) +
  geom_path(aes(x = Vmin, y = h, colour = "Stall Speed")) +
  geom_path(aes(x = Vmin * 1.2, y = h, colour = "Safety Factor")) +
  geom_path(aes(x = VmaxP, y = h, colour = "Maximum Speed")) +
  scale_color_manual(values=c("Stall Speed"="red", "Safety Factor"="orange",
                              "Maximum Speed"="purple"))
# Excess Power
ggplot(operatingwindow) +
  geom_point(data=filter(operatingwindow,Pexc>=0), aes(x=Vinf, y=h, colour=Pexc)) +
  geom_path(aes(x = Vmin, y = h), colour = "red") +
  geom_path(aes(x = Vmin * 1.2, y = h), colour = "orange") +
  geom_path(aes(x = VmaxP, y = h), colour = "purple") +
  scale_colour_gradientn(colours = brewer.pal(5, "RdYlGn"),
                       guide = "colourbar",
                       name = "Excess Power")

#---Use the previous functions to mutate a dataframe
