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

#---Weight Estimate
# Dimensional constants
RaymerClass <-
  data.frame(
    name = c("General Aviation - twin engine", "Twin Turboprop"),
    A = c(1.4, 0.92),
    C = c(-0.10,-0.05)
  )
# Raymer's Equivalent curve fits for We/W0
RaymerFit <- function(W0, type) {
  coef <- RaymerClass %>% filter_(interp(quote(name == x), x = type))
  return(coef$A * W0 ^ coef$C)
}
BatteryFrac <- function(R, g, E, eta, ClCd)
  (R * g) / (E * eta * ClCd)

# Weight estimation (Actually comes out in kg!!)
WeightEst <- function(Wpp, R, g, E, eta, ClCd, type, x1, x2, info) {
  SecantRootUnivariate(function(W0)
    W0 - Wpp / (1 - BatteryFrac(R, g, E, eta, ClCd) - RaymerFit(W0, type)), x1, x2, info)
}
WeightEst(120 * 6, 1000e3, 9.81, 1e6, 0.8, 10, "Twin Turboprop", 10, 5000, info = TRUE)

WeightEstPlot <- function(W0, Wpp, R, g, E, eta, ClCd, type) {
  W0 - Wpp / (1 - BatteryFrac(R, g, E, eta, ClCd) - RaymerFit(W0, type))
}
West = data.frame(W0 = seq(100, 5000, by = 100))
West <-
  mutate(West,
         Wb = BatteryFrac(1000e3, 9.81, 1e6, 0.9, 20),
         We = RaymerFit(W0, "Twin Turboprop"))

ggplot(West) + geom_point(aes(x = W0, y = Wb))  + geom_point(aes(x = W0, y = We)) +
  expand_limits(x = 0, y = 0)



plot(W0,
     WeightEstPlot(W0, 120 * 6, 1000e3, 9.81, 1e6, 0.8, 20, "Twin Turboprop"))

#---Use the previous functions to mutate a dataframe
