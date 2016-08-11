#----------------------------
#--- Functions
#============================

#--- General
# K
K <- function(AR, e) 1 / (pi * AR * e)
# Free stream velocity give Mach number and speed of sound
Vinf <- function(mach, a) mach*a
# Bernoulli value given free stream velocity and air density
qinf <- function(rho, Vinf) 1 / 2 * rho * Vinf^2
# Coefficient of lift at Vinf
Cl <- function(W, qinf, S) W / (qinf * S)
# Coefficient of drag from Cd0, K, Cl
Cd <- function(Cd0, K, Cl) Cd0 + K * Cl^2
# Lift Force
L <- function(qinf, S, Cl) qinf * S * Cl
# Draf Force
D <- function(qinf, S, Cd) qinf * S * Cd
# Lift on Drag
ClCd <- function(L, D) L / D

#---Thrust i.e. minimum thrust for (L/D)max
#   for prop, this gives the best range
#   for jet, this gives the best endurance
# Cl* 
Clstar <- function(Cd0, K) sqrt(Cd0/K)
# Cd*
Cdstar <- function(Cd0) 2 * Cd0
# (L/D)*
ClCdstar <- function(Cd0, K) 1 / sqrt(4 * Cd0 * K)
# V*
Vstar <- function(rho, W, S, K, Cd0) sqrt(2 / rho * W / S ) * (K / Cd0)^0.25
# u, dimensionless airspeed
U <- function(Vinf, Vstar) Vinf / Vstar

#---Power i.e. minimum thrust for (L^(3/2)/D)max
#   for prop, this gives the best endurance 
# Cl
Cl32 <- function(Clstar) sqrt(3) * Clstar
# Cd
Cd32 <- function(Cdstar) 2 * Cdstar # Double Check
# (L/D)
ClCd32 <- function(ClCdstar) sqrt(3/4) * ClCdstar
# V
V32 <- function(Vstar) (1/3)^(1/4) * Vstar

