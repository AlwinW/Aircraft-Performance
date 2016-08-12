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

#--- Power Required
# Minimum power i.e. (L^(3/2)/D)max @ V32
PRmin <- function(rho, W, S, Cd0, K) (256/27)^0.25 * (2/rho * W/S)^0.5 * (Cd0*K^3)^0.25
# Power required @ any airspeed
PR <- function(W, ClCd, Vinf) W/ClCd * Vinf


# Dummy power in level flight curve
nh <- 6; nv <- 51
test  <- data.frame(h = rep(seq(0,5000,length.out=nh),each=nv), Vinf = rep(seq(40,120,length.out=nv),times=nh))
test <- test %>%
  mutate(W = 115e3, S = 54.4, K = 0.0323, Cd0 = 0.02) %>%
  StandardAtomsphere(.) %>%
  group_by(h) %>%
  mutate(qinf = qinf(rho, Vinf),
    Cl = Cl(W, qinf, S),
    Cd = Cd(Cd0, K, Cl),
    ClCd = ClCd(Cl, Cd),
    PR = PR(W, ClCd, Vinf),
    Vstar = Vstar(rho, W, S, K, Cd0),
    V32 = V32(Vstar))
ggplot(test, aes(x=Vinf, y=PR, group=h, colour=as.factor(h))) +
  geom_path() +
  geom_point(aes(x=Vstar, y=0), shape = 1) +
  geom_point(aes(x=V32, y=0), shape = 2) +
  expand_limits(x = 0, y = 0)
#---Use the previous functions to mutate a dataframe
