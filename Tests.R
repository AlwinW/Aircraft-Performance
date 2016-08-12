#----------------------------
#--- Test
#============================

library(lazyeval)
library(tidyr)
library(dplyr)
library(purrr)
library(broom)
library(ggplot2)
library(RColorBrewer)
library(lazyeval)

test <- data.frame(W = 115e3, S = 54.4, K = 0.0369, Cd0 = 0.02, rho = 0.9091)
test <-  rbind(test, test*1.1)

test <- test %>% mutate(Vinf = 164.2919, qinf = qinf(rho, Vinf), Cl = Cl(W, qinf, S), Cd = Cd(Cd0, K, Cl),
                L = L(qinf, S, Cl), D = D(qinf, S, Cd), ClCd = ClCd(Cl, Cd),
                Clstar = Clstar(Cd0, K), Cdstar = Cdstar(Cd0), ClCdstar = ClCdstar(Cd0, K),
                Vstar = Vstar(rho, W, S, K, Cd0), U = U(Vinf, Vstar),
                Cl32 = Cl32(Clstar), Cd32 = Cd32(Cdstar), ClCd32 = ClCd32(ClCdstar), V32 = V32(Vstar))


# Dummy power in level flight curve
nh <- 11
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
    Pexc = Pexc(PA, PR),
    TA = TA(PA, Vinf),
    Texc = Texc(TA, TR)
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
# THRUST EXCESS
ggplot(powerthrustcurves,
       aes(
         x = Vinf,
         y = Texc,
         group = h,
         colour = as.factor(h)
       )) +
  geom_path() +
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
    Pexc = Pexc(PA, PR),
    TA = TA(PA, Vinf),
    Texc = Texc(TA, TR)
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