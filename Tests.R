#----------------------------
#--- Test
#============================

library(lazyeval)
library(tidyr)
library(dplyr)
library(purrr)
library(broom)
library(ggplot2)

test <- data.frame(W = 115e3, S = 54.4, K = 0.0369, Cd0 = 0.02, rho = 0.9091)
test <-  rbind(test, test*1.1)

test <- test %>% mutate(Vinf = 164.2919, qinf = qinf(rho, Vinf), Cl = Cl(W, qinf, S), Cd = Cd(Cd0, K, Cl),
                L = L(qinf, S, Cl), D = D(qinf, S, Cd), ClCd = ClCd(Cl, Cd),
                Clstar = Clstar(Cd0, K), Cdstar = Cdstar(Cd0), ClCdstar = ClCdstar(Cd0, K), 
                Vstar = Vstar(rho, W, S, K, Cd0), U = U(Vinf, Vstar),
                Cl32 = Cl32(Clstar), Cd32 = Cd32(Cdstar), ClCd32 = ClCd32(ClCdstar), V32 = V32(Vstar))
