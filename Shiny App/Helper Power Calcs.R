#----------------------------
#--- Power Calcs
#============================

# Take-off 

# Segment 1
# should be covered in takeoff

# Segment 2
Pseg2num <- 1
Pseg2Heights <- seq(35*0.3, 400*0.3, length.out = Pseg2num)
Pseg2 <- cbind(inputvals, h = Pseg2Heights) %>%
  StandardAtomsphere(.) %>%
  mutate(Clmax = Clmax + Clflaps,
         Cd0 = Cd0G,
         Vinf = Vmin(rho, W, S, Clmax) * 1.2,
         qinf = qinf(rho, Vinf)) %>%
  rowwise() %>%
  do(optimout = optim(0.1, function(n) - (Vinf * sqrt(1-n^2) - (P0 - Vinf*Cd0*qinf*S - Vinf*n^2*W^2/(qinf*S)) / W), data = .))


optim(0.1, function(n) -(Pseg2$Vinf * sqrt(1-n^2) - (Pseg2$P0 - Pseg2$Vinf*Pseg2$Cd0*Pseg2$qinf*Pseg2$S - Pseg2$Vinf*n^2*Pseg2$W^2/(Pseg2$qinf*Pseg2$S)) / Pseg2$W))


angle <- function(n) 
  Pseg2$Vinf * sqrt(1-n^2) - 
    (Pseg2$P0 - Pseg2$Vinf*Pseg2$Cd0*Pseg2$qinf*Pseg2$S - Pseg2$Vinf*n^2*(Pseg2$W^2)/(Pseg2$qinf*Pseg2$S)) 
      / Pseg2$W

SecantRootUnivariate(angle(n), 0.1, 0.9) 

W * Vinf * sin(theta) - P - V * qinf * S (C0d + K * (W * cos(theta) / (qinf * S)))

Pseg2 %>%
  mutate(optimout = 
    optim(0.5,
           function(theta) -(W * Vinf * sin(theta) - P - V * qinf * S (C0d + K * (W * cos(theta) / (qinf * S)))))
  )




