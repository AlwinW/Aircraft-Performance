#----------------------------
#--- Power Calcs
#============================

# Take-off 

# Segment 1
# should be covered in takeoff

# Segment 2
Pseg2num <- 5
Pseg2Heights <- seq(35*0.3, 400*0.3, length.out = Pseg2num)
Pseg2 <- cbind(inputvals, h = Pseg2HeightsPseg2num) %>%
  StandardAtomsphere(.) %>%
  mutate(Cl = Clmax + Clflaps,
    Vinf = Vmin(rho, W, S, Cl)) %>%
    

