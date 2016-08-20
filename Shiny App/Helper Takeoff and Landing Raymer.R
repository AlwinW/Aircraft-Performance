#----------------------------
#--- Takeoff and Landing
#============================
# Using methods described on page 486 of Raymer's Aircraft Design - A conceptual Approach book.

groundmu <- data.frame(names = c("Dry Concrete", "Wet Concrete", "Icy Concrete"),
                       brakesoff = c(0.04, 0.05, 0.02), #NB: 0.03-0.05 for dry
                       brakeson_min = c(0.3, 0.15, 0.06),
                       brakeson_max = c(0.5, 0.3, 0.10))

## TAKE OFF ############
# Accelerating along the flat ground
GroundAccel <- function(g, L, W, T,  D, mu) 
  g/W * (T - D - mu*(W - L))
GroundAccelapprox <- function(g, Cl, W, T, Cd0, mu, rho, K, V)
  g*(((T/W) - mu) + rho/(2*W/S) * (-Cd0 - K * Cl^2 + mu*Cl) * V^2)

# The aircraft has rolled and is starting to climb (transition)
TransitionRadius <- function(VTR, g) # R, ASSUMING n = 1.2
  VTR^2 / (0.2 * g)
TransitionAngle <- function(T, D, W) # gamma
  asin( (T - D) / W )
TransitionAngleapprox <- function(T, W, LD) # gamma approximation
  asin( T / W - 1/LD)
TransitionDist <- function(R, T, D, W)  # General transition distance ST
  R * (T - D)/W
TransitionDistapprox <- function(R, T, W, LD, gamma) #hTR
  R * (1 - cos(gamma))
TransitionDistObstical <- function(R, hTR) # ST for clearning an obstacle
  sqrt(R^2 - (R - hTR)^2)

# Climb to pass an obstical (e.g. 50ft for military)
ClimbDist <- function(hobs, hTR, gamma)
  (hobs - hTR) / tan(gamma)

# Balanced Field Length (BFL) [FEET]
# Total takeoff distance including obstacle clearance
Thrustav_prop_Raymer <- function(bhp, sigma, Ne, Dp) #Tav
  5.75 * bhp * ((sigma * Ne * Dp^2)/bhp) 
      # Ne -- number of engines, Dp -- propeller diameter (ft), bhp -- brakehorsepower
# Units of Weight?? Cl @ 1.2 Vstall
BFL_Raymer_ft <- function(gamma_oneout, D, W, S, rho, g, Clclimb, hobs, Clmax, sigma) {
  gamma_min <- 0.024 # 2 Engines
  G = gamma_oneout - gamma_min
  U = 0.01*Clmax + 0.02 # Flaps in take-off position
  BFL = 0.863/(1 + 2.3*G) * ((W/S) / (rho*g*Clclimb) + hobs) * (1/((Tav/ W) - U) + 2.7) +
    (655/sqrt(sigma))
}
# Alternate method: 
#   Integrate takeoff roll with an engine failture at V1 and compare to
#   braking analysis at V1 using landing analysis methods. reverse thrust not counted
#   Require: takeoff (inc hobs) = braking. 

## LANDING ###########
# Velocities
Vapp <- function(Vstall)
  1.3 * Vstall
VTD <- function(Vstall) # Decelerates during 'flare'
  1.15 * Vstall 
Vflare <- function(Vstall)
  1.23 *  Vstall
# Ground Roll
GroundFreeRoll <- function(VTD)
  VTD * 2   # Assume 1-3 seconds of reaction delay
# FAR Field length is 1.666* sum(approach, flare, free ground roll, braking ground roll)



