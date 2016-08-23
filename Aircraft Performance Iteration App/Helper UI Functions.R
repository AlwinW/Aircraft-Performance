#----------------------------
#--- Functions for UI
#============================
# These are genreal functions that do NOT require observe, require, etc

## Initial Inputs ======================================================================
input_initial <- data.frame(
  S = 18,
  b = 20.35,
  AR = 23,
  e = 0.9,
  K = 0.01538,
  
  Cd0 = 0.02,
  Clclean = 1.7,
  Clflaps = 0.8,
  Clhls = 1.2,
  
  m = 7500,
  W = 73548,
  WS = 4086,
  
  P0eng = 375000,
  P0 = 750000,
  
  ClG = 0.25,
  Cd0G = 0.035,
  hground = 2.5
)

inputvals <- input_initial

## Specifications ======================================================================
specifications <- data.frame(
  Variable = c("Mp", "Wp", "E", "Dens", "Srun", "Hobs", "Vappmax", "PerGrad2Seg", "ClimbCruise", "ClimbCeil", "AltCruise", "AltCeil", 
               "Mach", "Range", "LoadMax", "LoadMin"),
  Description = c("Payload Mass (kg)","Payload Weight (N)","Battery Specific Energy (J/kg)","Battery Density (kg/m^3)",
                  "Runway Length (m)","Screen Height (m)","Maximum Landing Speed (m/s)","Climb Gradient 2nd Segment (%)",
                  "Climb at Cruise (m/s)","Climb at Ceiling (m/s)","Altitude of Cruise (m)","Altitude of Ceiling (m)",
                  "Cruise Mach Number (M)","Range (m)","Load Factor Max (n)","Load Factor Min (n)"),
  Value = c(20 * 6, 720 * 6 * 9.8065, 1e6, 2750, 1200, 35 * 0.3048, 100 * 0.5144, 1.5, 300 * 0.3048 / 60,
            100 * 0.3048 / 60, 10000 * 0.3048, 12000 * 0.3048, 0.25, 1000e3, 3.5, -1.5)
)
  