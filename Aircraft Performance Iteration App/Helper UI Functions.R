#----------------------------
#--- Functions for UI
#============================
# These are genreal functions that do NOT require observe, require, etc

## Initial Inputs ======================================================================
input_initial <- data.frame(
  S = 24.5,
  b = 22.1359,
  AR = 20,
  e = 0.8,
  K = 0.01989,
  
  Cd0 = 0.015,
  Clclean = 1.7,
  Clflaps = 1.0,
  Clhls = 1.3,
  
  m = 6500,
  W = 63742,
  WS = 2601.72,
  
  P0eng = 180000,
  P0 = 360000,
  Etatotal = 0.80,
  
  ClG = 0.25,
  Cd0G = 0.025,
  hground = 2.5
)

resolution = 10
inputvals <- input_initial

## Specifications ======================================================================
specifications <- data.frame(
  Variable = c("Mp", "Wp", "E", "Dens", "Srun", "Hobs", "Vappmax", "PerGrad2Seg", "ClimbCruise", "ClimbCeil", "AltCruise", "AltCeil", 
               "Mach", "Range", "LoadMax", "LoadMin"),
  Description = c("Payload Mass (kg)","Payload Weight (N)","Battery Specific Energy (J/kg)","Battery Density (kg/m^3)",
                  "Runway Length (m)","Screen Height (m)","Maximum Landing Speed (m/s)","Climb Gradient 2nd Segment (%)",
                  "Climb at Cruise (m/s)","Climb at Ceiling (m/s)","Altitude of Cruise (m)","Altitude of Ceiling (m)",
                  "Cruise Mach Number (M)","Range (m)","Load Factor Max (n)","Load Factor Min (n)"),
  Value = c(120 * 6, 120 * 6 * 9.8065, 1e6, 2750, 1200, 35 * 0.3048, 100 * 0.5144, 1.5, 300 * 0.3048 / 60,
            100 * 0.3048 / 60, 10000 * 0.3048, 12000 * 0.3048, 0.25, 1000e3, 3.5, -1.5)
)
  
## Plot Interaction ======================================================================
xy_str <- function(e) {
  if(is.null(e)) return("NULL\n")
  paste0("x=", round(e$x, 4), " y=", round(e$y, 4), "\n")
}

xy_range_str <- function(e) {
  if(is.null(e)) return("NULL\n")
  paste0("xmin=", round(e$xmin, 4), " xmax=", round(e$xmax, 4), 
         " ymin=", round(e$ymin, 4), " ymax=", round(e$ymax, 4))
}