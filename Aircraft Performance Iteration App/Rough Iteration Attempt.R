#----------------------------
#--- Iteration Attempt
#============================

## Initial Inputs ======================================================================
input_initial <- data.frame(
  S = 24.5,
  b = 22.1359,
  AR = 20,
  e = 0.8,
  K = 0.01989,
  Cd0 = 0.015,
  Clclean = 1.5,
  Clflaps = 1.2,
  Clhls = 1.5,
  m = 6500,
  W = 63742,
  WS = 2601.72,
  P0eng = 180000,
  P0 = 360000,
  Etatotal = 0.80,
  alt_s = 0,
  ClG = 0.25,
  Cd0G = 0.025,
  hground = 2.5
)

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

inputvals <- input_initial

## Iterations ======================================================================

# Initial Data Frame
iterationvals <- input_initial[rep(row.names(input_initial), each = 2), 1:length(input_initial)]
# Set the changing variable
iterationvals$Clhls <- c(1.2, 1.8)
# Convert dataframe to a list
iterationvals <- split(iterationvals, seq(nrow(iterationvals)))
# Give id names (these are preserved)
names(iterationvals) <- c("x", "y")

iterationout <- lapply(iterationvals, function(x) suppressWarnings(MainIterationFunction(x, specifications, out = "Iteration")))
iterationout <- do.call("rbind", iterationout)

iterationout <-  iterationout %>%
  gather(key, value, -id, - Description) %>% 
  unite(temp, Description, key) %>%
  spread(temp, value)

melt(iterationout, id.vars = c("Description", "Specification", "Minimise", "Under", "Over"),
     variable.name = "key",
     value.name = "value")






