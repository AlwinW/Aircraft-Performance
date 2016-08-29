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

# Consider try using False Position method or something to try find a braketed root!

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

# Things to Vary
var_m = seq(5500, 6500, 500)
var_e = seq(0.8, 0.95, 0.05)

# Apply the variations
iterationvals <- input_initial[rep(rownames(input_initial), each = length(var_e) * length(var_m)), 1:length(input_initial)]
iterationvals$m <- rep(var_m, each = length(var_e))
iterationvals$e <- rep(var_e, times = length(var_m))
  
# Update the other parameters based on m, e, etc (functionalise)
UpdateParams <- function(input) {
  input$b <- sqrt(input$AR * input$S)
  input$K <- 1/(pi * input$AR * input$e)
  input$W <- input$m * 9.8065
  input$WS <- input$W / input$S
  input$P0 <- input$P0eng * 2
  return(input)
}

# Apply the updates to the parameters
iterationvals <- UpdateParams(iterationvals)


# Test for convergence / root finding
var_Clflaps = seq(0.2, 2.5, 0.1)
iterationvals <- input_initial[rep(rownames(input_initial), each =length(var_Clflaps)), 1:length(input_initial)]
iterationvals$Clflaps <- rep(var_Clflaps)
iteration <- list()
for (i in 1:length(var_Clflaps)) {
  iterationvals0 <- UpdateParams(iterationvals[i,])
  iteration[[i]] <- MainIterationFunction(iterationvals0, specifications, out = "Iteration")
}

iterationlong <-  melt(iteration,
     id.vars = c("Description", "Specification",
                 "m", "S", "Cd0",
                 "W", "WS", "AR", "Clflaps", "Clhls", "P0eng",
                 "Minimise", "Under", "Over"),
     variable.name = "key",
     value.name = "value")

# THIS IS SORT OF WHAT I WANT!!
head(gather(iterationlong, name, value, -L1, -Description) %>% spread(Description, value))

ggplot(iterationlong) +
  



inputvals <- input_initial

iterationvals <- input_initial

IterationCalcs <- function(iterationvals){
  iterationvals <- iterationvals %>%
    mutate(b = sqrt(AR * S),
           K = 1/(pi * AR * e),
           W = m * 9.8065,
           WS = W/S,
           P0 = P0eng * 2)
  iteration0 <- suppressWarnings(MainIterationFunction(iterationvals, specifications, out = "Iteration"))
  
  # Power
  while (iteration0[6,2] <= iteration0[6,3] |
         iteration0[7,2] <= iteration0[7,3] |
         iteration0[8,2] <= iteration0[8,3]) {
    iterationvals$P0eng = iterationvals$P0eng + 5000
    iterationvals <- iterationvals %>%
      mutate(b = sqrt(AR * S),
             K = 1/(pi * AR * e),
             W = m * 9.8065,
             WS = W/S,
             P0 = P0eng * 2)
    iteration0 <- suppressWarnings(MainIterationFunction(iterationvals, specifications, out = "Iteration"))
  }
  
  # Takeoff
  while (iteration0[4,2] >= iteration0[4,3] |
         iteration0[5,2] >= iteration0[5,3]) {
    iterationvals$Clflaps = iterationvals$Clflaps + 0.025
    iteration0 <- suppressWarnings(MainIterationFunction(iterationvals, specifications, out = "Iteration"))
  }
  
  # Landing
  while (iteration0[9,2] >= iteration0[9,3]) {
    iterationvals$Clhls = iterationvals$Clhls + 0.025
    iteration0 <- suppressWarnings(MainIterationFunction(iterationvals, specifications, out = "Iteration"))
  }
  
  # Weight Analysis
  while (iteration0[12,2] <= iteration0[12,3]) {
    iterationvals$AR = iterationvals$AR + 0.5
    iterationvals <- iterationvals %>%
      mutate(b = sqrt(AR * S),
             K = 1/(pi * AR * e),
             W = m * 9.8065,
             WS = W/S,
             P0 = P0eng * 2)
    iteration0 <- suppressWarnings(MainIterationFunction(iterationvals, specifications, out = "Iteration"))
  }
  
  return(iteration0)
}


iterationout <- lapply(iterationvals, function(x) IterationCalcs(x))


## Iterations ======================================================================

# Initial Data Frame
iterationvals_df <- input_initial[rep(row.names(input_initial), each = 2), 1:length(input_initial)]
# Set the changing variable
iterationvals_df$m <- c(6000, 6500)
# Convert dataframe to a list
# iterationvals_df <- split(iterationvals, seq(nrow(iterationvals)))
# Give id names (these are preserved)
# names(iterationvals) <- c("x", "y")

plot = list()

for (i in 1:2) {
  iterationvals <- iterationvals_df[i,]
  iterationvals$b = sqrt(iterationvals$AR * iterationvals$S)
  iterationvals$K = 1/(pi * iterationvals$AR * iterationvals$e)
  iterationvals$W = iterationvals$m * 9.8065
  iterationvals$WS = iterationvals$W/iterationvals$S
  iterationvals$P0 = iterationvals$P0eng * 2
  iteration0 <- suppressWarnings(MainIterationFunction(iterationvals, specifications, out = "Iteration"))
  
  # Power
  while (iteration0[6,2] <= iteration0[6,3] |
         iteration0[7,2] <= iteration0[7,3] |
         iteration0[8,2] <= iteration0[8,3]) {
    iterationvals$P0eng = iterationvals$P0eng + 5000
    iterationvals$b = sqrt(iterationvals$AR * iterationvals$S)
    iterationvals$K = 1/(pi * iterationvals$AR * iterationvals$e)
    iterationvals$W = iterationvals$m * 9.8065
    iterationvals$WS = iterationvals$W/iterationvals$S
    iterationvals$P0 = iterationvals$P0eng * 2
    iteration0 <- suppressWarnings(MainIterationFunction(iterationvals, specifications, out = "Iteration"))
  }
  
  # Takeoff
  while (iteration0[4,2] >= iteration0[4,3] |
         iteration0[5,2] >= iteration0[5,3]) {
    iterationvals$Clflaps = iterationvals$Clflaps + 0.025
    iteration0 <- suppressWarnings(MainIterationFunction(iterationvals, specifications, out = "Iteration"))
  }
  
  # Landing
  while (iteration0[9,2] >= iteration0[9,3]) {
    iterationvals$Clhls = iterationvals$Clhls + 0.025
    iteration0 <- suppressWarnings(MainIterationFunction(iterationvals, specifications, out = "Iteration"))
  }
  
  # Weight Analysis
  while (iteration0[12,2] <= iteration0[12,3]) {
    iterationvals$AR = iterationvals$AR + 0.5
    iterationvals$b = sqrt(iterationvals$AR * iterationvals$S)
    iterationvals$K = 1/(pi * iterationvals$AR * iterationvals$e)
    iterationvals$W = iterationvals$m * 9.8065
    iterationvals$WS = iterationvals$W/iterationvals$S
    iterationvals$P0 = iterationvals$P0eng * 2
    iteration0 <- suppressWarnings(MainIterationFunction(iterationvals, specifications, out = "Iteration"))
  }
  
  plot[[i]] = iteration0
  
}

melt(plot,
     id.vars = c("Description", "Specification",
                  "m", "S",
                  "W", "WS", "AR", "Clflaps", "Clhls", "P0eng",
                  "Minimise", "Under", "Over"),
     variable.name = "key",
     value.name = "value")


# iterationout <- lapply(iterationvals, function(x) suppressWarnings(MainIterationFunction(x, specifications, out = "Iteration")))
# 
# melt(iterationout, 
#      id.vars = c("Description", "Specification", 
#                   "m", "S", 
#                   "W", "WS", "AR", "Clflaps", "Clhls", "P0eng",
#                   "Minimise", "Under", "Over"),
#      variable.name = "key",
#      value.name = "value")






