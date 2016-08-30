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
var_m = seq(3500, 7500, 500)
var_e = seq(0.8, 0.95, 0.05)

# Apply the variations
iterationvals <- input_initial[rep(rownames(input_initial), each = length(var_e) * length(var_m)), 1:length(input_initial)]
iterationvals$m <- rep(var_m, each = length(var_e))
iterationvals$e <- rep(var_e, times = length(var_m))
iterationvals <- iterationvals %>%
  rowwise() %>%
  do(., data.frame(., tempWS = seq(1600, 3200, 200))) %>%
  ungroup() %>%
  mutate(S = m*9.8065 / tempWS) %>%
  select(-tempWS)
  
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
iterationvals <- data.frame(UpdateParams(iterationvals))

iteration <- list()
pb <- txtProgressBar(min=0, max = nrow(iterationvals), style = 3)
for (i in 1:nrow(iterationvals)) {
  iterationvals0 <- UpdateParams(iterationvals[i,])
  iteration[[i]] <- MainIterationFunction(iterationvals0, specifications, out = "Iteration")
  setTxtProgressBar(pb, i)
}

iterationlong <-  melt(iteration,
                       id.vars = c("Description", "Specification",
                                   "Minimise", "Under", "Over"),
                       variable.name = "key",
                       value.name = "Iteration") %>%
  select(-key, -Minimise, -Under, -Over) 

asdf <- iterationlong %>%
  unite(Description, Description, L1, sep = "._")%>%
  gather(name, value, -Description) %>%
  separate(Description, c("Description", "ID"), sep="\\._") %>%
  mutate(ID = as.integer(ID)) %>%
  spread(Description, value) %>%
  arrange(ID, name)

ggplot(filter(asdf, name == "Iteration")) +
  geom_point(aes(x = m, y = WS, colour = `Empty Weight`))
ggplot(filter(asdf, name == "Iteration")) +
  geom_point(aes(x = Cd0, y = m, colour = `Empty Weight`))
ggplot(data = filter(asdf, name == "Iteration")) +
  geom_point(aes(x = e, y = m, colour = `Empty Weight`)) 

## Power per engine ####
# Test for convergence / root finding
var_P0eng = seq(10000, 250000, 1000)
iterationvals <- input_initial[rep(rownames(input_initial), each =length(var_P0eng)), 1:length(input_initial)]
iterationvals$P0eng <- rep(var_P0eng)
iterationvals <- data.frame(UpdateParams(iterationvals))
iterationP0eng <- list()
pb <- txtProgressBar(min=0, max = nrow(iterationvals), style = 3)
for (i in 1:length(var_P0eng)) {
  iterationvals0 <- UpdateParams(iterationvals[i,])
  iterationP0eng[[i]] <- MainIterationFunction(iterationvals0, specifications, out = "Iteration")
  setTxtProgressBar(pb, i)
}

iterationlongP0eng <-  melt(iterationP0eng,
                       id.vars = c("Description", "Specification",
                                   "Minimise", "Under", "Over"),
                       variable.name = "key",
                       value.name = "Iteration") %>%
  select(-key, -Minimise, -Under, -Over) %>%
  unite(Description, Description, L1, sep = "._")%>%
  gather(name, value, -Description) %>%
  separate(Description, c("Description", "ID"), sep="\\._") %>%
  mutate(ID = as.integer(ID)) %>%
  spread(Description, value) %>%
  arrange(ID, name)
# 2nd Segment Percentage Gradient
ggplot(filter(iterationlongP0eng, name == "Iteration")) +
  geom_point(aes(x = P0eng, y = `2nd Segment PerGrad` - 1.5, colour = `Empty Weight`))
# Climb rate at Ceiling
ggplot(filter(iterationlongP0eng, name == "Iteration")) +
  geom_point(aes(x = P0eng, y = `Ceiling Climb Rate` - 0.508000, colour = `Empty Weight`))
# Takeoff
ggplot(filter(iterationlongP0eng, name == "Iteration")) +
  geom_point(aes(x = P0eng, y = `Normal Takeoff`, colour = `Empty Weight`)) +
  geom_point(aes(x = P0eng, y = `Estimated BFL`, colour = `Empty Weight`)) +
  geom_line(aes(x = P0eng, y = `Normal Takeoff`*1.1), colour = "red") +
  ylim(-000, 2000)
ggplot(filter(iterationlongP0eng, name == "Iteration")) +
  geom_point(aes(x = P0eng, y = 1200 - pmax(`Normal Takeoff`, `Estimated BFL`), colour = `Empty Weight`)) +
  ylim(-2000, 2000)
# Landing
ggplot(filter(iterationlongP0eng, name == "Iteration")) +
  geom_point(aes(x = P0eng, y = 1200 - `Landing Dist`, colour = `Empty Weight`))
# Approach Speed
ggplot(filter(iterationlongP0eng, name == "Iteration")) +
  geom_point(aes(x = P0eng, y = 51.44 - `Approach Speed`, colour = `Empty Weight`))
# Empty Weight
ggplot(filter(iterationlongP0eng, name == "Iteration")) +
  geom_point(aes(x = P0eng, y = `Empty Weight`, colour = `Empty Weight`)) + 
  ylim(0.2,0.7)

## AR ####
# Test for convergence / root finding
var_AR = seq(15, 30, 0.1)
iterationvals <- input_initial[rep(rownames(input_initial), each =length(var_AR)), 1:length(input_initial)]
iterationvals$AR <- rep(var_AR)
iterationvals <- data.frame(UpdateParams(iterationvals))
iterationAR <- list()
pb <- txtProgressBar(min=0, max = nrow(iterationvals), style = 3)
for (i in 1:length(var_AR)) {
  iterationvals0 <- UpdateParams(iterationvals[i,])
  iterationAR[[i]] <- MainIterationFunction(iterationvals0, specifications, out = "Iteration")
  setTxtProgressBar(pb, i)
}

iterationlongAR <-  melt(iterationAR,
                            id.vars = c("Description", "Specification",
                                        "Minimise", "Under", "Over"),
                            variable.name = "key",
                            value.name = "Iteration") %>%
  select(-key, -Minimise, -Under, -Over) %>%
  unite(Description, Description, L1, sep = "._")%>%
  gather(name, value, -Description) %>%
  separate(Description, c("Description", "ID"), sep="\\._") %>%
  mutate(ID = as.integer(ID)) %>%
  spread(Description, value) %>%
  arrange(ID, name)
# 2nd Segment Percentage Gradient
ggplot(filter(iterationlongAR, name == "Iteration")) +
  geom_point(aes(x = AR, y = `2nd Segment PerGrad` - 1.5, colour = `Empty Weight`))
# Climb rate at Ceiling
ggplot(filter(iterationlongAR, name == "Iteration")) +
  geom_point(aes(x = AR, y = `Ceiling Climb Rate` - 0.508000, colour = `Empty Weight`))
# Takeoff
ggplot(filter(iterationlongAR, name == "Iteration")) +
  geom_point(aes(x = AR, y = `Normal Takeoff`, colour = `Empty Weight`)) +
  geom_point(aes(x = AR, y = `Estimated BFL`, colour = `Empty Weight`))
ggplot(filter(iterationlongAR, name == "Iteration")) +
  geom_point(aes(x = AR, y = 1200 - pmax(`Normal Takeoff`, `Estimated BFL`), colour = `Empty Weight`))
# Landing
ggplot(filter(iterationlongAR, name == "Iteration")) +
  geom_point(aes(x = AR, y = 1200 - `Landing Dist`, colour = `Empty Weight`))
# Approach Speed
ggplot(filter(iterationlongAR, name == "Iteration")) +
  geom_point(aes(x = AR, y = 51.44 - `Approach Speed`, colour = `Empty Weight`))
# Empty Weight
ggplot(filter(iterationlongAR, name == "Iteration")) +
  geom_point(aes(x = AR, y = `Empty Weight`, colour = `Empty Weight`))


## Clflaps ####
# Test for convergence / root finding
var_Clflaps = seq(0.2, 1.5, 0.01)
iterationvals <- input_initial[rep(rownames(input_initial), each =length(var_Clflaps)), 1:length(input_initial)]
iterationvals$Clflaps <- rep(var_Clflaps)
iterationvals <- data.frame(UpdateParams(iterationvals))
iterationClflaps <- list()
pb <- txtProgressBar(min=0, max = nrow(iterationvals), style = 3)
for (i in 1:length(var_Clflaps)) {
  iterationvals0 <- UpdateParams(iterationvals[i,])
  iterationClflaps[[i]] <- MainIterationFunction(iterationvals0, specifications, out = "Iteration")
  setTxtProgressBar(pb, i)
}

iterationlongClflaps <-  melt(iterationClflaps,
                         id.vars = c("Description", "Specification",
                                     "Minimise", "Under", "Over"),
                         variable.name = "key",
                         value.name = "Iteration") %>%
  select(-key, -Minimise, -Under, -Over) %>%
  unite(Description, Description, L1, sep = "._")%>%
  gather(name, value, -Description) %>%
  separate(Description, c("Description", "ID"), sep="\\._") %>%
  mutate(ID = as.integer(ID)) %>%
  spread(Description, value) %>%
  arrange(ID, name)
# 2nd Segment Percentage Gradient
ggplot(filter(iterationlongClflaps, name == "Iteration")) +
  geom_point(aes(x = Clflaps, y = `2nd Segment PerGrad` - 1.5, colour = `Empty Weight`))
# Climb rate at Ceiling
ggplot(filter(iterationlongClflaps, name == "Iteration")) +
  geom_point(aes(x = Clflaps, y = `Ceiling Climb Rate` - 0.508000, colour = `Empty Weight`))
# Takeoff
ggplot(filter(iterationlongClflaps, name == "Iteration")) +
  geom_point(aes(x = Clflaps, y = `Normal Takeoff`, colour = `Empty Weight`)) +
  geom_point(aes(x = Clflaps, y = `Estimated BFL`, colour = `Empty Weight`))
ggplot(filter(iterationlongClflaps, name == "Iteration")) +
  geom_point(aes(x = Clflaps, y = 1200 - pmax(`Normal Takeoff`, `Estimated BFL`), colour = `Empty Weight`))
# Landing
ggplot(filter(iterationlongClflaps, name == "Iteration")) +
  geom_point(aes(x = Clflaps, y = 1200 - `Landing Dist`, colour = `Empty Weight`))
# Approach Speed
ggplot(filter(iterationlongClflaps, name == "Iteration")) +
  geom_point(aes(x = Clflaps, y = 51.44 - `Approach Speed`, colour = `Empty Weight`))
# Empty Weight
ggplot(filter(iterationlongClflaps, name == "Iteration")) +
  geom_point(aes(x = Clflaps, y = `Empty Weight`, colour = `Empty Weight`))


## Clhls ####
# Test for convergence / root finding
var_Clhls = seq(0.2, 1.5, 0.01)
iterationvals <- input_initial[rep(rownames(input_initial), each =length(var_Clhls)), 1:length(input_initial)]
iterationvals$Clhls <- rep(var_Clhls)
iterationvals <- data.frame(UpdateParams(iterationvals))
iterationClhls <- list()
pb <- txtProgressBar(min=0, max = nrow(iterationvals), style = 3)
for (i in 1:length(var_Clhls)) {
  iterationvals0 <- UpdateParams(iterationvals[i,])
  iterationClhls[[i]] <- MainIterationFunction(iterationvals0, specifications, out = "Iteration")
  setTxtProgressBar(pb, i)
}

iterationlongClhls <-  melt(iterationClhls,
                              id.vars = c("Description", "Specification",
                                          "Minimise", "Under", "Over"),
                              variable.name = "key",
                              value.name = "Iteration") %>%
  select(-key, -Minimise, -Under, -Over) %>%
  unite(Description, Description, L1, sep = "._")%>%
  gather(name, value, -Description) %>%
  separate(Description, c("Description", "ID"), sep="\\._") %>%
  mutate(ID = as.integer(ID)) %>%
  spread(Description, value) %>%
  arrange(ID, name)
# 2nd Segment Percentage Gradient
ggplot(filter(iterationlongClhls, name == "Iteration")) +
  geom_point(aes(x = Clhls, y = `2nd Segment PerGrad` - 1.5, colour = `Empty Weight`))
# Climb rate at Ceiling
ggplot(filter(iterationlongClhls, name == "Iteration")) +
  geom_point(aes(x = Clhls, y = `Ceiling Climb Rate` - 0.508000, colour = `Empty Weight`))
# Takeoff
ggplot(filter(iterationlongClhls, name == "Iteration")) +
  geom_point(aes(x = Clhls, y = `Normal Takeoff`, colour = `Empty Weight`)) +
  geom_point(aes(x = Clhls, y = `Estimated BFL`, colour = `Empty Weight`))
ggplot(filter(iterationlongClhls, name == "Iteration")) +
  geom_point(aes(x = Clhls, y = 1200 - pmax(`Normal Takeoff`, `Estimated BFL`), colour = `Empty Weight`))
# Landing
ggplot(filter(iterationlongClhls, name == "Iteration")) +
  geom_point(aes(x = Clhls, y = 1200 - `Landing Dist`, colour = `Empty Weight`))
# Approach Speed
ggplot(filter(iterationlongClhls, name == "Iteration")) +
  geom_point(aes(x = Clhls, y = 51.44 - `Approach Speed`, colour = `Empty Weight`))
# Empty Weight
ggplot(filter(iterationlongClhls, name == "Iteration")) +
  geom_point(aes(x = Clhls, y = `Empty Weight`, colour = `Empty Weight`))
