#----------------------------
#--- Iteration Attempt
#============================

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
  Etatotal = 0.8,
  alt_s = 1,
  ClG = 0.25,
  Cd0G = 0.035,
  hground = 2.5
)

inputvals <- input_initial

TruelyBadassIterationSummary <- function(thatsrightonlyoneinput)
  suppressWarnings(MainIterationFunction(thatsrightonlyoneinput, specifications))

# Initial Data Frame
iterationvals <- input_initial[rep(row.names(input_initial), each = 2), 1:length(input_initial)]
# Set the changing variable
iterationvals$Clhls <- c(1.2, 1.8)
# Convert dataframe to a list
iterationvals <- split(iterationvals, seq(nrow(iterationvals)))
# Give id names (these are preserved)
names(iterationvals) <- c("x", "y")

iterationout <- lapply(iterationvals, function(x) TruelyBadassIterationSummary(x))
iterationout <- do.call("rbind", iterationout)
iterationout$id <- c(rep("x", 25), rep("y", 25))

iterationout <-  iterationout %>%
  gather(key, value, -id, - Description) %>% 
  unite(temp, Description, key) %>%
  spread(temp, value)






test1 <- input_initial[rep(row.names(input_initial), each = 2), 1:length(input_initial)]
test1$Clhls <- c(1.2, 1.8)


for (i in 1:nrow(test1)) {
  summary[[i]] = TruelyBadassIterationSummary(test1[i,])
}

test2 <- split(test1, seq(nrow(test1)))
test2 <- list(test1[1,], test1[2,])

out2 <- lapply(test2, function(x) TruelyBadassIterationSummary(x))

names(out2) <- c("x", "y")

out3 <- do.call("rbind", out2)
rownames(out3) <- NULL
out3$id <- c(rep("x", 25), rep("y", 25))
gather(out3, value, Description)

THEOUTPUTIWANT <- gather(out3, key, value, -id, - Description) %>% unite(temp, Description, key) %>%
  spread(temp, value)

out3 %>% spread(Description, Value)

out3 <- do.call("cbind", out2)
out2$id <- rownames(out2)







