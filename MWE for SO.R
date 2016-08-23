# Randomly generated dataframe
df <-  data.frame(a = rnorm(10, 0, 1), x = rnorm(10, 1, 3), y = rnorm(10, 2, 3))

# Function that takes multiple arguments and returns multiple values in a list
zsummary <- function(x, y) { 
  if (y < 0) return(list(NA, NA))
  z = rnorm(10, x, abs(y))
  return(list(mean(z), sd(z)))
}

# Example of something that works using dplyr
# However, this results in a lot of function calls...
library(dplyr)
df %>% rowwise() %>%
  mutate(mean = zsummary(x,y)[[1]], sd = zsummary(x,y)[[1]])

# Answer 1 using lists
zsummary1 <- function(x, y) { 
  if (y < 0) return(list(NA, NA))
  z = rnorm(10, x, abs(y))
  return(list(mean(z), sd(z)))
}

cbind(df,t(mapply(zsummary1, df$x, df$y)))

# Answer 2 using dataframe
zsummary2 <- function(x, y) { 
  if (y < 0) return(data.frame(mean = NA, sd = NA))
  z = rnorm(10, x, abs(y))
  return(data.frame(mean = mean(z), sd = sd(z)))
}
df %>% rowwise() %>%
  do(data.frame(., zsummary2(.$x, .$y)))

# Answer 3 using a function of a single input
zsummary3 <- function(input) { 
  if (input[2] < 0) return(c(mean = NA, sd = NA))
  z = rnorm(10, input[1], abs(input[2]))
  return(c(mean = mean(z), sd = sd(z)))
}

cbind(df, t(apply(df[-1], 1, zsummary3)))

# Answer 4 using a function of a single input (Ans 3 adapted)
zsummary4 <- function(input) { 
  if (input["y"] < 0) return(c(mean = NA, sd = NA))
  z = rnorm(10, input["x"], abs(input["y"]))
  return(c(mean = mean(z), sd = sd(z)))
}
# df[c("x","y")]
cbind(df, t(apply(df, 1, zsummary4)))
