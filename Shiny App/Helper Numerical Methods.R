#----------------------------
#--- Numerical Methods
#============================

#---Secant Root Finding Method

SecantRootUnivariate <- function(func, x1, x2, info = FALSE) {
  # reduce number of function calls by storing results as vars
  fx1 <-  func(x1); fx2 <- func(x2)
  fxr <- 10; loop = 1
  while (abs(fxr) > 0.00001 & loop < 50) {
    xr <- x2 - (fx2*(x1 - x2))/(fx1 - fx2)
    fxr <- func(xr)
    if (fx1 * fxr < 0) {
      x2 <- xr; fx2 <-  fxr
    } else {
      x1 <- xr; fx1 <- fxr
    }
    loop <- loop + 1
  }
  if (info == TRUE) {
    return(c(xr, fxr, loop))
  } else {
    return(xr)
  }
}


#---Climb Rate theta
ClimbRate <- function(V, P, Cd0, q, S, W) {
  n1 = 0.000001
  f1 = V * sqrt(1- n1^2) * W - (P - V * Cd0 * q * S - V * n1^2 * W^2/(q * S)) / W
  n2 = 0.9999999
  f2 = V * sqrt(1 - n2^2) * W - (P - V * Cd0 * q * S - V * n2^2 * W^2/(q * S)) / W
  
  fr = 5
  loop = 1
  
  while (abs(fr) > 0.00001 & loop < 50) {
    nr = n2 - (f2*(n1 - n2))/(f1 - f2)
    fr = V * sqrt(1 - nr^2) * W - (P - V * Cd0 * q * S - V * nr^2 * W^2/(q * S)) / W
    if (f1 * fr < 0) {
      x2 <- xr; fx2 <-  fxr
    } else {
      x1 <- xr; fx1 <- fxr
    }
    loop <- loop + 1
  }
  
  return(nr)
  
}


