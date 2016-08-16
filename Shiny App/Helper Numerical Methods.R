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
