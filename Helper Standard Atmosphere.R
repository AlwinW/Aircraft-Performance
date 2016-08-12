#----------------------------
#--- Standard Atmosphere
#============================

standatomconst <- data.frame(
  T0 = 288.15,
  a0 = 340.3,
  rho0 = 1.225,
  p0 = 101325,
  hc = 11000,
  dTdh = 0.0065,
  Tc = 216.55,
  deltac = 0.22277,
  goRTc = 0.00015779,
  thetac = 0.75149,
  g0 = 9.8065,
  rearth = 6371000
)

StandardAtomsphere <- function(data) {
  # Give the data an ID so we can sort it later
  data <- data %>%
    cbind(., standatomconst) %>%
    rowwise() %>%
    mutate(
      T = if(h <= hc) {T0 - dTdh * h} else {Tc},
      theta = if(h <= hc) {T / T0} else {T / T0},
      delta =  if(h <= hc) {theta ^ 5.256} else {deltac * exp(-goRTc * (h - hc))},
      sigma = if(h <= hc) {theta ^ 4.256} else {delta / thetac},
      p = p0 * delta,
      rho = rho0 * sigma,
      a = a0 * sqrt(theta),
      g = (rearth/(rearth+h))^2 * g0) %>%
    select(-hc, -dTdh, -Tc, -deltac, -goRTc, -thetac, -rearth, - theta, -delta)
  return(as.data.frame(data))
}
# #---Test
# h <- data.frame(h = seq(0, 20000, by = 1000))
# h$dummy1 <- 1
# h$dummy2 <- 2
# # Remove "-theta, - delta" to work
# h <- StandardAtomsphere(h)
# ggplot(data=h) +
#   geom_path(aes(x=h, y=theta), colour = "blue") +
#   geom_path(aes(x=h, y=delta), colour = "red") +
#   geom_path(aes(x=h, y=sigma), colour = "yellow")
