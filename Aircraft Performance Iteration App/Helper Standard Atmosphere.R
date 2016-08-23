#----------------------------
#--- Standard Atmosphere
#============================
# This calculates standard atmosphere values at designated heights

standatomconst <- data.frame(
  T_sl = 288.15,
  a_sl = 340.3,
  rho_sl = 1.225,
  p_sl = 101325,
  hc = 11000,
  dTdh = 0.0065,
  Tc = 216.55,
  deltac = 0.22277,
  goRTc = 0.00015779,
  thetac = 0.75149,
  g_sl = 9.8065,
  rearth = 6371000
)

StandardAtomsphere <- function(data) {
  # Give the data an ID so we can sort it later
  data <- data %>%
    cbind(., standatomconst) %>%
    rowwise() %>%
    mutate(
      T = if(h <= hc) {T_sl - dTdh * h} else {Tc},
      theta = if(h <= hc) {T / T_sl} else {T / T_sl},
      delta =  if(h <= hc) {theta ^ 5.256} else {deltac * exp(-goRTc * (h - hc))},
      sigma = if(h <= hc) {theta ^ 4.256} else {delta / thetac},
      p = p_sl * delta,
      rho = rho_sl * sigma,
      a = a_sl * sqrt(theta),
      g = (rearth/(rearth+h))^2 * g_sl) %>%
    select(-hc, -dTdh, -Tc, -deltac, -goRTc, -thetac, -rearth, - theta, -delta) %>%
    ungroup()
  return(as.data.frame(data))
}