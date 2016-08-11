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
  data$ID <- seq.int(nrow(data))
  out <- data %>%
    select(ID, h) %>%
    cbind(., standatomconst) %>%
    mutate(
      T = if(h <= hc) {T0 - dTdh * h} else {},
      theta = if(h <= hc) {T / T0} else {},
      delta =  if(h <= hc) {theta ^ 5.256} else {},
      sigma = if(h <= hc) {theta ^ 4.256} else {},
      p = if(h <= hc) {p0 * delta} else {},
      rho = if(h <= hc) {rho * sigma} else {}
    )

  
  if (standatomconst$h <= standatomconst$hc) {
    standatomconst <-  mutate(
      standatomconst,
      T = T0 - dTdh * h,
      theta = T / T0,
      delta =  theta ^ 5.256,
      sigma = theta ^ 4.256,
      p = p0 * delta,
      rho = rho * sigma
    )
  } else {
    standatomconst <-  mutate(
      standatomconst,
      T = Tc,
      theta = T / T0,
      delta = deltac * exp(-goRTc * (h - hc)),
      sigma = delta / thetac,
      p = p0 * delta
    )
  }
  standatomconst <- standatomconst %>%
    mutate(p = p0 * delta,
           rho = rho0 * sigma,
           a = a0 * sqrt(theta)) %>%
    select(T, p, rho, a, sigma)
}

# #---Test
h <- data.frame(h = seq(0, 20000, by = 1000))
# h %>% rowwise() %>% do(sa = StandardAtomsphere(h))
