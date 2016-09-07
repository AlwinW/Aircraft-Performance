ggplot(filter(IterationOutLong, name == "Iteration")) +
  geom_point(aes(x = m, y = S, colour = WS))

ggplot(filter(IterationOutLong, name == "Iteration")) +
  geom_point(aes(x = m, y = S, colour = SrefSwet))

library(GGally)

ggparcoord(mtcars,columns = c(1,5:10)) + geom_line()


ggparcoord(filter(IterationOutLong, name == "Iteration") %>% mutate(ID = as.factor(ID)),
           columns = c(5, 3, 6, 7, 9, 10, 11, 12, 13, 14, 15),
           groupColumn = "ID") +
  geom_line()

library(MASS)
parcoordlabel<-function (x, col = 1, lty = 1,  lblcol="grey",...) 
{
  df <- as.data.frame(x)
  pr <- lapply(df, pretty)
  rx <- lapply(pr, range, na.rm = TRUE)
  x <- mapply(function(x,r) {
    (x-r[1])/(r[2]-r[1])
  },
  df, rx)
  matplot(1L:ncol(x), t(x), type = "l", col = col, lty = lty, 
          xlab = "", ylab = "", axes = FALSE, ...)
  axis(1, at = 1L:ncol(x), labels = colnames(x))
  for (i in 1L:ncol(x)) {
    lines(c(i, i), c(0, 1), col = "grey70")
    text(c(i, i), seq(0,1,length.out=length(pr[[i]])), labels = pr[[i]], 
         xpd = NA, col=lblcol)
  }
  invisible()
}
cardeaths &lt;- data.frame(Seatbelts[,1], Seatbelts[,5], Seatbelts[,6], 
                           Seatbelts[,8])
colnames(cardeaths) &lt;- c("DriversKilled", "DistanceDriven", "PriceofGas", 
                            "SeatbeltLaw")
parcoord(state.x77[, c(7, 4, 6, 2, 5, 3)], var.label=TRUE)


testplot <- filter(IterationOutLong, name == "Iteration", AR < 30, Clhls < 1, `Empty Weight` > 0.35) %>% 
  select(m, S, WS, AR, `Cruise near Vstar`, `Cruise near Vstar`, P0eng, Clflaps, Clhls, `Empty Weight`) %>%
  mutate(AR = -AR, P0eng = -P0eng, WS = -WS)
parcoordlabel(testplot, col=rainbow(length(testplot[,1])))


testplot <- filter(IterationOutLong, name == "Iteration", AR < 30, Clhls < 1, `Empty Weight` > 0.35, Cd0 > 0.015) %>% 
  mutate(b = sqrt(S/AR))%>% 
  select(m, S, WS, Clhls, AR, b, `Cruise near Vstar`, `Cruise near Vstar`, P0eng, e, SrefSwet, `Empty Weight`) %>%
  mutate(AR = -AR, P0eng = -P0eng, WS = -WS, Clhls = -Clhls, SrefSwet = -SrefSwet)
parcoordlabel(testplot, col=rainbow(length(testplot[,1])))

#--- Cd0 = 0.02
testplot <- filter(IterationOutLong, name == "Iteration", AR < 30, Cd0 > 0.019) %>% 
  select(Cd0,m, S, WS, Clhls, AR, `Cruise near Vstar`, `Cruise near Vstar`, P0eng, e, SrefSwet, `Empty Weight`) %>%
  mutate(AR = -AR, P0eng = -P0eng, WS = -WS, Clhls = -Clhls, SrefSwet = -SrefSwet)
parcoordlabel(testplot, col=rainbow(length(testplot[,1])))

#--- Cd0 = 0.0175
testplot <- filter(IterationOutLong, name == "Iteration", AR < 30, Clhls < 0.6, `Empty Weight` > 0.35, Cd0 > 0.015) %>% 
  select(m, S, WS, Clhls, AR, `Cruise near Vstar`, `Cruise near Vstar`, P0eng, e, SrefSwet) %>%
  mutate(AR = -AR, P0eng = -P0eng, WS = -WS, Clhls = -Clhls, SrefSwet = -SrefSwet)
parcoordlabel(testplot, col=rainbow(length(testplot[,1])))


#--- FULL  3 DAY 1296 ITERATION
testplot <- filter(IterationOutLong1296, name == "Iteration", AR < 30, Clhls < 1, `Empty Weight` > 0.35) %>% 
  mutate(b = sqrt(S/AR), Cd0LightTwinMargin = SrefSwet * 0.0045 - Cd0)%>% 
  dplyr::select(m, S, WS, Clhls, AR, b, `Cruise near Vstar`, `Cruise near Vstar`, P0eng, e, SrefSwet, `Empty Weight`, Cd0LightTwinMargin, Cd0) %>%
  mutate(AR = -AR, P0eng = -P0eng, WS = -WS, Clhls = -Clhls, SrefSwet = -SrefSwet)
parcoordlabel(testplot, col=rainbow(length(testplot[,1])))

filterplot1296 <- filter(IterationOutLong1296, name == "Iteration") %>%
  mutate(b = sqrt(S/AR), Cd0LightTwinMargin  = Cd0 - SrefSwet * 0.0045) %>%
  filter(AR <= 30, Clhls <= 1, `Empty Weight` > 0.35, Cd0LightTwinMargin > -0.0002) %>%
  mutate(AR = -AR, P0eng = -P0eng, WS = -WS, Clhls = -Clhls, SrefSwet = -SrefSwet) %>%
  dplyr::select(m, S, AR, b, `Cruise near Vstar`, `Cruise near Vstar`, P0eng, e, SrefSwet, `Empty Weight`, Cd0LightTwinMargin, Cd0) 

parcoordlabel(filterplot1296, col=rainbow(nrow(filterplot1296)) )

filterplot1296 <- filter(IterationOutLong1296, name == "Iteration") %>%
  mutate(b = sqrt(S/AR), Cd0LightTwinMargin  = Cd0 - SrefSwet * 0.0045) %>%
  filter(`Cruise near Vstar` > 70, AR <= 30, Clhls <= 1, `Empty Weight` > 0.35) %>%
  mutate(AR = -AR, P0eng = -P0eng, WS = -WS, Clhls = -Clhls, SrefSwet = -SrefSwet) %>%
  dplyr::select(m, WS, AR, b, `Cruise near Vstar`, `Cruise near Vstar`, P0eng, e, SrefSwet, `Empty Weight`, Cd0LightTwinMargin, Cd0) 

parcoordlabel(filterplot1296, col=rainbow(nrow(filterplot1296)) )


filterplotfocus<- filter(IterationOutLongFocus, name == "Iteration") %>%
  mutate(b = sqrt(S/AR), Cd0LightTwinMargin  = Cd0 - SrefSwet * 0.0030) %>%
  filter(AR <= 30, Clhls <= 1, `Empty Weight` > 0.35, e < 0.95) %>%
  mutate(AR = -AR, P0eng = -P0eng, WS = -WS, Clhls = -Clhls, SrefSwet = -SrefSwet) %>%
  dplyr::select(m, S, AR, b, `Cruise near Vstar`, `Cruise near Vstar`, P0eng, Clhls, e, SrefSwet, `Empty Weight`, Cd0LightTwinMargin, Cd0) 

parcoordlabel(filterplotfocus, col=rainbow(nrow(filterplotfocus)) )


filterplotfocus<- filter(IterationOutLongFocus, name == "Iteration") %>%
  mutate(b = sqrt(S/AR), Cd0LightTwinMargin  = Cd0 - SrefSwet * 0.0045) %>%
  filter(AR <= 25) %>%
  mutate(AR = -AR, P0eng = -P0eng, WS = -WS, Clhls = -Clhls, SrefSwet = -SrefSwet) %>%
  dplyr::select(m, S, AR, b, `Cruise near Vstar`, `Cruise near Vstar`, P0eng, Clhls, e, SrefSwet, `Empty Weight`, Cd0LightTwinMargin, Cd0) 

parcoordlabel(filterplotfocus, col=rainbow(nrow(filterplotfocus)) )



