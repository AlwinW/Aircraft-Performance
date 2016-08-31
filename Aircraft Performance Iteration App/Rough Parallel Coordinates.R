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


testplot <- filter(IterationOutLong, name == "Iteration", AR < 30) %>% 
  select(m, S, WS, AR, `Cruise near Vstar`, `Cruise near Vstar`, P0eng, Clflaps, Clhls, `Empty Weight`) %>%
  mutate(AR = -AR, P0eng = -P0eng)
parcoordlabel(testplot, col=rainbow(length(testplot[,1])))
