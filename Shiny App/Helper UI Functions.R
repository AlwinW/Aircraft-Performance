#----------------------------
#--- Functions for UI
#============================
# These are genreal functions that do NOT require observe, require, etc

# Return hover, click data on graphs in a meaningful way
# Also think about allowing interactive zooming
xy_str <- function(e) {
  if(is.null(e)) return("NULL\n")
  paste0("x=", round(e$x, 4), " y=", round(e$y, 4), "\n")
}

xy_range_str <- function(e) {
  if(is.null(e)) return("NULL\n")
  paste0("xmin=", round(e$xmin, 4), " xmax=", round(e$xmax, 4), 
         " ymin=", round(e$ymin, 4), " ymax=", round(e$ymax, 4))
}

### the follow isn't reactive...
inputvalsupdate <- function() {
  data.frame(S = input$S, b = input$b, AR = input$AR, e = input$e, K = input$K,
             Cd0 = input$Cd0, Clzeroa = input$Clzeroa, Clmax = input$Clmax, Clflaps = input$Clflaps, Clhls = input$Clhls,
             m = input$m, W = input$W, WS = input$WS,
             P0eng = input$P0eng, P0 = input$P0
  )
}