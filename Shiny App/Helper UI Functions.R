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