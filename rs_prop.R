library(raster)

rs_prop_2 <- function(vals, na.rm) {
  mode <- modal(vals)
  sum(vals==mode)/length(vals)
}
