
my_m <- function(x) {
  x <- x / sum(x)
  rx <- ((x[2] + x[6] - x[8] - x[12])/2) + (sqrt(3)/2 * (x[3] + x[5] - x[9] - x[11])) + (x[4] - x[10])
  ry <- ((x[3] - x[5] - x[9] + x[11])/2) + (sqrt(3)/2 * (x[2] - x[6] - x[8] + x[12])) + (x[1] - x[7])
  m <- sqrt(rx^2 + ry^2)
  return(m)
}