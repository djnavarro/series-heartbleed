library(Rcpp)

# define the turmite() function
sourceCpp(here::here("turmite_a.cpp"))

filename <- here::here("turmite_11.png")
grain <- 1000
pixels <- 5000
nsteps <- 200000
ncols <- 1000
ss <- 31

col2hex <- function(name) {
  col_matrix <- grDevices::col2rgb(name)
  col_string <- grDevices::rgb(
    red   = col_matrix[1, ]/255, 
    green = col_matrix[2, ]/255, 
    blue  = col_matrix[3, ]/255
  )
  return(col_string)
}

grid <- turmite(grain, nsteps, ss)

palette <- c(
  col2hex("black"), 
  paletteer::paletteer_c("scico::grayC", ncols)
)

inds <- 1 + ceiling(ncols * grid/nsteps)
rast <- as.raster(matrix(palette[inds], grain, grain))
  
png(filename, pixels, pixels)
op <- par(mar=c(0,0,0,0))
plot(rast)
dev.off()
par(op)


