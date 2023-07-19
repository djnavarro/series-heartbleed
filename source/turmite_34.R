library(Rcpp)
library(tidyverse)
library(ambient)
library(flametree)
library(ggforce)
library(voronoise)
library(jasmines)

output <- here::here("turmite_34.png")
set.seed(11111)

grains_high <- 1000
grains_wide <- 1000

pixels_high <- grains_high * 5
pixels_wide <- grains_wide * 5



# turmite background ------------------------------------------------------

cat("turmite wandering...\n")

# define the turmite() function
sourceCpp(here::here("turmite_e.cpp"))

nsteps <- 2000000
ncols <- 1000
ss <- 3

# create long grid for the raster with appropriate aspect ratio
ar <- grains_high / grains_wide
raster <- long_grid(
  x = seq(0, 1,  length.out = grains_wide), 
  y = seq(0, ar, length.out = grains_high)
)

# run the turmite over the grid
adj <- 1.2
f <- jasmines::palette_adjust(
  name = "base",
  prefix = NULL,
  red.f = adj,
  blue.f = adj,
  green.f = adj
)
palette <- f(ncols + 1)


#palette <- paletteer::paletteer_c("grDevices::PuOr", ncols + 1)
grid <- t(turmite(grains_wide, grains_high, nsteps, ss))
inds <- 1 + ceiling(ncols * grid/nsteps)
raster$shade <- palette[inds]



# generate dust heart ------------------------------------------------------

cat("dust heart beating...\n")

dat <- use_seed(11111) %>%
  scene_bubbles(n = 2, grain = 1000) %>%
  mutate(ind = 1:n(), x = x * 3, y = y * 3) %>%
  unfold_slice(iterations = 3, scale = 100 * 10^-24, scatter = TRUE, output1 = "id") %>%
  mutate(ind = 1:n(), x = x * .1, y = y * .1) %>%
  mutate(
    x = x + rnorm(n())/500,
    y = y + rnorm(n())/500
  ) %>%
  unfold_breeze(
    iterations = 200,
    scale = .0002,
    drift = .001,
    fractal = ambient::billow,
    octaves = 16
  ) %>%
  unfold_inside() %>%
  mutate(val = 1 + (inside>0)*ind) %>%
  filter(inside > 0)


dat$val <- normalise(dat$val, to = c(1, ncols+1))
dat$val <- round(dat$val)


adj <- 1
f <- jasmines::palette_adjust(
  name = "grayC",
  prefix = "#00000000",
  red.f = adj,
  blue.f = adj,
  green.f = adj
)

#f <- scico::scico
palette_heart <- f(ncols+1) #palette <- paletteer::paletteer_c("scico::lajolla", ncols + 1)

dat$shade <- palette_heart[dat$val]

dat$x <- (dat$x + .3)
dat$y <- (dat$y + .4)


# render the image --------------------------------------------------------

cat("image rendering...\n")

bg <- "black"

pic <- ggplot(
  data = raster,
  mapping = aes(x, y, fill = shade)
) + 
  
  # the raster object forms the background
  geom_raster(alpha = .5) + 
  
  # the heart is made of dust/points
  geom_point(
    data = dat, 
    mapping = aes(x,y, colour = shade, alpha = exp(-(time-1)/10)), 
    inherit.aes = FALSE,
    show.legend = FALSE,
    size = .5
  ) +
  
  # bunch of settings...
  scale_fill_identity() + 
  scale_colour_identity() + 
  scale_alpha_identity() + 
  coord_equal(xlim = c(0, 1), ylim = c(0,1)) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() + 
  theme(panel.background = element_rect(fill = bg, color = bg)) +
  NULL

# export image
ggsave(
  filename = output,
  plot = pic,
  width = pixels_wide / 300,
  height = pixels_high / 300,
  dpi = 300
)



