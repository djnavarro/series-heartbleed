library(Rcpp)
library(tidyverse)
library(ambient)
library(flametree)
library(ggforce)
library(voronoise)
library(jasmines)

output <- here::here("turmite_56.png")

seed <- 56
ncols <- 1000

fill_base <- "black"
palette_name <- "ggthemes::Gold-Purple Diverging"

palette_base <- paletteer::paletteer_c(palette_name, ncols + 1)
palette_heart <- paletteer::paletteer_c(palette_name, ncols + 1)

heart_size <- .5
n_slices <- 10

shift <- c(.3, .4)



# start -------------------------------------------------------------------



set.seed(seed)

grains_high <- 2500
grains_wide <- 2500

pixels_high <- grains_high * 2
pixels_wide <- grains_wide * 2



# turmite background ------------------------------------------------------

cat("turmite wandering...\n")

# define the turmite() function
sourceCpp(here::here("turmite_e.cpp"))

nsteps <- 10000000
ss <- 3

# create long grid for the raster with appropriate aspect ratio
ar <- grains_high / grains_wide
raster <- long_grid(
  x = seq(0, 1,  length.out = grains_wide), 
  y = seq(0, ar, length.out = grains_high)
)

# # run the turmite over the grid
# adj <- 1
# f <- jasmines::palette_adjust(
#   name = "rainbow",
#   prefix = "#222222FF",
#   red.f = adj,
#   blue.f = adj,
#   green.f = adj
# )
# palette <- f(ncols + 1)
#palette <- paletteer::paletteer_c("grDevices::PuOr", ncols + 1)


grid <- t(turmite(grains_wide, grains_high, nsteps, ss))
inds <- 1 + ceiling(ncols * grid/nsteps)
raster$shade <- palette_base[inds]



# generate dust heart ------------------------------------------------------

cat("dust heart beating...\n")

dat <- use_seed(seed) %>%
  entity_heart(grain = 1000, size = heart_size) %>%
  mutate(ind = 1:n()) %>%
  unfold_slice(iterations = n_slices, scale = .5 * 10^-24, scatter = TRUE, output1 = "id") %>%
  mutate(
    x = x + rnorm(n())/500,
    y = y + rnorm(n())/500
  ) %>%
  unfold_breeze(
    iterations = 100,
    scale = .0002,
    drift = .0005,
    fractal = ambient::ridged,
    octaves = 8
  ) %>%
  unfold_inside() %>%
  mutate(val = 1 + (inside>0)*ind)


dat$val <- normalise(dat$val, to = c(1, ncols+1))
dat$val <- round(dat$val)


# adj <- .9
# f <- jasmines::palette_adjust(
#   name = "oslo",
#   prefix = "#222222FF",
#   red.f = adj,
#   blue.f = adj,
#   green.f = adj
# )

#f <- scico::scico
#palette_heart <- f(ncols+1) 

dat$shade <- palette_heart[dat$val]

dat$x <- (dat$x + shift[1])
dat$y <- (dat$y + shift[2])


# render the image --------------------------------------------------------

cat("image rendering...\n")

bg <- fill_base

pic <- ggplot(
  data = raster,
  mapping = aes(x, y, fill = shade)
) + 
  
  # the raster object forms the background
  geom_raster(alpha = .5) + 
  
  # the heart is made of dust/points
  geom_point(
    data = dat, 
    mapping = aes(x,y, color = shade, alpha = exp(-(time-1)/20)), 
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



