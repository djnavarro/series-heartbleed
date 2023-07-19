library(Rcpp)
library(tidyverse)
library(ambient)
library(flametree)
library(ggforce)
library(voronoise)
library(jasmines)

output <- here::here("turmite_20.png")

grains_high <- 1000
grains_wide <- 1000

pixels_high <- grains_high * 5
pixels_wide <- grains_wide * 5



# turmite background ------------------------------------------------------

cat("turmite wandering...\n")

# define the turmite() function
sourceCpp(here::here("turmite_d.cpp"))

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
f <- jasmines::palette_adjust(
  name = "rainbow",
  prefix = "#222222FF",
  red.f = .7,
  blue.f = .7,
  green.f = .7
)

palette <- f(ncols + 1)
grid <- t(turmite(grains_wide, grains_high, nsteps, ss))
inds <- 1 + ceiling(ncols * grid/nsteps)
raster$shade <- palette[inds]



# generate dust heart ------------------------------------------------------

cat("dust heart beating...\n")

dat <- use_seed(3) %>%
  entity_heart(grain = 1000, size = .2) %>%
  unfold_slice(iterations = 5, scale = 10^-24, scatter = TRUE, output1 = "id") %>%
  mutate(
    x = x + rnorm(n())/500,
    y = y + rnorm(n())/500
  ) %>%
  unfold_breeze(
    iterations = 200,
    scale = .0003,
    drift = .001,
    fractal = ambient::billow,
    octaves = 16
  ) %>%
  unfold_inside() %>%
  mutate(val = 1 + (inside>0)*ind)


dat$val <- normalise(dat$val, to = c(1, ncols+1))
dat$val <- round(dat$val)


# f <- jasmines::palette_adjust(
#   name = "rainbow",
#   prefix = "#222222FF",
#   red.f = .7,
#   blue.f = .7,
#   green.f = .7
# )

f <- scico::scico
palette_heart <- f(ncols + 1, palette = "oslo")

dat$shade <- palette_heart[dat$val]

dat$x <- (dat$x + .5)
dat$y <- (dat$y + .45)

# 
# # compute aspect ratio of generated tree
# ar2 <- with(dat, (max(y) - min(y))/(max(x) - min(x)))
# 
# # scale the heart image to fit the postcard
# dat <- dat %>%
#   mutate(
#     x = normalise(x, to = range(raster$x)),
#     y = normalise(y, to = range(raster$y))
#   ) %>%
#   mutate(
#     x = x * min(1, ar/ar2),
#     y = y * min(1, ar2/ar)
#   ) %>%
#   mutate(
#     x = .2 + x * .7,
#     y = y * .8
#   )
# 


# render the image --------------------------------------------------------

cat("image rendering...\n")

pic <- ggplot(
  data = raster,
  mapping = aes(x, y, fill = shade)
) + 
  
  # the raster object forms the background
  geom_raster(alpha = .5) + 
  
  # the heart is made of dust/points
  geom_point(
    data = dat, 
    mapping = aes(x,y, colour = shade, alpha = exp(-(time-1)/20)), 
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
  NULL

# export image
ggsave(
  filename = output,
  plot = pic,
  width = pixels_wide / 300,
  height = pixels_high / 300,
  dpi = 300
)



