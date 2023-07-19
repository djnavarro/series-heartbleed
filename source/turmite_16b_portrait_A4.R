library(Rcpp)
library(tidyverse)
library(ambient)
library(flametree)
library(ggforce)
library(voronoise)

output <- here::here("turmite_16b_portrait_A4.png")

grains_high <- 1414
grains_wide <- 1000

pixels_high <- grains_high * 5
pixels_wide <- grains_wide * 5



# turmite background ------------------------------------------------------

cat("turmite wandering...\n")

# define the turmite() function
sourceCpp(here::here("turmite_d.cpp"))

nsteps <- 4000000
ncols <- 1000
ss <- 3

# create long grid for the raster with appropriate aspect ratio
ar <- grains_high / grains_wide
raster <- long_grid(
  x = seq(0, 1,  length.out = grains_wide), 
  y = seq(0, ar, length.out = grains_high)
)

# run the turmite over the grid
palette <- scico::scico(ncols + 1, palette = "broc")
grid <- t(turmite(grains_wide, grains_high, nsteps, ss))
inds <- 1 + ceiling(ncols * grid/nsteps)
raster$shade <- palette[inds]



# generate flametree ------------------------------------------------------

cat("flametree growing...\n")


seed_ft <- 4
set.seed(seed_ft)

# the "flametree" itself
ftree <- flametree_grow(
  time = 8,
  seed = seed_ft,
  angle = c(-15, -8, 8, 15, 25),
  scale = c(.5, .8, .9)
)

# compute aspect ratio of generated tree
ar2 <- with(ftree, (max(coord_y) - min(coord_y))/(max(coord_x) - min(coord_x)))

# scale the tree image to fit the postcard
ftree <- ftree %>%
  mutate(
    coord_x = normalise(coord_x, to = range(raster$x)),
    coord_y = normalise(coord_y, to = range(raster$y))
  ) %>%
  mutate(
    coord_x = coord_x * min(1, ar/ar2), 
    coord_y = coord_y * min(1, ar2/ar)
  ) %>% 
  mutate(
    coord_x = .2 + coord_x * .7,
    coord_y = coord_y * .8
  )



# voronoise ---------------------------------------------------------------

palette_leaf <- scico::scico(ncols + 1, palette = "lisbon")

# "leaf" coordinates are at terminal locations (id_step = 2) 
# on the terminal branches (id_leaf == TRUE) in the tree
vleaf <- ftree %>% 
  filter(id_leaf == TRUE, id_step == 2) %>%
  sample_frac(1)



# render the image --------------------------------------------------------

cat("image rendering...\n")

pic <- ggplot(
  data = raster,
  mapping = aes(x, y, fill = shade)
) + 
  
  # the raster object forms the background
  geom_raster(alpha = .5) + 
  
  # # tree trunk is drawn using geom_bezier from the
  # # ggforce package (loaded by voronoise)
  geom_bezier(
    data = ftree,
    mapping = aes(
      x = coord_x,
      y = coord_y,
      group = id_path,
      size = 1 + seg_wid * 8
    ),
    lineend = "round",
    colour = "grey20",
    alpha = 1,
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  #leaves generated using the voronoise package 
  geom_voronoise(
    data = vleaf,
    mapping = aes(
      x = coord_x,
      y = coord_y,
      group = 1
    ),
    expand = 0,
    radius = 0,
    max.radius = .018,
    size = .2,
    alpha = 1,
    fill = "grey20",
    #color = "white",
    # perturb = function(data) {
    #   f <- perturb_float(angle = 180);
    #   data <- f(data)
    #   return(data %>%
    #            filter(x > 0, x < .8, y > 0, y < .8) %>%
    #            mutate(x = x - .1))
    # },
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  
  #leaves generated using the voronoise package 
  geom_voronoise(
    data = vleaf %>% mutate(
      leaf_shade = sample(palette_leaf, n(), replace = TRUE)),
    mapping = aes(
      x = coord_x,
      y = coord_y,
      fill = leaf_shade,
      group = 1
    ),
    expand = 0,
    radius = 0,
    max.radius = .015,
    size = .2,
    alpha = .6,
    #fill = "white",
    #color = "white",
    # perturb = function(data) {
    #   f <- perturb_float(angle = 180);
    #   data <- f(data)
    #   return(data %>%
    #            filter(x > 0, x < .8, y > 0, y < .8) %>%
    #            mutate(x = x - .1))
    # },
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  
  # bunch of settings...
  scale_fill_identity() + 
  scale_size_identity() + 
  coord_equal() + 
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



