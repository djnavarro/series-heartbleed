library(Rcpp)
library(tidyverse)
library(ambient)
library(flametree)
library(ggforce)

# define the turmite() function
sourceCpp(here::here("turmite_d.cpp"))

output <- here::here("turmite_15.png")

grains_high <- 800
grains_wide <- 1200

pixels_high <- grains_high * 5
pixels_wide <- grains_wide * 5

nsteps <- 5000000
ncols <- 1000
ss <- 3


# create long grid for the raster with appropriate aspect ratio
ar <- grains_high / grains_wide
raster <- long_grid(
  x = seq(0, 1,  length.out = grains_wide), 
  y = seq(0, ar, length.out = grains_high)
)

# run the turmite over the grid
palette <- scico::scico(ncols + 1, palette = "vik")
grid <- t(turmite(grains_wide, grains_high, nsteps, ss))
inds <- 1 + ceiling(ncols * grid/nsteps)
raster$shade <- palette[inds]



# generate flametree ------------------------------------------------------

seed_ft <- 2
set.seed(seed_ft)

# the "flametree" itself
ftree <- flametree_grow(
  time = 14,
  seed = seed_ft,
  angle = c(-2:4) * 10,
  scale = c(.6, .8, .9)
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
    coord_x = .2 + coord_x * .8,
    coord_y = coord_y * .8
  )



# render the image --------------------------------------------------------

cat("rendering image...\n")

pic <- ggplot(
  data = raster,
  mapping = aes(x, y, fill = shade)
) + 
  
  # the raster object forms the background
  geom_raster() + 
  
  # # tree trunk is drawn using geom_bezier from the
  # # ggforce package (loaded by voronoise)
  geom_bezier(
    data = ftree,
    mapping = aes(
      x = coord_x,
      y = coord_y,
      group = id_path,
      size = .5 + seg_wid * 8
    ),
    lineend = "round",
    colour = "white",
    alpha = 1,
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



