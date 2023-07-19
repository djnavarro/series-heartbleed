library(Rcpp)
library(dplyr)
library(ggplot2)

heartbleed <- function(seed) {
  
  set.seed(seed)
  
  sys_id <- "67"
  sys_name <- "heartbleed"
  
  prefix <- paste0(sys_name, "_", sys_id, "_")
  fname <- paste0(prefix, seed, ".jpg")
  output <- here::here("image", paste0("sys_", sys_id), fname)
  
  
  # sample palette from the colorir package
  sample_shades <- function(scramble = TRUE) {
    pal <- sample(colorir::colores$palette_name, 1)
    shades <- colorir::colores$colour[
      colorir::colores$palette_name == pal
    ]
    if(scramble) shades <- sample(shades)
    return(shades)
  }
  
  blend_shades <- function(x, y, p = .5) {
    x <- col2rgb(x)
    y <- col2rgb(y)
    z <- round(p*x + (1-p)*y)
    z <- rgb(red = z[1, ]/255, green = z[2, ]/255, blue = z[3, ]/255)
    return(z)
  }
  
  # interpolate colours
  mix_shades <- function(shades, n) {
    (colorRampPalette(shades))(n)
  }
  
  # create two separate colour vectors
  ncols <- 6
  palette <- sample_shades()
  palette_base <- mix_shades(palette[1:2], ncols)
  palette_heart <- mix_shades(palette[3:4], ncols)
  
  # other parameters
  heart_size <- runif(1, min = .4, max = .6)
  n_slices <- sample(10:15, 1)
  shift <- c(.3, .4)
  
  
  
  # start -------------------------------------------------------------------
  
  
  grains_high <- 1250
  grains_wide <- 1250
  
  pixels_high <- grains_high * 4
  pixels_wide <- grains_wide * 4
  
  
  
  # turmite background ------------------------------------------------------
  
  cat("turmite wandering...\n")
  
  # define the turmite() function
  sourceCpp(here::here("source", paste0("turmite.cpp")))
  
  nsteps <- 10000000
  ss <- 3
  
  # create long grid for the raster with appropriate aspect ratio
  ar <- grains_high / grains_wide
  raster <- ambient::long_grid(
    x = seq(0, 1,  length.out = grains_wide), 
    y = seq(0, ar, length.out = grains_high)
  )
  
  grid <- t(turmite(grains_wide, grains_high, nsteps, ss))
  inds <- 1 + ceiling(ncols * grid/nsteps)
  raster$shade <- palette_base[inds]
  
  
  
  # generate dust heart ------------------------------------------------------
  
  cat("dust heart beating...\n")
  
  dat <- 
    jasmines::use_seed(seed) %>%
    jasmines::scene_grid(
      x = 1:3, 
      y = 1:3,
      entity = "circle",
      size = 1,
      grain = 500
    ) %>%
    mutate(
      x = ambient::normalise(x),
      y = ambient::normalise(y),
      ind = 1:n()
    ) %>%
    jasmines::unfold_slice(
      iterations = n_slices, 
      scale = .5 * 10^-24, 
      scatter = TRUE, 
      output1 = "id"
    ) %>%
    jasmines::unfold_breeze(
      iterations = 10,
      scale = .0002,
      drift = .0005,
      fractal = ambient::ridged,
      octaves = 8
    ) %>%
    jasmines::unfold_inside() %>%
    mutate(val = 1 + (inside>0)*ind)
  
  
  dat$val <- ambient::normalise(dat$val, to = c(1, ncols+1))
  dat$val <- round(dat$val)
  
  dat$shade <- palette_heart[dat$val]
  
  #dat$x <- (dat$x + shift[1])
  #dat$y <- (dat$y + shift[2])
  
  
  # render the image --------------------------------------------------------
  
  cat("image rendering...\n")
  
  bg <- palette_base[round(ncols/2)]
  brd <- 0

  #shadow <- blend_shades(bg, "black")
  shadow <- "black"
  
  pic <- ggplot(
    data = raster,
    mapping = aes(x, y, fill = shade)
  ) + 
    
    # the raster object forms the background
    geom_raster(alpha = .5) + 
    
    
    # the heart shadow is made of dust/points
    geom_point(
      data = dat, 
      mapping = aes(
        x = x + rnorm(nrow(dat), sd = .1), 
        y = y + rnorm(nrow(dat), sd = .1), 
        colour = purrr::map_chr(shade, ~ blend_shades(.x, shadow)),
        alpha = exp(-(time - 1) / 20)
      ), 
      inherit.aes = FALSE,
      show.legend = FALSE,
      stroke = 0,
      size = 1
    ) +
    
    # the heart is made of dust/points
    geom_point(
      data = dat, 
      mapping = aes(
        x = x, 
        y = y, 
        color = shade, 
        alpha = exp(-(time - 1) / 20)
      ), 
      stroke = 0,
      inherit.aes = FALSE,
      show.legend = FALSE,
      size = 1
    ) +
    
    # bunch of settings...
    scale_fill_identity() + 
    scale_colour_identity() + 
    scale_alpha_identity() + 
    coord_equal(
      xlim = c(0 + brd, 1 - brd), 
      ylim = c(0 + brd, 1 - brd)
    ) + 
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void() + 
    theme(
      panel.background = element_rect(
        fill = bg, 
        color = bg
      )
    ) +
    NULL
  
  # export image
  ggsave(
    filename = output,
    plot = pic,
    width = pixels_wide / 300,
    height = pixels_high / 300,
    dpi = 150 #300
  )
  
}

seeds <- 1520:1599
for(seed in seeds) {
  cat("seed", seed, "\n")
  heartbleed(seed)
}
