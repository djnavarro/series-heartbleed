library(tidyverse)

set.seed(1)

grain <- 1000L
dirs <- c("N", "E", "S", "W")
n_turmites <- 10000
n_steps <- 10000
step <- 1
filename <- "~/Desktop/turmite_09.png"

colours <- reverse(scico::scico(300, palette = "oslo"))


reverse <- function(x){
  x[seq(length(x), 1)]
}

turmites <- tibble(
  x = sample(grain, n_turmites, TRUE),
  y = sample(grain, n_turmites, TRUE),
  write = TRUE,
  facing = sample(dirs, n_turmites, TRUE),
  state = sample(c(TRUE, FALSE), n_turmites, TRUE),
  turn = !state
)



# cells is a numeric matrix
cells <- matrix(
  data = 0L, 
  nrow = grain, 
  ncol = grain
)

reorient <- function(facing, turn) {
  move <- c("N" = "E", "E" = "S", "S" = "W", "W" = "N")
  facing[turn == TRUE] <- move[facing[turn == TRUE]]
  return(facing)
}

wrap <- function(x) {
  ((x - 1) %% grain) + 1
}

x_shift <- function(x, facing, step = 1) {
  move <- c("N" = 0, "E" = 1, "S" = 0, "W" = -1)
  x <- x + move[facing] * step
  return(wrap(x))
}

y_shift <- function(y, facing, step = 1) {
  move <- c("N" = 1, "E" = 0, "S" = -1, "W" = 0)
  y <- y + move[facing] * step
  return(wrap(y))
}

mat <- function(x, y) {
  matrix(c(x, y), ncol = 2)
}

for(i in 1:n_steps) { 
  
  if(i %% 100 == 0) cat(".")
  if(i %% 1000 == 0) cat(" ", i, "\n")
  
  # turmites turn based on their current state
  turmites <- turmites %>% 
    mutate(
      turn = !state,
      write = state,
      facing = reorient(facing, turn),
      state = cells[mat(x, y)] %% 2 == 1
    )
  
  # turmites write
  x <- turmites$x[turmites$write == TRUE]
  y <- turmites$y[turmites$write == TRUE]
  ind <- mat(x,y) 
  cells[ind] <- cells[ind] + 1

  # turmites move forward based on their turn/facing
  turmites <- turmites %>% 
    mutate(
      x = x_shift(x, facing, step),
      y = y_shift(y, facing, step)
    )
}

# retain only the coloured cells
image <- cells + 2
image[image %% 2 == 0] <- 0 #image[image %% 2 == 0] * -1

# write image
png(filename, 5000, 5000)
op <- par(mar=c(0,0,0,0))
image(image, axes = FALSE, col = colours)
dev.off()
par(op)
