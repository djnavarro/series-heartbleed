library(tidyverse)

grain <- 500L
dirs <- c("N", "E", "S", "W")
n_turmites <- 3000
step <- 1

turmites <- tibble(
  x = sample(grain, n_turmites, TRUE),
  y = sample(grain, n_turmites, TRUE),
  location = x + (y - 1)*grain,
  write = TRUE,
  facing = sample(dirs, n_turmites, TRUE),
  state = sample(c(TRUE, FALSE), n_turmites, TRUE),
  turn = !state
)

palette <- jasmines::palette_named("magma")
filename <- "~/Desktop/turmite_05.png"

library(Matrix)


# place is a sparse numeric matrix
place <- Matrix(
  data = 0L, 
  nrow = grain^2, 
  ncol = 1L, 
  sparse = TRUE
)

reorient <- function(facing, turn) {
  move <- c("N" = "E", "E" = "S", "S" = "W", "W" = "N")
  facing[turn == TRUE] <- move[facing[turn == TRUE]]
  return(facing)
}

x_shift <- function(x, facing, step = 1) {
  move <- c("N" = 0, "E" = 1, "S" = 0, "W" = -1)
  x <- x + move[facing] * step
  x <- ((x - 1) %% grain) + 1
  return(x)
}

y_shift <- function(y, facing, step = 1) {
  move <- c("N" = 1, "E" = 0, "S" = -1, "W" = 0)
  y <- y + move[facing] * step
  y <- ((y - 1) %% grain) + 1
  return(y)
}

for(i in 1:500) { 
  
  if(i %% 10 == 0) cat(".")
  if(i %% 100 == 0) cat(" ", i, "\n")
  
  # turmites turn based on their current state
  turmites <- turmites %>% 
    mutate(
      turn = !state,
      write = state,
      facing = reorient(facing, turn),
      state = place[location] %% 2 == 1
    )
  
  # turmites write
  ind <- turmites$location[turmites$write == TRUE]
  place[ind] <- place[ind] + 1

  # turmites move forward based on their turn/facing
  turmites <- turmites %>% 
    mutate(
      x = x_shift(x, facing, step),
      y = y_shift(y, facing, step),
      location = x + (y - 1) * grain
    )
  
}

# retain only the coloured cells
place <- place + 2
place[place %% 2 == 0] <- 0

img <- as.vector(place)
img <- matrix(img, grain, grain)

# write image
png(filename, grain, grain)
op <- par(mar=c(0,0,0,0))
image(img, axes = FALSE, col = palette(50))
dev.off()
par(op)
