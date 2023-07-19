grid_size <- 200
walk_time <- 100000
filename <- "~/Desktop/turmite_02.png"

machine <- list(
  grid = matrix(
    data = 0,
    nrow = grid_size,
    ncol = grid_size
  ),
  turmite = list(
    x = round(grid_size/2),
    y = round(grid_size/2),
    facing = "north",
    state = 0
  )
)


move <- function(turmite) {
  if(turmite$facing == "north") turmite$y <- turmite$y + 1
  if(turmite$facing == "south") turmite$y <- turmite$y - 1
  if(turmite$facing == "east")  turmite$x <- turmite$x + 1
  if(turmite$facing == "west")  turmite$x <- turmite$x - 1
  if(turmite$x == 0) turmite$x <- grid_size
  if(turmite$x == grid_size + 1) turmite$x <- 1
  if(turmite$y == 0) turmite$y <- grid_size
  if(turmite$y == grid_size + 1) turmite$y <- 1
  return(turmite)
}

turn <- function(turmite, direction) {
  if(direction == "clockwise") {
    turmite$facing <- switch(turmite$facing,
      "north" = "east",
      "south" = "west",
      "east" = "south",
      "west" = "north"
    )
    return(turmite)
  }
  if(direction == "anticlockwise") {
    turmite$facing <- switch(turmite$facing,
      "north" = "west",
      "south" = "east",
      "east" = "north",
      "west" = "south"
    )
    return(turmite)
  }
  
  return(turmite)
}

transition <- function(machine, time) {

  if(time %% 1000 == 0) cat(".")
  if(time %% 10000 == 0) cat(" ", time, "\n")

  grid <- machine$grid
  turmite <- machine$turmite
  colour <- grid[turmite$x, turmite$y]

  if(colour == 0 & turmite$state == 0) {
    grid[turmite$x, turmite$y] <- time       # write
    turmite <- turn(turmite, "clockwise") # turn
    turmite <- move(turmite)              # move
    turmite$state <- 0
    return(list(grid = grid, turmite = turmite))
  }

  if(colour == 0 & turmite$state == 1) {
    grid[turmite$x, turmite$y] <- 0       # write
    turmite <- turn(turmite, "anticlockwise")      # turn
    turmite <- move(turmite)              # move
    turmite$state <- 0
    return(list(grid = grid, turmite = turmite))
  }

  if(colour > 0 & turmite$state == 0) {
    grid[turmite$x, turmite$y] <- time
    turmite <- turn(turmite, "clockwise")
    turmite <- move(turmite)
    turmite$state <- 1
    return(list(grid = grid, turmite = turmite))
  }

  if(colour > 0 & turmite$state == 1) {
    grid[turmite$x, turmite$y] <- 0
    turmite <- turn(turmite, "null")
    turmite <- move(turmite)
    turmite$state <- 1
    return(list(grid = grid, turmite = turmite))
  }

}


machine <- purrr::reduce(
  .x = 1:walk_time,
  .f = transition,
  .init = machine
)

png(filename, 1000, 1000)
op <- par(mar=c(0,0,0,0))
image(machine$grid, axes = FALSE, col = scico::scico(100, palette = "lajolla"))
dev.off()
par(op)
