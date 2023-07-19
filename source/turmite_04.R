grid_size <- 500L
location <- c(250L, 250L)
walk_time <- 1000000L
palette <- "tokyo"
filename <- "~/Desktop/turmite_04.png"

library(Matrix)

# initial state
# grid <- sparseMatrix(
#   i = integer(0), 
#   j = integer(0), 
#   dims = c(grid_size, grid_size)
# )
grid <- Matrix(data = 0L, nrow = grid_size, ncol = grid_size, sparse = TRUE)

facing <- "N" 
state <- 0L

for(time in 1:walk_time) {
  
  # print to user
  if(time %% 1000 == 0) cat(".")
  if(time %% 10000 == 0) cat(" ", time, "\n")
  
  # obtain cell colour
  colour <- grid[location[1], location[2]] %% 2L

  # apply rule for turmite in state 0...
  if(state == 0L) {
    if(colour == 0L) { # ... and cell in colour 0
    
      grid[location[1], location[2]] <- grid[location[1], location[2]] + 1L
      turn <- 1L 
      state <- 0L
    
    } else { # ... and cell in colour 1

      grid[location[1], location[2]] <- grid[location[1], location[2]] + 1L
      turn <- 1L
      state <- 1L      
    
    }
    
  # otherwise apply the rule for turmite in state 1...
  } else {
    if(colour == 0L) { # ... and cell in colour 0    
       turn <- 0L
       state <- 0L
    
    } else { # ... and cell in colour 1
       turn <- 0L
       state <- 1L
    
    }
  }
  
  # turn operation
  if(turn == 1L) {
    facing <- switch(facing,
                     "N" = "E",
                     "E" = "S",
                     "S" = "W",
                     "W" = "N"
    )
  }
  if(turn == -1L) {
    facing <- switch(facing,
                     "N" = "W",
                     "W" = "S",
                     "S" = "E",
                     "E" = "N"
    )
  }
  
  # calculate move
  move <- switch(facing,
                 "N" = c(0L, 1L),
                 "S" = c(0L, -1L),
                 "E" = c(1L, 0L),
                 "W" = c(-1L, 0L)
  )
  
  # update location
  location <- location + move
  location <- ((location - 1L) %% grid_size) + 1L
  
  
}

# retain only the coloured cells
grid[grid %% 2L == 0L] <- 0L
grid <- as.matrix(grid)

# write image
png(filename, 5000, 5000)
op <- par(mar=c(0,0,0,0))
image(grid, axes = FALSE, col = scico::scico(5000, palette = palette))
dev.off()
par(op)
