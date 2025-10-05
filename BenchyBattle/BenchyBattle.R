# Benchy Battle - A Puzzle Game
# This script generates and visualizes two easy, medium, and hard puzzles. 

# Load required libraries. If you don't have them installed, run:
# install.packages("ggplot2")
# install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

# Global variables for the grid size and ship fleet
GRID_SIZE <- 10
SHIPS <- c(5, 4, 3, 3, 2)

# Helper function to check if a potential ship placement is valid
isValidPlacement <- function(grid, start_x, start_y, ship_length, direction) {
  
  # Check if the ship fits within the grid bounds
  if (direction == "horizontal") {
    if ((start_x + ship_length - 1) >= GRID_SIZE) return(FALSE)
  } else { # vertical
    if ((start_y + ship_length - 1) >= GRID_SIZE) return(FALSE)
  }
  
  # Define the area to check for collisions and adjacencies
  x_start_check <- max(1, start_x - 1)
  y_start_check <- max(1, start_y - 1)
  
  if (direction == "horizontal") {
    x_end_check <- min(GRID_SIZE, start_x + ship_length)
    y_end_check <- min(GRID_SIZE, start_y + 1)
  } else { # vertical
    x_end_check <- min(GRID_SIZE, start_x + 1)
    y_end_check <- min(GRID_SIZE, start_y + ship_length)
  }
  
  # Check the surrounding area for existing ships
  for (i in x_start_check:x_end_check) {
    for (j in y_start_check:y_end_check) {
      if (grid[i, j] != 0) return(FALSE)
    }
  }
  
  return(TRUE)
}

# Main function to place all ships on the grid
placeShips <- function(grid) {
  
  placed_grid <- grid
  
  for (i in seq_along(SHIPS)) {
    ship_length <- SHIPS[i]
    ship_name <- SHIP_NAMES[i]
    
    placement_successful <- FALSE
    attempt_count <- 0
    
    while (!placement_successful && attempt_count < 100) {
      start_x <- sample(1:GRID_SIZE, 1)
      start_y <- sample(1:GRID_SIZE, 1)
      direction <- sample(c("horizontal", "vertical"), 1)
      
      if (isValidPlacement(placed_grid, start_x, start_y, ship_length, direction)) {
        # Place the ship segments
        for (k in 1:ship_length) {
          if (direction == "horizontal") {
            placed_grid[start_x + k - 1, start_y] <- i
          } else { # vertical
            placed_grid[start_x, start_y + k - 1] <- i
          }
        }
        placement_successful <- TRUE
      }
      attempt_count <- attempt_count + 1
    }
    
    # If placement fails after many attempts, restart the entire process
    if (!placement_successful) {
      return(placeShips(matrix(0, nrow = GRID_SIZE, ncol = GRID_SIZE)))
    }
  }
  
  return(placed_grid)
}

# Function to calculate the row and column hints
calculateHints <- function(grid) {
  # Fix: Swapped the apply margins to correctly calculate row and column sums
  row_hints <- apply(grid, 1, function(x) sum(x > 0))
  col_hints <- apply(grid, 2, function(x) sum(x > 0))
  return(list(row = row_hints, col = col_hints))
}

# Function to generate the puzzle with hints based on difficulty
generatePuzzleHints <- function(solved_grid, difficulty = "easy") {
  
  puzzle_grid <- matrix(0, nrow = GRID_SIZE, ncol = GRID_SIZE)
  
  # Define the total number of hints and the minimum number of ship hints
  hint_configs <- switch(difficulty,
                         "e" = list(total_hints = 15, ship_hints = 9),
                         "m" = list(total_hints = 10, ship_hints = 5),
                         "h" = list(total_hints = 5, ship_hints = 3),
                         list(total_hints = 10, ship_hints = 5)) # Default
  
  # Identify all ship and water coordinates
  all_coords <- expand.grid(x = 1:GRID_SIZE, y = 1:GRID_SIZE)
  ship_coords <- all_coords[solved_grid[all_coords$x + (all_coords$y - 1) * GRID_SIZE] > 0, ]
  
  # Randomly select the required number of ship hints
  num_ship_hints_to_select <- min(hint_configs$ship_hints, nrow(ship_coords))
  ship_hint_coords <- ship_coords[sample(nrow(ship_coords), num_ship_hints_to_select), ]
  
  # Fill the puzzle grid with the initial ship hints
  for (i in 1:nrow(ship_hint_coords)) {
    x <- ship_hint_coords[i, "x"]
    y <- ship_hint_coords[i, "y"]
    puzzle_grid[x, y] <- solved_grid[x, y]
  }
  
  # Calculate the number of remaining hints to fill
  remaining_hints_to_select <- hint_configs$total_hints - num_ship_hints_to_select
  
  # Get all coordinates that have not been revealed yet
  unrevealed_coords <- all_coords
  for (i in 1:nrow(ship_hint_coords)) {
    x <- ship_hint_coords[i, "x"]
    y <- ship_hint_coords[i, "y"]
    unrevealed_coords <- unrevealed_coords[!(unrevealed_coords$x == x & unrevealed_coords$y == y), ]
  }
  
  # Randomly select the remaining hints from the unrevealed squares
  if (remaining_hints_to_select > 0) {
    random_hint_coords <- unrevealed_coords[sample(nrow(unrevealed_coords), remaining_hints_to_select), ]
    
    # Fill the puzzle grid with the remaining hints
    for (i in 1:nrow(random_hint_coords)) {
      x <- random_hint_coords[i, "x"]
      y <- random_hint_coords[i, "y"]
      if (solved_grid[x, y] == 0) {
        puzzle_grid[x, y] <- -1 # Revealed water
      } else {
        puzzle_grid[x, y] <- solved_grid[x, y] # Revealed ship
      }
    }
  }
  
  return(puzzle_grid)
}

# Function to create a ggplot plot of a grid
plotGrid <- function(grid_matrix, row_hints, col_hints, title, show_labels = FALSE) {
  
  # Convert the matrix to a data frame for plotting
  plot_data <- expand.grid(x = 1:GRID_SIZE, y = 1:GRID_SIZE)
  plot_data$value <- as.vector(t(grid_matrix))
  
  # Define color scale based on whether it's the solved or puzzle grid
  if (show_labels) { # This is the puzzle grid
    fill_scale <- scale_fill_manual(
      values = c("gray90", "blue", rep("red", 5)),
      labels = c("Ocean", "Water Hint", rep("Ship Hint", 5)),
      name = "Piece Type"
    )
    fill_levels <- c(0, -1, 1:5)
  } else { # This is the solved grid
    fill_scale <- scale_fill_manual(
      values = c("gray90", rep("red", 5)),
      labels = c("Ocean", rep("Ship", 5)),
      name = "Piece Type"
    )
    fill_levels <- c(0, 1:5)
  }
  
  # Map values to a factor for coloring
  plot_data$fill_type <- factor(plot_data$value, levels = fill_levels)
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = x, y = y, fill = fill_type)) +
    geom_tile(color = "black", size = 0.5) +
    coord_fixed(ratio = 1) +
    fill_scale +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
    labs(title = title)
  
  # Add hints as annotations
  for (i in 1:GRID_SIZE) {
    p <- p +
      annotate("text", x = i, y = 0.5, label = col_hints[i], size = 6) +
      annotate("text", x = 10.5, y = i, label = row_hints[i], size = 6)
  }
  
  # Add text labels for ship segments if requested
  if (show_labels) {
    # Update: Only add labels for ship segments (value > 0)
    ship_labels <- plot_data[plot_data$value > 0, ]
    p <- p + geom_text(data = ship_labels, aes(label = value), color = "black", size = 5)
  }
  
  return(p)
}

# --- Main Program Logic ---

# 1. Create a list to store all the plots
all_plots <- list()

# 2. Define the difficulty levels to generate
difficulties <- c("e", "m", "h")
num_puzzles_per_difficulty <- 2

# 3. Loop through each difficulty level and generate the puzzles
for (difficulty in difficulties) {
  for (i in 1:num_puzzles_per_difficulty) {
    # Generate a solved puzzle grid
    solved_grid <- placeShips(matrix(0, nrow = GRID_SIZE, ncol = GRID_SIZE))
    
    # Calculate the numerical hints for the solved grid
    hints <- calculateHints(solved_grid)
    
    # Create the puzzle grid with hints
    puzzle_grid <- generatePuzzleHints(solved_grid, difficulty = difficulty)
    
    # Create the two plots with dynamic titles
    solved_plot <- plotGrid(solved_grid, hints$row, hints$col, 
                            paste0("Solved: ", difficulty, i), show_labels = FALSE)
    
    puzzle_plot <- plotGrid(puzzle_grid, hints$row, hints$col, 
                            paste0("Puzzle: ", difficulty, i), show_labels = TRUE)
    
    # Add both plots to the list
    all_plots[[length(all_plots) + 1]] <- solved_plot
    all_plots[[length(all_plots) + 1]] <- puzzle_plot
  }
}

# 4. Save all the plots to a single PDF file
# The plots will be arranged in a grid, with each puzzle and its solution side-by-side
pdf("benchybattle.pdf", width = 14, height = 7)
for(i in 1:(length(all_plots)/2)) {
  p_puzzle <- all_plots[[i*2]]
  p_solved <- all_plots[[i*2 - 1]]
  grid.arrange(p_puzzle,p_solved, ncol = 2)
}
dev.off()
