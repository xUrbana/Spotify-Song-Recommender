require(readr)
require(dplyr)
require(parallel)
require(tictoc)

std.devs <- readr::read_rds("data/std.devs.rds")
mean.vector <- readr::read_rds("data/mean.vector.rds")
prepped.data <- readr::read_rds("data/prepped.data.rds")

k.means <- function(input, non.data.cols = NA) {

  df <- prepped.data
  
  # Use 1 less than the number of cores on the system
  cores <- detectCores()
  cl <- makeCluster(cores[1] - 1)
  
  # If columns to drop were provided, drop them
  if (is.vector(non.data.cols)) {
    data.cols <- df %>%
      dplyr::select(-non.data.cols)
  } else {
    data.cols <- df
  }
  
  # Scale and center input to match the data
  input <- (input - mean.vector) / std.devs
  
  # Euclidean distance from the ith data row to the input row
  find.dist <- function(i) {
    dist(rbind(data.cols[i, ], input))
  }
  
  # Export object to each core for computation
  clusterExport(cl, c("find.dist", "data.cols", "input"), envir = environment())
  
  # Compute distances in parallel, preserve order
  distances <- parSapply(cl, 1:nrow(data.cols), find.dist)
  
  # Terminate the cluster, clean up connections
  stopCluster(cl)
  
  # Add the distances to the original data frame and order them
  df$distance <- distances
  
  # Return the data ordered by distance
  df[order(df$distance),]
}

read.data <- function(path, drop.cols = NULL, order.cols = NULL, factors = NULL) {
  
  # Read the RDS file
  data <- readr::read_rds(path)
  
  # If there are columns to drop, drop them
  if (is.vector(drop.cols)) {
    data <- data %>%
      dplyr::select(-drop.cols)
  }
  
  # If an order is requested, order it
  if (is.vector(order.cols)) {
    data <- data %>%
      dplyr::relocate(order.cols)
  }
  
  # Convert to tibble for easy use with dplyr
  data <- dplyr::as_tibble(data)
  
  # Convert columns to factor
  if (is.vector(factors)) {
    data[factors] <- lapply(data[factors], factor)
  }
  
  data
}
