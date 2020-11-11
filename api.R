source("classifier.R")

# The input vector must be ordered in the following order:
# 
#   year, explicit, mode, popularity, acousticness, danceability, energy,
#   instrumentalness, key, liveness, loudness

#* @post /classifier
function(req, k = 5) {
  
  # All inputs are numeric, so convert...
  input <- as.numeric(req$body)
  
  # Call the k-means algorithm on the input
  result <- k.means(input, non.data.cols = c("id"))
  
  # Use k = 5
  list(recommendations = result[1:k])
}