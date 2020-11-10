require(plumber)

# Specify the API location
r <- plumb("api.R")

# Run the web api on port 8008
r$run(port=8008)
