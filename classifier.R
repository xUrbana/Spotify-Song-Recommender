library(readr)
library(dplyr)
library(caret)


# Read in data, drop unneeded columns, and reorder it
raw.data <- readr::read_csv("data/data.csv") %>%
  dplyr::select(-release_date) %>%
  dplyr::relocate(c("id", "name", "artists", "year", "explicit",
                    "mode", "popularity")) %>%
  dplyr::as_tibble()

raw.data$artists <- as.factor(raw.data$artists)

data <- raw.data %>%
  dplyr::select(-c(id, name))


# Create index to split based on labels  
index <- caret::createDataPartition(data$artists, p=0.75, list=FALSE)

# Subset training set with index
data.training <- data[index,]

# Subset test set with index
data.test <- data[-index,]

knn.model <- caret::train(
  data.training[-1],
  data.training$artists,
  method = "knn")

