library("tidyverse")

data <- read.csv("data_raw/data.csv")
fillers <- read.csv("data_raw/fillers_grade.csv")

data %>% select("Label", "Field name", "Field value", "PennElementType", "PennElementName", "Parameter", "Value", "partid", "sentence", "construct", "classification", "IO", "counter", "group", "item")

# Loop through participant ids to calculate?
for(id in unique(unlist(data[c("partid")]))) {
  # Select all data where column = id
  # Select all data where Parameter = Choice and PennElementName = test
  curr_data <- subset(data, partid==id & Parameter=="Choice" & PennElementName=="test")
  
  # Select Choice values and calculate stdev and mean
  # Assuming scores <- data[c(whatever meets criteria?)] 
  # zscores <- (scores - mean(scores)) / sd(scores)
  scores <- as.vector(curr_data$Value)
  zscores <- (scores - mean(scores)) / sd(scores)
  
  # Loop through fillers where classification = high
  # Compare zscore and increment count of unexpected
  
  # Loop through fillers where classification = low
  
  # If bad, then???
  
}