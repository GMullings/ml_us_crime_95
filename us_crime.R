setwd("~/R/mit_ml/us_crime")
.libPaths(c("C:/Users/geoff/Documents/R/win-library/3.6"))

dataset <- read.delim("CommViolPredUnnormalizedData.txt", sep = ",", header=FALSE)
filename <- "headers.csv" # Headers had to be created in Excel from attribute information in README
headers <- read.csv(filename)
colnames(dataset) = colnames(headers)

dim(dataset)
sapply(dataset, class)