setwd("~/R/mit_ml/us_crime")
.libPaths(c("C:/Users/geoff/Documents/R/win-library/3.6"))

# Ensure you have the latest versions of GGPlot2 and IPred installed, they're common hiccups
install.packages("caret")
install.packages("ellipse")
library(caret)
library(ellipse)

# Ingestion
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00211/CommViolPredUnnormalizedData.txt","CommViolPredUnnormalizedData.txt")
dataset <- read.delim("CommViolPredUnnormalizedData.txt", sep = ",", header=FALSE)
filename <- "headers.csv" # Headers had to be created in Excel from attribute information in README
headers <- read.csv(filename)
colnames(dataset) = colnames(headers)

# EDA
dim(dataset)
sapply(dataset, class)

x = dataset[,6:129] # Potential predictors
y = dataset[130:147] # Potential goals

head(y)
sapply(y,class) # All of these should be numeric.

# Converting factors in goal variables into numeric data

f = sapply(y,is.factor)
y[ , f] = as.data.frame(apply(y[ , f], 2, as.numeric))
warnings() # Previously "?" data are now NAs. Compare to prior head(y) output.
sum(is.na(y))
head(y)
sapply(y,class) # All numeric.
y = cbind.data.frame(dataset[ ,1:5],y) # Rebinding demographics to goals, first five columns

head(x)
sapply(x,class) # Nearly all of the factors here should be numeric except LemasGangUnitDeploy. It's technically nomial with 0 means NO, 10 means YES, and 5 means Part Time.

# Converting factors in predictive variables into numeric data

f = sapply(x,is.factor)
x[ , f] = as.data.frame(apply(x[ , f], 2, as.numeric))
warnings() # Previously "?" data are now NAs. Compare to prior head(y) output.
head(x)
sapply(x, class) # All numeric
NominalX = dataset$LemasGangUnitDeploy # Isolating multinomial predictor
levels(NominalX) # 0 means NO, 10 means YES, and 5 means Part Time.
x = x[ ,-122] # Removing LemasGangUnitDeploy from numeric predictor set.
x = cbind.data.frame(dataset[ ,1:5],x) # Rebinding demographics to goals, first five columns

# Creating the training dataset of 80% of the rows in the original dataset.
nudataset = cbind.data.frame(x,y[6:23])
validation_index <- createDataPartition(nudataset$murders, p=0.80, list=FALSE)
# Selecting 20% of the data for validation.
validation = nudataset[-validation_index,]
# Training is the remaining 80% of data for model training and testing.
training = nudataset[validation_index,]







