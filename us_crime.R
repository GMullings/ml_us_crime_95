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

sapply(training, class)

Summary(training$murders)
summary(training$murders)
summary(training$murdPerPop)
summary(training$rapes)
summary(training$rapesPerPop)
summary(training$robberies)
summary(training$robbbPerPop)
summary(training$assaults)
summary(training$assaultPerPop)
summary(training$burglaries)
summary(training$burglPerPop)
summary(training$larcenies)
summary(training$larcPerPop)
summary(training$autoTheft)
summary(training$autoTheftPerPop)
summary(training$arsons)
summary(training$arsonsPerPop)
summary(training$ViolentCrimesPerPop)
summary(training$nonViolPerPop)
summary(training$population)

m <- ggplot(training, aes(y=murders)) + geom_boxplot(varwidth=T, fill="plum") +
  labs(title="Murders",
       subtitle="",
       caption="DOJ",
       x="Murders",
       y="Count")

mp <- ggplot(training, aes(y=murdPerPop)) + geom_boxplot(varwidth=T, fill="plum") +
  labs(title="Murders",
       subtitle="",
       caption="DOJ",
       x="Murders",
       y="Per Resident")

m
mp
# Tighter bounds around around murder pers pop.

v <- ggplot(training, aes(y=ViolentCrimesPerPop)) + geom_boxplot(varwidth=T, fill="plum") +
  labs(title="Violent Crimes",
       subtitle="",
       caption="DOJ",
       x="Violent Crimes",
       y="Per Resident")

nv <- ggplot(training, aes(y=nonViolPerPop)) + geom_boxplot(varwidth=T, fill="plum") +
  labs(title="Non-Violent Crimes",
       subtitle="",
       caption="DOJ",
       x="Non-Violent Crimes",
       y="Per Resident")

crimebifig = ggarrange(v, nv,
                ncol = 2)
crimebifig

plot(training[,6], training[,146], main="Population x Non-violent Crime Per Person", xlab="Population", ylab="Non-Violent Crime Per Person", pch=20)
plot(training[,6], training[,145], main="Population x Violent Crime Per Person", xlab="Population", ylab="Violent Crime Per Person", pch=20)
# Crime Rates seem unrelated to population after per person control.
plot(training[,8], training[,145], main="County Black % x Violent Crime Per Person", xlab="Population", ylab="% Black", pch=20)
# Seems like a positive relationship between the proportion of Black residents in the County and crime, but this is likely masking an omitted variable.
plot(training[,8], training[,34], main="County Black % x % In Poverty", xlab="Population % Black", ylab="Population % In Poverty", pch=20)
# Poverty and the Black proportion seem correlated, and might explain the relationship between race proportions and crime.
plot(training[,34], training[,145], main="County % In Poverty x Violent Crime Per Person", xlab="Population % In Poverty", ylab="Violent Crimes Per Person", pch=20)
# Violent crime and Poverty are positively related.
