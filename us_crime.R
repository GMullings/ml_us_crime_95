setwd("~/R/mit_ml/us_crime")
.libPaths(c("C:/Users/geoff/Documents/R/win-library/3.6"))

# Ensure you have the latest versions of GGPlot2 and IPred installed, they're common hiccups
install.packages("caret")
install.packages("ellipse")
install.packages("car")
install.packages("factoextra")
install.packages("rpart.plot")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("mboost")
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(caret)
library(ellipse)
library(car)
library(factoextra)
library(cluster)
library(class)
library(rpart)
library(rpart.plot)
library(mboost)
library(randomForest)

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
nudataset = nudataset %>% drop_na(ViolentCrimesPerPop)
# Forces the NAs to 0s instead
#nudataset[is.na(nudataset)] = 0
set.seed(100)
validation_index <- createDataPartition(nudataset$murders, p=0.80, list=FALSE)
# Selecting 20% of the data for validation.
validation = nudataset[-validation_index,]
# Training is the remaining 80% of data for model training and testing.
training = nudataset[validation_index,]
trainingx = training[6:128]
validationx = validation[6:128]
trainingy = training[129:146]
carttraining = cbind.data.frame(trainingx, trainingy[17])
carttraining = carttraining[,colSums(is.na(carttraining))<1]

# Scaling datasets for K-Clustering and KNN Regression, using the predictive (X) columns to identify clusters with dependent var (Y) means. Validation set's identification and dependent var means will be compared. 
scaletraining = as.data.frame(scale(training[6:146]))
scaletraining[is.na(scaletraining)] = 0
scalevalidation = as.data.frame(scale(validation[6:146]))
scalevalidation[is.na(scalevalidation)] = 0
scaletrainingx = as.data.frame(scale(training[6:128]))
scaletrainingy = as.data.frame(scale(training[129:146]))
scalevalidationx = as.data.frame(scale(validation[6:128]))
scalevalidationy = as.data.frame(scale(validation[129:146]))
knntraining = cbind.data.frame(scaletrainingx, scaletrainingy[17])
knntraining = knntraining[,colSums(is.na(knntraining))<1]

sapply(training, class)

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

# Linear Regression setup, investigating relationships
covs = cor(training[,6:128],training[,129:146])
NonVRegs = as.data.frame(cor(training[,6:128],training[,145]))
NonVRegsPrime = subset(NonVRegs, V1 > .5) # These variables show the most promise for our "anchor" regressors.
NonVRegsNeg = subset(NonVRegs, V1 < -.5) # These variables show the most promise for our "anchor" regressors.
lm_model <- lm(ViolentCrimesPerPop ~ PctKidsBornNeverMar + PctKids2Par + racePctWhite + (PctKidsBornNeverMar*PctKids2Par), data=training) # After including the interaction term KidsBornNeverMar is insignificant.
lm_model <- lm(ViolentCrimesPerPop ~ PctKids2Par + racePctWhite + FemalePctDiv + (PctKids2Par*FemalePctDiv), data=training) # Model improved by including Female divorce percentage with an interaction term for interactions with Percentage of kids in 2 parent households

# Seeking out omitted variables
PctKidsOms = as.data.frame(cor(training[,6:128],training[,50]))
PctKidsOms = rbind.data.frame(subset(PctKidsOms, V1 > .5), subset(PctKidsOms, V1 < -.5))
cor(training$ViolentCrimesPerPop, training$pctWPubAsst)
lm_model2 <- lm(ViolentCrimesPerPop ~ PctKids2Par + racePctWhite + FemalePctDiv + pctWPubAsst + (PctKids2Par*FemalePctDiv), data=training)
racePctWhiteOms = as.data.frame(cor(training[,6:128],training[,9]))
racePctWhiteOms = rbind.data.frame(subset(racePctWhiteOms, V1 > .5), subset(racePctWhiteOms, V1 < -.5))
cor(training$ViolentCrimesPerPop, training$PctPersDenseHous)
cor(training$racePctWhite, training$PctPopUnderPov)
cor(training$ViolentCrimesPerPop, training$PctPopUnderPov)
lm_model1 <- lm(ViolentCrimesPerPop ~ racePctWhite + FemalePctDiv + pctWPubAsst + (racePctWhite*pctWPubAsst) + PctPopUnderPov, data=training)
vif(lm_model1)
lm_model2 <- lm(ViolentCrimesPerPop ~ PctKids2Par + racePctWhite + FemalePctDiv + pctWPubAsst + PctPersDenseHous + (PctKids2Par*FemalePctDiv), data=training)
vif(lm_model) # Housing Density may have been an omitted variable allowing for more interaction between race percentages and public assistance in each county.
lm_model2 <- lm(ViolentCrimesPerPop ~ racePctWhite + FemalePctDiv + pctWPubAsst + (racePctWhite*pctWPubAsst), data=training)

# Evaluating the better model with Akaike test
AIC(lm_model)
AIC(lm_model1)
AIC(lm_model2)
# Lm_model1 seems to be moderately better.

# Linear Regression Prediction
predictedlm = as.data.frame(predict(lm_model1, validation))
residslm = validation$ViolentCrimesPerPop-predictedlm
# RMSE
sqrt(mean(residslm$`predict(lm_model1, validation)`^2))
# Linear Regresion predicted validation set R Squared
cor(validation$ViolentCrimesPerPop, predictedlm) ^ 2

ggplot(residslm, aes(y=predict(lm_model1, validation))) + geom_boxplot(varwidth=T, fill="plum") +
labs(title="Linear Regression Model Violent Crimes Prediction Resids",
subtitle="",
caption="DOJ",
x="Violent Crimes",
y="Residual")

### K Cluster area, results from here will vary since this is executed partly with Monte Carlo

# K Cluster Classification Setup, finding the optimal number of clusters.
fviz_nbclust(scaletrainingx, kmeans, method = "wss") # Based on sum of squares 5 seems like the optimal number of clusters.
gap_stat = clusGap(scaletrainingx, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat) # I think the gap stat is underestimating due to tightly packed clusters.

# Predictor-ish function for kmeans
predict.kmeans <- function(object, newdata){
centers <- object$centers
n_centers <- nrow(centers)
dist_mat <- as.matrix(dist(rbind(centers, newdata)))
dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
max.col(-dist_mat)
}

# K Cluster classification
kmeans_model = kmeans(scaletrainingx, centers = 5, nstart = 25)
fviz_cluster(kmeans_model, data = scaletrainingx) # My suspicions were correct, the clusters are tightly packed with LA and NYC as two big outliers. NYC is its own cluster.
aggregate(training, by=list(cluster=kmeans_model$cluster), mean)

predictedkm = as.data.frame(predict(kmeans_model, scalevalidationx))
colnames(predictedkm) = "cluster"
predictedkm = cbind(predictedkm, validation)
aggregate(validation, by=list(cluster=predictedkm$cluster), mean) # NYC is enough of an outlier that no validation data falls into its categorization.
aggregate(validation, by=list(cluster=predictedkm$cluster), mean) - head(aggregate(training, by=list(cluster=kmeans_model$cluster), mean),-1) # The difference in means, removing the fifth cluster from the validation set because NYC is an outlier and had no comparison.
# Violent Crimes Per Pop means from the training set clustering were well within one standard deviation of the validation set's associated cluster distribution.

# K-Nearest Neighbor (KNN) Regression setup
control = trainControl(method="cv", number=10)
metric = "Rsquared"

# KNN Ridge Regression
set.seed(7)
knn_model = train(ViolentCrimesPerPop~., data=knntraining, method="knn", metric=metric, trControl=control)
# Basic KNN Regression
set.seed(7)
knn_model1 = knnreg(ViolentCrimesPerPop~., data=knntraining)
# KNN Regression predictions and evaluation
predictedknn = as.data.frame(predict(knn_model, scalevalidation))
predictedknn1 = as.data.frame(predict(knn_model1, scalevalidation))
# RMSE
residsknn = as.data.frame(scalevalidation$ViolentCrimesPerPop-predictedknn)
residsknn1 = scalevalidation$ViolentCrimesPerPop-predictedknn1
sqrt(mean(residsknn$`predict(knn_model, scalevalidation)`^2))
sqrt(mean(residsknn1$`predict(knn_model1, scalevalidation)`^2))
# R-Squared
cor(scalevalidationy$ViolentCrimesPerPop, predictedknn) ^ 2
cor(scalevalidationy$ViolentCrimesPerPop, predictedknn1) ^ 2

# Ridge regression seems to be a better fit.

ggplot(residsknn, aes(y=predict(knn_model, scalevalidation))) + geom_boxplot(varwidth=T, fill="plum") +
  labs(title="KNN Regression Model Violent Crimes Prediction Resids",
       subtitle="",
       caption="DOJ",
       x="Violent Crimes",
       y="Residual")

# CART Regression
set.seed(7)
cart_model = rpart(ViolentCrimesPerPop~., data = carttraining, control=rpart.control(cp=0.0001))
bestcart <- cart_model$cptable[which.min(cart_model$cptable[,"xerror"]),"CP"] # Best CP specification chosen based on the lowest cross validation error.
pruned_cart_model <- prune(cart_model, cp=bestcart)
prp(pruned_cart_model,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output
# Training RMSE & R Squared
trainingcart = as.data.frame(predict(pruned_cart_model, training))
residstraincart = training$ViolentCrimesPerPop-trainingcart
sqrt(mean(residstraincart$`predict(pruned_cart_model, training)`^2))
cor(training$ViolentCrimesPerPop, trainingcart) ^ 2
# CART Prediction
predictedcart = as.data.frame(predict(pruned_cart_model, validation))
residscart = validation$ViolentCrimesPerPop-predictedcart
# RMSE
sqrt(mean(residscart$`predict(pruned_cart_model, validation)`^2)) # Slightly better than linear regression
# R Squared
cor(validation$ViolentCrimesPerPop, predictedcart) ^ 2

#GLM Boost
glm_model <- glmboost(ViolentCrimesPerPop~., data=carttraining)
coef(glm_model, off2int = TRUE)
trainingglm = as.data.frame(predict(glm_model, carttraining))
residstrainglm = training$ViolentCrimesPerPop-trainingglm
sqrt(mean(residstrainglm$V1^2))
cor(training$ViolentCrimesPerPop, trainingglm) ^ 2

# GLM Prediction
predictedglm = as.data.frame(predict(glm_model, validation))
residsglm = validation$ViolentCrimesPerPop-predictedglm

# RMSE
sqrt(mean(residsglm$V1^2))
# R Squared
cor(validation$ViolentCrimesPerPop, predictedglm) ^ 2

# Support Vector Machines (SVM)
set.seed(7)
svm_model <- train(ViolentCrimesPerPop~., data=carttraining, method="svmRadial", metric=metric, trControl=control)
print(svm_model)

# SVM Predictions
predictedsvm = as.data.frame(predict(svm_model, validation))
residssvm = validation$ViolentCrimesPerPop-predictedsvm

# RMSE
sqrt(mean(residssvm$'predict(svm_model, validation)'^2))
# R Squared
cor(validation$ViolentCrimesPerPop, predictedsvm) ^ 2

# Random Forest Regressions
rf_model = randomForest(ViolentCrimesPerPop~., data=carttraining)
varImp(rf_model)
trainingrf = as.data.frame(predict(rf_model, carttraining))
residstrainrf = training$ViolentCrimesPerPop-trainingrf
sqrt(mean(residstrainrf$'predict(rf_model, carttraining)'^2))
cor(training$ViolentCrimesPerPop, trainingrf) ^ 2

predictedrf = as.data.frame(predict(rf_model, validation))
residsrf = validation$ViolentCrimesPerPop-predictedrf

# RMSE
sqrt(mean(residsrf$'predict(rf_model, validation)'^2))
# R Squared
cor(validation$ViolentCrimesPerPop, predictedrf) ^ 2