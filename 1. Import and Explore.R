# Some imports
library(readr)
library(tidyr)
library(dplyr)
library(broom)
library(ggplot2)
library(ggExtra)
library(maps)
library(RColorBrewer)
library(tidyverse)
library(gridExtra)
library(grid)
library(png)
l#ibrary(downloader)
library(grDevices)
library(cluster) 
library(factoextra)
install.packages("caret")
library(caret)

# Set wd and import
train <- read.csv(file = 'train.csv')
head(train) # Really big
ncol(train)
nrow(train)

summary(train) # Very hard to read.

# Lets create something that can be more easily read.
train.data <- train[,-c(1,2)]
train.means <- colMeans(train.data) # Means of each column
train.sd <- apply(train.data, 2, sd) # SD of each column

ggplot(as.data.frame(train.means), aes(x = train.means)) + geom_histogram(color="black", fill="blue",binwidth = 0.01)
ggplot(as.data.frame(train.sd), aes(x = train.sd)) + geom_histogram(color="black", fill="green",binwidth = 0.01)

# With a normal distribution
# Means
ggplot(as.data.frame(train.means), aes(x = train.means)) + geom_histogram(aes(y =..density..),
                                                                          color="black", 
                                                                          fill="blue",
                                                                          binwidth = 0.01) + 
  stat_function(fun = dnorm, args = list(mean = mean(train.means), sd = sd(train.means)))

# Standard Deviations
ggplot(as.data.frame(train.sd), aes(x = train.sd)) + geom_histogram(aes(y =..density..),
                                                                    color="black", 
                                                                    fill="green",
                                                                    binwidth = 0.01) +
  stat_function(fun = dnorm, args = list(mean = mean(train.sd), sd = sd(train.sd)))

# Test for normality
qqnorm(train.means,main="QQ plot of means",pch=19)
qqline(train.means)

qqnorm(train.sd,main="QQ plot of SDs",pch=19)
qqline(train.sd)

# Test actual data using Shapiro-Wilk
shapiro.test(train.data[,2:4]) # Error because more than one column
#install.packages("mvShapiroTest")
#library(mvShapiroTest)
#mvShapiro.Test(as.matrix(train.data[,2:150]))
#install.packages("MVN")
library(MVN)
mvn(train.data,multivariatePlot = "qq")
# Nope!

# Correlation Analysis
res <- cor(train.data)
round(res, 2)
library(reshape2)
melted_data <- melt(res)
head(melted_data)
ggplot(data = melted_data, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
# Looks like white noise!

head(melted_data)
ggplot(melted_data, aes(x = melted_data$value)) + geom_histogram(aes(y =..density..),
                                                                    color="black", 
                                                                    fill="yellow",
                                                                    binwidth = 0.01) + 
  stat_function(fun = dnorm, args = list(mean = mean(melted_data$value), sd = sd(melted_data$value)))


# PCA
# Standardise the data
scaled.dat <- scale(train.data)
# check that we get mean of 0 and sd of 1
colMeans(scaled.dat)  # Looks good.

dat.pca <- prcomp(scaled.dat)
summary(dat.pca)
# Not very useful

# K means clustering
clusters <- kmeans(scaled.dat, 5)
str(clusters)

# We want to figure out if there is an optimum number of clusters
silhouette_score <- function(k){
  km <- kmeans(scaled.dat, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(scaled.dat))
  mean(ss[,3])
}
k <- 2:20
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
# It seems as though 16 might be the optimum number of clusters

# Another visualisation
fviz_nbclust(scaled.dat, kmeans, method='silhouette',k.max = 20)

# Actual clustering 
km.final <- kmeans(scaled.dat, 16)
## Total Within cluster sum of square
km.final$tot.withinss
## Cluster sizes
km.final$size
sum(km.final$size) # 250 
dat.km <- as.data.frame(scaled.dat)
dat.km$cluster <- km.final$cluster


# KNN 

set.seed(123)
# Add back in the target variable first
scaled.dat <- cbind(scaled.dat,train$target)

# break up the data into train and test 
##Generate a random number that is 90% of the total number of rows in dataset.
ran <- sample(1:nrow(scaled.dat), 0.9 * nrow(scaled.dat)) 

##extract training set
scaled.train <- scaled.dat[ran,] 
##extract testing set
scaled.test <- scaled.dat[-ran,] 

library(class)
pr <- knn(scaled.train[,0:300],scaled.test[,0:300],cl=scaled.train[,301],k=13)

##create confusion matrix
tab <- table(pr,scaled.test[,301])

##this function divides the correct predictions by total number of predictions that tell us how accurate the model is.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
## 76%

vector = c() # This is the empty vector where we will store our scores
neighbour_score <- function(k){
  pr <- knn(scaled.train[,0:300],scaled.test[,0:300],cl=scaled.train[,301],k)
  tab <- table(pr,scaled.test[,301])
  vector <- c(accuracy(tab))
}
k <- 2:20
avg_neighbour <- sapply(k, neighbour_score)
plot(k, type='b', avg_neighbour, xlab='Number of neighbours', ylab='Average Accuracy Scores (%)', frame=FALSE)
# It seems as though 7 might be the optimum number of neighbours

# Odd plot.

# Lets try support vector machine, SVM
library(e1071)
library(rpart)

## svm
svm.model <- svm(scaled.train[,301] ~ .,data = scaled.train)
svm.pred <- predict(svm.model, scaled.test)
# Have a look at both predictions and actual target data
svm.pred
scaled.test[,301]
# Seems like we could guess a cutoff (not very accurate).

## svm with different values
svm.model <- svm(scaled.train[,301] ~ .,data = scaled.train,cost = 5)
svm.pred <- predict(svm.model, scaled.test)
# Have a look at both predictions and actual target data
svm.pred
scaled.test[,301]
# Seems like we could guess a cutoff (not very accurate).

# Find a function to define a cutoff value
cutoff_fn <- function(z){
  sp <- ifelse(svm.pred > z,1,0) # Apply the cutoff
  sp_tab <- table(sp,scaled.test[,301]) # Tabulate the values
  # Calculate the total it got correct
  correct <- sum(diag(sp_tab))/sum(sp_tab)
}
z <- seq(0.1, 1, by=0.05)
accuracy <- sapply(z, cutoff_fn)

plot(z, type='b', accuracy, xlab='Cutoff Value for Prediction', ylab='Accuracy Score (%)', frame=FALSE)
# It seems as though a cutoff of 0.45 might be the optimum cutoff value


# Create a dataframe and store the next function's results
df <- data.frame(matrix(ncol = 3, nrow = length(z)))
x <- c("0.1", "1", "10")
colnames(df) <- x


# Now try to change the cost value in the svm() function
costvector <- c(0.1,1,1000)
#accuracy <- sapply(z, cutoff_fn)

for (i in 1:length(costvector)) {
  svm.model <- svm(scaled.train[,301] ~ .,data = scaled.train,cost = i)
  svm.pred <- predict(svm.model, scaled.test)
  
  accuracy <- sapply(z, cutoff_fn)
  df[,i] <- accuracy
}


plot(z, type='b', df[,1], xlab='Cutoff Value for Prediction', ylab='Accuracy Score (%)', frame=FALSE)
lines(z, df[,2], col="red",type='b')
lines(z, df[,3], col="blue",type = 'b')

# So a cutoff value of 0.45 and a cost of 1 seems good.


# K(10) fold cross validations
library(caret)

set.seed(121)
flds <- createFolds(1:nrow(scaled.dat), k = 10, list = TRUE, returnTrain = FALSE)

# Create an empty vector to store the accuracy in
result <- c()

# Run the SVM model for each fold, where each fold indexing is the test set
for (i in 1:10) {
  # Get the new training and test sets for current fold
  new.test.set <- scaled.dat[unlist(flds[i],use.names = FALSE),]
  new.train.set <- scaled.dat[-unlist(flds[i],use.names = FALSE),]
  
  # Run the model for the values found above
  svm.mod <- svm(new.train.set[,301] ~ .,data = new.train.set,cost = 5)
  svm.pred <- predict(svm.mod, new.test.set)
  
  # Now apply the cutoff value of 0.45
  sp <- ifelse(svm.pred > 0.45,1,0) # Apply the cutoff
  sp_tab <- table(sp,new.test.set[,301]) # Tabulate the values
  # Calculate the total it got correct
  #correct <- sum(diag(sp_tab))/sum(sp_tab)
  result[i] <- sum(diag(sp_tab))/sum(sp_tab)
}

result
mean(result) # 0.86%, not bad!
