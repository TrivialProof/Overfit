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


