# Some imports
library(readr)
library(tidyr)
library(dplyr)
library(broom)
library(ggplot2)
library(ggExtra)
library(maps)
library(RColorBrewer)

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
