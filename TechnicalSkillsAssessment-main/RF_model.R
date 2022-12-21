
###create local filepaths and install/load relevant libraries
getwd()
setwd("C://Users//shira//Desktop//Technical Skills Assessment_2022")

#install.packages("tree")
#install.packages("naniar")
#install.packages("ppcor")
library("tree")
library("dplyr")
library("naniar")
library(tidyverse)
library(randomForest)
library(ppcor)

###load input files into a dataframe
df <- read.csv("clinical_preprocessing.csv", stringsAsFactors = F)

###exploratory analysis: checking pearson correlation between Num.Mutated.Genes & Num.Mutations
df2 <- df[, c(15, 16)]
pcor(df2, method = "pearson")

###Selection of inputs for the RF model
inputs <- df[, c(2, 3, 4, 6, 10, 11, 12, 13, 15, 17)]
inputs$Outcome <- as.character(inputs$Outcome)
inputs$Outcome <- as.factor(inputs$Outcome)


#####USING RF MODEL#####

#use bagging to avoid losing data while training model

set.seed(12345)
bag.tree.DN = randomForest(Outcome~., data = inputs, mtry = 3, ntree = 501,
                           importance = TRUE, na.action=na.roughfix, keep.inbag = TRUE,
                           norm.votes = FALSE, replace = FALSE)


###generate a visualization of per-category outcome error rate, as well as the OOB error
###rate, and how they change as the number of decision trees are increased
plot(bag.tree.DN, legend=TRUE)

###generate summary statistics of model
summary(bag.tree.DN)

###generate confusion matrix to evaluate final error rates
bag.tree.DN$confusion

###save individual number of decision tree votes as a separate dataframe
bag.tree.DN_df <- as.data.frame(bag.tree.DN$votes)
dim(bag.tree.DN_df)

View(bag.tree.DN_df)

###export to csv format
write.csv(bag.tree.DN_df,
          "votes.csv")
print(row.names(bag.tree.DN_df))

