library(mlbench)
library(caret)

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

library(e1071)
library(caTools)
library(effects)

set.seed(7)

# # load data

full_feat_set <- read.csv("full_feature_set_for_models.csv", header = TRUE)
elo_feat_set <- read.csv("elo_feature_set_for_models.csv", header = TRUE)
elo_wr_pstats_feat_set <- read.csv("elo_wr_pstats_feature_set_for_models.csv", header = TRUE)

full_feat_set$outcome_label <- as.factor(full_feat_set$outcome_label)
elo_feat_set$outcome_label <- as.factor(elo_feat_set$outcome_label)
elo_wr_pstats_feat_set$outcome_label <- as.factor(elo_wr_pstats_feat_set$outcome_label)

# shuffle
shuffle_index <- sample(1:nrow(orig_data))

# shuffled data
full_feat_set <- full_feat_set[shuffle_index,]
elo_feat_set <- elo_feat_set[shuffle_index,]
elo_wr_pstats_feat_set <- elo_wr_pstats_feat_set[shuffle_index,]

trainRowNumbers <- createDataPartition(full_feat_set$outcome_label, p=0.8, list=FALSE)
trainData <- full_feat_set[trainRowNumbers,]
testData <- full_feat_set[-trainRowNumbers,]

pca_train_data <- trainData
pca_test_data <- testData

pca_train_labs <- pca_train_data$outcome_label
pca_train_data <- pca_train_data[,-ncol(pca_train_data)]

train.pca <- prcomp(pca_train_data, scale = TRUE)
pca_train_data <- data.frame(train.pca$x[,1:5], outcome_label = pca_train_labs)

pca_test_labs <- pca_test_data$outcome_label
pca_test_data <- predict(train.pca, newdata = pca_test_data[,1:ncol(pca_test_data)])
pca_test_data <- data.frame(pca_test_data[,1:5], outcome_label = pca_test_labs)

################################################################################

# # Log Reg

# full feat
trainData <- full_feat_set[trainRowNumbers,]
testData <- full_feat_set[-trainRowNumbers,]

trainData$outcome_label <- relevel(trainData$outcome_label, ref = 1)
full_feat_multinom_model <- multinom(outcome_label ~ ., data = trainData)
summary(full_feat_multinom_model)
exp(coef(full_feat_multinom_model))

ff_train_pred <- predict(full_feat_multinom_model, newdata = trainData, "class")
ff_test_pred <- predict(full_feat_multinom_model, newdata = testData[,1:(ncol(testData)-1)], "class")

ff_train_probs <- predict(full_feat_multinom_model, newdata = trainData, "probs")
ff_test_probs <- predict(full_feat_multinom_model, newdata = testData[,1:(ncol(testData)-1)], "probs")

cm <- table(testData[,10], ff_test_pred)
confusionMatrix(cm)

plot(Effect("elo_dif",full_feat_multinom_model),multiline=T)
plot(Effect("pstats_mean_dif",full_feat_multinom_model),multiline=T)

################################################################################

# elo feats
trainData <- elo_feat_set[trainRowNumbers,]
testData <- elo_feat_set[-trainRowNumbers,]

trainData$outcome_label <- relevel(trainData$outcome_label, ref = 1)
elo_feat_multinom_model <- multinom(outcome_label ~ ., data = trainData)
summary(elo_feat_multinom_model)
exp(coef(elo_feat_multinom_model))

elo_train_pred <- predict(elo_feat_multinom_model, newdata = trainData, "class")
elo_test_pred <- predict(elo_feat_multinom_model, newdata = testData[,1:(ncol(testData)-1)], "class")

elo_train_probs <- predict(elo_feat_multinom_model, newdata = trainData, "probs")
elo_test_probs <- predict(elo_feat_multinom_model, newdata = testData[,1:(ncol(testData)-1)], "probs")

cm <- table(testData$outcome_label, elo_test_pred)
confusionMatrix(cm)

plot(Effect("elo_dif",elo_feat_multinom_model),multiline=T)

################################################################################

# elo_wr feats
trainData <- elo_wr_pstats_feat_set[trainRowNumbers,]
testData <- elo_wr_pstats_feat_set[-trainRowNumbers,]

trainData$outcome_label <- relevel(trainData$outcome_label, ref = 1)
elo_wr_feat_multinom_model <- multinom(outcome_label ~ ., data = trainData)
summary(elo_wr_feat_multinom_model)
exp(coef(elo_wr_feat_multinom_model))

elo_wr_train_pred <- predict(elo_wr_feat_multinom_model, newdata = trainData, "class")
elo_wr_test_pred <- predict(elo_wr_feat_multinom_model, newdata = testData[,1:(ncol(testData)-1)], "class")

elo_train_probs <- predict(elo_wr_feat_multinom_model, newdata = trainData, "probs")
elo_test_probs <- predict(elo_wr_feat_multinom_model, newdata = testData[,1:(ncol(testData)-1)], "probs")

cm <- table(testData$outcome_label, elo_wr_test_pred)
confusionMatrix(cm)

plot(Effect("elo_dif",elo_wr_feat_multinom_model),multiline=T)

################################################################################

# pca feats

trainData <- pca_train_data
testData <- pca_test_data

trainData$outcome_label <- relevel(trainData$outcome_label, ref = 1)
pca_multinom_model <- multinom(outcome_label ~ ., data = trainData)
summary(pca_multinom_model)
exp(coef(pca_multinom_model))

pca_train_pred <- predict(pca_multinom_model, newdata = trainData, "class")
pca_test_pred <- predict(pca_multinom_model, newdata = testData[,1:(ncol(testData)-1)], "class")

pca_train_probs <- predict(pca_multinom_model, newdata = trainData, "probs")
pca_test_probs <- predict(pca_multinom_model, newdata = testData[,1:(ncol(testData)-1)], "probs")

cm <- table(testData$outcome_label, pca_test_pred)
confusionMatrix(cm)

plot(Effect("PC1",pca_multinom_model),multiline=T)