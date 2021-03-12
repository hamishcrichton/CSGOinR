library(mlbench)
library(caret)
library(FSinR)

library(rpart)
library(party)
library(randomForest)

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

library(e1071)
library(caTools)
library(effects)

set.seed(7)

orig_data <- read.csv("C:\\Users\\dowoo\\OneDrive\\Desktop\\too_many_columns.csv", header = TRUE)

# # datasets

# some amends
# wr
orig_data$wr_dif <- orig_data$wr_1 - orig_data$wr_2

#elo difs
orig_data$elo_dif <- orig_data$team1_ELO - orig_data$team2_ELO
orig_data$elo_t1od_dif <- orig_data$team1_elo_o - orig_data$team2_elo_d
orig_data$elo_t1do_dif <- orig_data$team1_elo_d - orig_data$team2_elo_o

orig_data <- orig_data[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 30, 31, 32, 28)]

orig_data$outcome_label <- as.factor(orig_data$outcome_label)

# drop names & scores for RFE
main_data <- subset(orig_data, select = -c(Date,
                                           team1,
                                           team1_score,
                                           team2,
                                           team2_score,
                                           t1_rounds_won_offence,
                                           t2_rounds_won_offence,
                                           t1_rounds_won_defence,
                                           t2_rounds_won_defence,
                                           team1_o,
                                           team1_d,
                                           team2_o,
                                           team2_d,
                                           team1_ELO,
                                           team2_ELO,
                                           team1_elo_o,
                                           team1_elo_d,
                                           team2_elo_d,
                                           team2_elo_o,
                                           wr_1,
                                           wr_2,
                                           pstats_med_dif))


full_feat_set <- main_data
elo_feat_set <- subset(main_data, select = c(elo_dif,
                                             elo_t1od_dif,
                                             elo_t1do_dif,
                                             outcome_label))

elo_wr_pstats_feat_set <- subset(main_data, select = c(elo_dif,
                                                       elo_t1od_dif,
                                                       elo_t1do_dif,
                                                       wr_dif,
                                                       pstats_mean_dif,
                                                       outcome_label))


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


# # Naive Bayes

# full feat
trainData <- full_feat_set[trainRowNumbers,]
testData <- full_feat_set[-trainRowNumbers,]

ff_bayes_classif <- naiveBayes(outcome_label ~ ., data = trainData)
ff_bayes_classif 
y_pred <- predict(ff_bayes_classif, newdata = testData) 

ff_cm <- table(testData$outcome_label, y_pred) 
ff_cm 
confusionMatrix(ff_cm)

################################################################################

# elo feats
trainData <- elo_feat_set[trainRowNumbers,]
testData <- elo_feat_set[-trainRowNumbers,]

elo_bayes_classif <- naiveBayes(outcome_label ~ ., data = trainData)
elo_bayes_classif 
y_pred <- predict(elo_bayes_classif, newdata = testData) 

elo_cm <- table(testData$outcome_label, y_pred) 
elo_cm 
confusionMatrix(elo_cm)

################################################################################

# elo_wr feats
trainData <- elo_wr_pstats_feat_set[trainRowNumbers,]
testData <- elo_wr_pstats_feat_set[-trainRowNumbers,]

elo_wr_bayes_classif <- naiveBayes(outcome_label ~ ., data = trainData)
elo_wr_bayes_classif
y_pred <- predict(elo_wr_bayes_classif, newdata = testData) 

elo_wr_cm <- table(testData$outcome_label, y_pred) 
elo_wr_cm 
confusionMatrix(elo_wr_cm)

################################################################################

# pca feats

trainData <- pca_train_data
testData <- pca_test_data

pca_bayes_classif <- naiveBayes(outcome_label ~ ., data = trainData)
pca_bayes_classif
y_pred <- predict(pca_bayes_classif, newdata = testData) 

pca_cm <- table(testData$outcome_label, y_pred) 
pca_cm 
confusionMatrix(pca_cm)
