library(mlbench)
library(caret)

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

# # Naive Bayes

# full feat

trainData <- full_feat_set[trainRowNumbers,]
testData <- full_feat_set[-trainRowNumbers,]

ff_bayes_classif <- naiveBayes(outcome_label ~ ., data = trainData)
ff_bayes_classif 
y_pred <- predict(ff_bayes_classif, newdata = testData) 

cm <- table(testData$outcome_label, y_pred) 
confusionMatrix(cm)

################################################################################

# elo feats

trainData <- elo_feat_set[trainRowNumbers,]
testData <- elo_feat_set[-trainRowNumbers,]

elo_bayes_classif <- naiveBayes(outcome_label ~ ., data = trainData)
elo_bayes_classif 
y_pred <- predict(elo_bayes_classif, newdata = testData) 

cm <- table(testData$outcome_label, y_pred) 
confusionMatrix(cm)

################################################################################

# elo_wr feats

trainData <- elo_wr_pstats_feat_set[trainRowNumbers,]
testData <- elo_wr_pstats_feat_set[-trainRowNumbers,]

elo_wr_bayes_classif <- naiveBayes(outcome_label ~ ., data = trainData)
elo_wr_bayes_classif
y_pred <- predict(elo_wr_bayes_classif, newdata = testData) 

cm <- table(testData$outcome_label, y_pred) 
confusionMatrix(cm)

################################################################################

# pca feats

trainData <- pca_train_data
testData <- pca_test_data

pca_bayes_classif <- naiveBayes(outcome_label ~ ., data = trainData)
pca_bayes_classif
y_pred <- predict(pca_bayes_classif, newdata = testData) 

cm <- table(testData$outcome_label, y_pred) 
confusionMatrix(cm)
