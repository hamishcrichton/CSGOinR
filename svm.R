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

# # SVM

# full feat

trainData <- full_feat_set[trainRowNumbers,]
testData <- full_feat_set[-trainRowNumbers,]

full_feat_svm <- svm(outcome_label~., data=trainData, 
                     method="C-classification", kernal="radial", 
                     gamma=0.1, cost=10)

tune_obj <- tune(svm, outcome_label~., data=trainData, #tuning object tests different cost and gamma values via gridsearch
                 ranges = list(gamma=c(0.1, 1, 10), cost = 10^(-1:2)),
                 tunecontrol = tune.control(sampling = "fix"))

summary(tune_obj)
plot(tune_obj)

summary(full_feat_svm)

plot(full_feat_svm, trainData, elo_dif ~ wr_dif)

svm_preds <- predict(full_feat_svm, testData)
cm <- table(testData$outcome_label, svm_preds)
confusionMatrix(cm)

################################################################################

# elo feats
trainData <- elo_feat_set[trainRowNumbers,]
testData <- elo_feat_set[-trainRowNumbers,]

elo_feat_svm <- svm(outcome_label~., data=trainData, 
                     method="C-classification", kernal="radial", 
                     gamma=0.1, cost=10)

elo_tune_obj <- tune(svm, outcome_label~., data=trainData,  #tuning object tests different cost and gamma values via gridsearch
                 ranges = list(gamma=c(0.1, 1, 10), cost = 10^(-1:2)),
                 tunecontrol = tune.control(sampling = "fix"))

summary(elo_tune_obj)
plot(elo_tune_obj)

summary(elo_feat_svm)

plot(elo_feat_svm, trainData, elo_dif ~ elo_t1od_dif)

svm_preds <- predict(elo_feat_svm, testData)
cm <- table(testData$outcome_label, svm_preds)
confusionMatrix(cm)

################################################################################

# elo_wr feats
trainData <- elo_wr_pstats_feat_set[trainRowNumbers,]
testData <- elo_wr_pstats_feat_set[-trainRowNumbers,]

elo_wr_feat_svm <- svm(outcome_label~., data=trainData, 
                    method="C-classification", kernal="radial", 
                    gamma=0.1, cost=10)

elo_wr_tune_obj <- tune(svm, outcome_label~., data=trainData,  #tuning object tests different cost and gamma values via gridsearch
                     ranges = list(gamma=c(0.1, 1, 10), cost = 10^(-1:2)),
                     tunecontrol = tune.control(sampling = "fix"))

summary(elo_wr_tune_obj)
plot(elo_wr_tune_obj)

summary(elo_wr_feat_svm)

plot(elo_wr_feat_svm, trainData, elo_dif ~ wr_dif)

svm_preds <- predict(elo_wr_feat_svm, testData)
cm <- table(testData$outcome_label, svm_preds)
confusionMatrix(cm)

################################################################################

# pca feats

trainData <- pca_train_data
testData <- pca_test_data

pca_feat_svm <- svm(outcome_label~., data=trainData, 
                       method="C-classification", kernal="radial", 
                       gamma=0.1, cost=10)

pca_tune_obj <- tune(svm, outcome_label~., data=trainData,  #tuning object tests different cost and gamma values via gridsearch
                        ranges = list(gamma=c(0.1, 1, 10), cost = 10^(-1:2)),
                        tunecontrol = tune.control(sampling = "fix"))

summary(pca_tune_obj)
plot(pca_tune_obj)

summary(pca_feat_svm)

plot(pca_feat_svm, trainData, PC1 ~ PC2)

svm_preds <- predict(pca_feat_svm, testData)
cm <- table(testData$outcome_label, svm_preds)
confusionMatrix(cm)
