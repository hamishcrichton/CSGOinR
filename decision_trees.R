library(mlbench)
library(caret)
library(FSinR)
library(rpart)
library(party)
library(randomForest)

library(e1071)
library(caTools)
library(effects)

set.seed(7)

orig_data <- read.csv("too_many_columns.csv", header = TRUE)

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

################################################################################

# # DT

# full feat
dt_full_feat <- rpart(outcome_label ~., data = trainData, method = "class")
par(xpd = NA)
plot(dt_full_feat)
text(dt_full_feat, digits = 3)
print(dt_full_feat, digits = 2)

predicted.classes <- dt_full_feat %>% 
  predict(testData, type = "class")

mean(predicted.classes == testData$outcome_label)

cm <- table(testData$outcome_label, predicted.classes)
confusionMatrix(cm)

dt_full_feat_2 <- train(
  outcome_label ~., data = trainData, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(dt_full_feat_2)

################################################################################

# elo feats
trainData <- elo_feat_set[trainRowNumbers,]
testData <- elo_feat_set[-trainRowNumbers,]

dt_elo_feat <- rpart(outcome_label ~., data = trainData, method = "class")
par(xpd = NA)
plot(dt_elo_feat)
text(dt_elo_feat, digits = 3)
print(dt_elo_feat, digits = 2)

predicted.classes <- dt_elo_feat %>% 
  predict(testData, type = "class")

mean(predicted.classes == testData$outcome_label)

cm <- table(testData$outcome_label, predicted.classes)
confusionMatrix(cm)

dt_elo_feat_2 <- train(
  outcome_label ~., data = trainData, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(dt_elo_feat_2)

################################################################################

# elo_wr feats
trainData <- elo_wr_pstats_feat_set[trainRowNumbers,]
testData <- elo_wr_pstats_feat_set[-trainRowNumbers,]

dt_elowr_feat <- rpart(outcome_label ~., data = trainData, method = "class")
par(xpd = NA)
plot(dt_elowr_feat)
text(dt_elowr_feat, digits = 3)
print(dt_elowr_feat, digits = 2)

predicted.classes <- dt_elowr_feat %>% 
  predict(testData, type = "class")

mean(predicted.classes == testData$outcome_label)

cm <- table(testData$outcome_label, predicted.classes)
confusionMatrix(cm)

dt_elowr_feat_2 <- train(
  outcome_label ~., data = trainData, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(dt_elowr_feat_2)

################################################################################

# pca feats

trainData <- pca_train_data
testData <- pca_test_data

dt_pca_feat <- rpart(outcome_label ~., data = trainData, method = "class")
par(xpd = NA)
plot(dt_pca_feat)
text(dt_pca_feat, digits = 3)
print(dt_pca_feat, digits = 2)

predicted.classes <- dt_pca_feat %>% 
  predict(testData, type = "class")

mean(predicted.classes == testData$outcome_label)

cm <- table(testData$outcome_label, predicted.classes)
confusionMatrix(cm)

dt_pca_feat_2 <- train(
  outcome_label ~., data = trainData, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(dt_pca_feat_2)