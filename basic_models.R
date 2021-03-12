install.packages("party")
install.packages("caTools") 
install.packages("effects") 

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

#non cat pca data
pca_data <- subset(orig_data, select = -c(Date,
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
                                              pstats_med_dif,
                                              outcome_label))


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
                                          wr_2))

res.pca <- prcomp(pca_data, scale = TRUE)

full_feat_set <- subset(rfe_data, select = -c(pstats_med_dif))
elo_feat_set <- subset(rfe_data, select = c(elo_dif,
                                            elo_t1od_dif,
                                            elo_t1do_dif,
                                            outcome_label))

elo_wr_pstats_feat_set <- subset(rfe_data, select = c(elo_dif,
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
#res.pca <- res.pca[shuffle_index,]


trainRowNumbers <- createDataPartition(full_feat_set$outcome_label, p=0.8, list=FALSE)
trainData <- full_feat_set[trainRowNumbers,]
testData <- full_feat_set[-trainRowNumbers,]
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

dt_full_feat_2 <- train(
  outcome_label ~., data = trainData, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(dt_full_feat_2)

# to replicate for other sets, change train and test data then repeat code

# # Random Forest


dt_rf <- randomForest(outcome_label ~ .,data=trainData)
dt_rf
dt_pred <- predict(dt_rf, newdata=testData[,1:(ncol(testData)-1)])
dt_pred
y
cm <- table(observed=testData[,10], predicted=dt_pred)
cm

# # Log Reg
trainData$outcome_label <- relevel(trainData$outcome_label, ref = 1)
multinom_model <- multinom(outcome_label ~ ., data = trainData)
summary(multinom_model)
exp(coef(multinom_model))
head(round(fitted(multinom_model), 2))

train_pred <- predict(multinom_model, newdata = trainData, "class")
test_pred <- predict(multinom_model, newdata = testData[,1:(ncol(testData)-1)], "class")

train_probs <- predict(multinom_model, newdata = trainData, "probs")
test_probs <- predict(multinom_model, newdata = testData[,1:(ncol(testData)-1)], "probs")

tab <- table(trainData$outcome_label, train_pred)
round((sum(diag(tab))/sum(tab))*100,2)
tab <- table(testData[,10], test_pred)
round((sum(diag(tab))/sum(tab))*100,2)

#plot(NA, xlim = c(min(testData$elo_dif), max(testData$elo_dif)), ylim = c(0, 1), xlab = "elo_dif", ylab = "Predicted Probability")
#lines(testData$elo_dif, test_probs[, 1], col = "red", lwd = 2)
#lines(testData$elo_dif, test_probs[, 2], col = "blue", lwd = 2)
#lines(testData$elo_dif, test_probs[, 3], col = "green", lwd = 2)
## some text labels help clarify things:
#text(9, 0.75, "y==0", col = "red")
#text(6, 0.4, "y==2", col = "green")
#text(5, 0.15, "y==1", col = "blue")

#plot(NA, xlim = c(min(elo_dif), max(elo_dif)), ylim = c(0, 1), xlab = "elo_dif", ylab = "Predicted Probability", 
#     bty = "l")
# polygons
#polygon(c(testData$elo_dif, rev(testData$elo_dif)), c(test_probs[, 1], rep(0, nrow(test_probs))), col = rgb(1, 
                                                                              0, 0, 0.3), border = rgb(1, 0, 0, 0.3))
#polygon(c(testData$elo_dif, rev(testData$elo_dif)), c(test_probs[, 2], rep(0, nrow(test_probs))), col = rgb(0, 
                                                                              0, 1, 0.3), border = rgb(0, 0, 1, 0.3))
#polygon(c(testData$elo_dif, rev(testData$elo_dif)), c(test_probs[, 3], rep(0, nrow(test_probs))), col = rgb(0, 
                                                                              1, 0, 0.3), border = rgb(0, 1, 0, 0.3))
# text labels
#text(9, 0.4, "y=0", font = 2)
#text(2.5, 0.4, "y=1", font = 2)
#text(-1.5, 0.4, "y=2", font = 2)
# optionally highlight predicted class:
#lines(testData$elo_dif[train_probs == 0], rep(0, sum(train_probs == 0)), col = "red", lwd = 3)
#lines(testData$elo_dif[train_probs == 1], rep(0, sum(train_probs == 1)), col = "blue", lwd = 3)
#lines(testData$elo_dif[train_probs == 2], rep(0, sum(train_probs == 2)), col = "green", lwd = 3)


plot(Effect("elo_dif",multinom_model),multiline=T)
plot(Effect("pstats_mean_dif",multinom_model),multiline=T)

# # SVM 

full_feat_svm <- svm(outcome_label~., data=trainData, 
            method="C-classification", kernal="radial", 
            gamma=0.1, cost=10)

tune_obj <- tune(svm, outcome_label~., data=trainData, 
                     ranges = list(gamma=c(0.1, 1, 10), cost = 10^(-1:2)),
                     tunecontrol = tune.control(sampling = "fix"))

summary(tune_obj)
plot(tune_obj)

summary(full_feat_svm)

plot(full_feat_svm, trainData, elo_dif ~ wr_dif)

svm_preds <- predict(full_feat_svm, testData)
xtab <- table(testData$outcome_label, svm_preds)
xtab
round((sum(diag(xtab))/sum(xtab))*100,2)

# Naive Bayes
classifier_cl <- naiveBayes(outcome_label ~ ., data = trainData)
classifier_cl 
y_pred <- predict(classifier_cl, newdata = testData) 

cm <- table(testData$outcome_label, y_pred) 
cm 
confusionMatrix(cm)

# to do - mix up feature sets, graphs, figure out how to access PCs. 