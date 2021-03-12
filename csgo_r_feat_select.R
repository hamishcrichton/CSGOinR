library(corrplot)
library(factoextra)
library(mlbench)
library(caret)
library(FSinR)
library(randomForest)

set.seed(7)

orig_data <- read.csv("C:\\Users\\dowoo\\OneDrive\\Desktop\\too_many_columns.csv", header = TRUE)

# # datasets

orig_data <- orig_data[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 29, 30, 31, 32, 28)]

#non cat
non_cat_data <- subset(orig_data, select = -c(Date,
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

# drop outcomes for PCA non cat data
pca_data <- subset(non_cat_data, select = -c(outcome_label))

# drop names & scores for RFE
rfe_data <- subset(orig_data, select = -c(Date,
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

#################################################################

# # correlation plot

col<- colorRampPalette(c("blue", "white", "red"))(20)
cormat<-rquery.cormat(non_cat_data, type="full", col=col)

#################################################################

# # PCA

res.pca <- prcomp(pca_data, scale = TRUE)

fviz_eig(res.pca) # plot var explained

fviz_pca_var(res.pca, # group correlated vars
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#################################################################

# # feat importance

fit<- randomForest(factor(rfe_data[,11])~., data=rfe_data) #fit randomforest
importance <- varImp(fit)
print(importance)
plot(importance)
varImpPlot(fit,type=2)

#################################################################

# # RFE

control <- rfeControl(functions=rfFuncs, method="cv", number=3) # define the control using a random forest selection function
results <- rfe(rfe_data[,1:10], rfe_data[,11], sizes=c(1:11), rfeControl=control)
print(results)
predictors(results) # list the chosen features
plot(results, type=c("g", "o"))

#################################################################

# # feat var

feature_variance <- caret::nearZeroVar(rfe_data, saveMetrics = TRUE) #caret version
feature_variance
# freqRatio: This is the ratio of the percentage frequency for the most common value over the second most common value.
# percentUnique: This is the number of unique values divided by the total number of samples multiplied by 100.

#################################################################

# # selectkbest

filter_evaluator <- filterEvaluator('determinationCoefficient') #see https://rdrr.io/cran/FSinR/man/filterEvaluator.html for options
skb_direct_search <- selectKBest(k=(ncol(rfe_data)-1)) # set k to num of feats you want, will allocate bool flag
skb_direct_search(rfe_data, 'outcome_label', filter_evaluator)
