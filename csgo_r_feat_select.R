library(corrplot)
library(factoextra)
library(mlbench)
library(caret)
library(FSinR)
library(randomForest)

set.seed(7)

orig_data <- read.csv("too_many_columns.csv", header = TRUE)

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
                                              ))

# drop outcomes for PCA non cat data
pca_data <- subset(non_cat_data, select = -c(outcome_label))

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

fit<- randomForest(factor(non_cat_data[,11])~., data=non_cat_data) #fit randomforest
importance <- varImp(fit)
print(importance)
plot(importance)
varImpPlot(fit,type=2)

#################################################################

# # RFE

control <- rfeControl(functions=rfFuncs, method="cv", number=3) # define the control using a random forest selection function
results <- rfe(non_cat_data[,1:10], non_cat_data[,11], sizes=c(1:11), rfeControl=control)
print(results)
predictors(results) # list the chosen features
plot(results, type=c("g", "o"))

#################################################################

# # feat var

feature_variance <- caret::nearZeroVar(non_cat_data, saveMetrics = TRUE) #caret version
feature_variance
# freqRatio: This is the ratio of the percentage frequency for the most common value over the second most common value.
# percentUnique: This is the number of unique values divided by the total number of samples multiplied by 100.

#################################################################

# # selectkbest

filter_evaluator <- filterEvaluator('determinationCoefficient') #see https://rdrr.io/cran/FSinR/man/filterEvaluator.html for options
skb_direct_search <- selectKBest(k=(ncol(non_cat_data)-1)) # set k to num of feats you want, will allocate bool flag
skb_direct_search(non_cat_data, 'outcome_label', filter_evaluator)

#################################################################

# # Create & Save Feature Sets
main_data <- subset(non_cat_data, select = -c(pstats_med_dif))

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

write.csv(main_data, 'full_feature_set_for_models.csv', row.names = FALSE)
write.csv(elo_feat_set, 'elo_feature_set_for_models.csv', row.names = FALSE)
write.csv(elo_wr_pstats_feat_set, 'elo_wr_pstats_feature_set_for_models.csv', row.names = FALSE)
