install.packages('PerformanceAnalytics')
install.packages('corrplot')
install.packages("factoextra")
install.packages("mlbench")
install.packages("caret")
install.packages("FSinR")

library(PerformanceAnalytics)
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")

library(factoextra)

library(mlbench)
library(caret)
library(FSinR)

set.seed(7)

orig_data <- read.csv("C:\\Users\\dowoo\\OneDrive\\Desktop\\too_many_columns.csv", header = TRUE)

# # datasets

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
                                              team2_d))

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
                                          team2_d))

#correlation plot
col<- colorRampPalette(c("blue", "white", "red"))(20)
cormat<-rquery.cormat(non_cat_data, type="full", col=col)

# PCA
res.pca <- prcomp(pca_data, scale = TRUE)

# plot var explained
fviz_eig(res.pca)

# group correlated vars
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# feat importance
require(randomForest)
fit<- randomForest(factor(rfe_data[,15])~., data=rfe_data))
importance <- varImp(fit)
print(importance)
plot(importance)
varImpPlot(fit,type=2)

#RFE
control <- rfeControl(functions=rfFuncs, method="cv", number=3) # define the control using a random forest selection function
results <- rfe(rfe_data[,1:14], rfe_data[,15], sizes=c(1:15), rfeControl=control)
print(results)
predictors(results) # list the chosen features
plot(results, type=c("g", "o"))

#feat var
var(rfe_data) # default package
feature_variance <- caret::nearZeroVar(rfe_data, saveMetrics = TRUE) #caret version
feature_variance
# freqRatio: This is the ratio of the percentage frequency for the most common value over the second most common value.
# percentUnique: This is the number of unique values divided by the total number of samples multiplied by 100.

# selectkbest
filter_evaluator <- filterEvaluator('determinationCoefficient') #see https://rdrr.io/cran/FSinR/man/filterEvaluator.html for options
skb_direct_search <- selectKBest(k=(ncol(rfe_data)-1))
skb_direct_search(rfe_data, 'outcome_label', filter_evaluator)
