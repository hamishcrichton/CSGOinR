install.packages('plyr')

library(stringr)
library(plyr)

totalresult <- read.csv("C:\\Users\\dowoo\\OneDrive\\Desktop\\total_results_again.csv", header = TRUE)

#Convert h2h to T1 Win % & total games played

test <- totalresult$head2head # edit to col
test <- str_replace_all(test, " ", "")
test <- substring(test, 1, 5)
t1_wins <- substring(test, 1, 2)
t2_wins <- substring(test, 4, 5)
t1_wins <- sapply(t1_wins, as.numeric)
t2_wins <- sapply(t2_wins, as.numeric)
totalresult$h2htot <- t1_wins + t2_wins
totalresult$t1_win_perc <- t1_wins / totalresult$h2htot

# Convert form into single num val

form_converter <- function(pos, na_val){
  if (pos== "L") return (0)
  if (pos== "W") return (2)
  if (pos== "tie") return (1)
  if (pos== "NA") return (na_val)
} 

form_value <- function(form_as_string, na_val){
  first <- form_converter(substring(form_as_string, 1, 1), na_val)
  second <- form_converter(substring(form_as_string, 2, 2), na_val)
  third <- form_converter(substring(form_as_string, 3, 3), na_val)
  fourth <- form_converter(substring(form_as_string, 4, 4), na_val)
  fifth <- form_converter(substring(form_as_string, 5, 5), na_val)
  
  out = sum(first, second, third, fourth, fifth) 
}

# loop to test different values for na
#t1
for (i in 1:10) {
  na_val <- i / 5
  out <- c(0)
  out[i] <- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team1_form, na_val)
  paste("form_val_", as.character(na_val), sep = "") <- out
}


totalresult$t1_form_na_as_0.2 <- form_val_0.2
totalresult$t1_form_na_as_0.4 <- form_val_0.4
totalresult$t1_form_na_as_0.6 <- form_val_0.6
totalresult$t1_form_na_as_0.8 <- form_val_0.8
totalresult$t1_form_na_as_1 <- form_val_1
totalresult$t1_form_na_as_1.2 <- form_val_1.2
totalresult$t1_form_na_as_1.4 <- form_val_1.4
totalresult$t1_form_na_as_1.6 <- form_val_1.6
totalresult$t1_form_na_as_1.8 <- form_val_1.8
totalresult$t1_form_na_as_2 <- form_val_2

#t2
for (i in 1:10) {
  na_val <- i / 5
  out[i] <- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team2_form, na_val)
  paste("form_val_", as.character(na_val), sep = "") <- out
}

totalresult$t2_form_na_as_0.2 <- form_val_0.2
totalresult$t2_form_na_as_0.4 <- form_val_0.4
totalresult$t2_form_na_as_0.6 <- form_val_0.6
totalresult$t2_form_na_as_0.8 <- form_val_0.8
totalresult$t2_form_na_as_1 <- form_val_1
totalresult$t2_form_na_as_1.2 <- form_val_1.2
totalresult$t2_form_na_as_1.4 <- form_val_1.4
totalresult$t2_form_na_as_1.6 <- form_val_1.6
totalresult$t2_form_na_as_1.8 <- form_val_1.8
totalresult$t2_form_na_as_2 <- form_val_2


# World Rank Num Val

totalresult$wr_1 <- str_squish(totalresult$team1_rank)
totalresult$wr_1 <- str_replace_all(totalresult$wr_1, "World rank: #", "")

totalresult$wr_2 <- str_squish(totalresult$team2_rank)
totalresult$wr_2 <- str_replace_all(totalresult$wr_2, "World rank: #", "")

# Best of
t1_sc <- totalresult$team1_score
t2_sc <- totalresult$team2_score

get_best_of_rds <- function(t1_score, t2_score) {
  if (max(t1_score,t2_score) == 1) {
    if sum(t1_score,t2_score) return (1)
    else return (2)}
  if (max(t1_score,t2_score) == 2) return (3)
  if (max(t1_score,t2_score) == 3) return (5)
  if (max(t1_score,t2_score) == 4) return (7)
  if (max(t1_score,t2_score) == 5) return (9)
  if (max(t1_score,t2_score) > 5) return (1)
  return("0")
}

totalresult$num_best_of <- Vectorize(get_best_of_rds, vectorize.args = c("t1_score", "t2_score"))(t1_sc, t2_sc)

# avg player stats
stats_convert <- function(stats) {
  if (length(stats) == 0){
    return("NA")
  }
  else{  
    check <- str_squish(as.character(stats))  
    check_2 <- strsplit(check, ',')
    check_3 <- sapply(check_2, as.numeric)
    return(check_3)
  }
}


# t1 loop
for (i in 1:nrow(totalresult)){
  
  criteria_t1 <- totalresult$team1[i]
  
  out_mean <- c(0)
  out_med <- c(0)
  
  tmp <- totalresult[i+1:nrow(totalresult),]
  
  sub_df <- subset(tmp, team1 %in% criteria_t1 | team2 %in% criteria_t1)
  
  if (nrow(sub_df)>0){
    if (nrow(sub_df)>5){
      sub_df <- sub_df[1:5,]  
    }
    for (j in 1:nrow(sub_df)){ 
      if (sub_df$team1[j]==criteria_t1){
        # get team1 player stats & concat
        stats <- sub_df$team1_player_stats[j]
        stats <- stats_convert(sub_df$team1_player_stats[j])
        out_mean[j] <- mean(stats)
        out_med[j] <- median(stats)
        
      } else if (sub_df$team2[j]==criteria_t1) {
        # get team2 player stats & concat
        stats <- sub_df[j,'team2_player_stats']
        stats <- stats_convert(sub_df$team2_player_stats[j])
        out_mean[j] <- mean(stats)
        out_med[j] <- median(stats)
      }}}
  totalresult$t1_pstats_mean[i] <- mean(out_mean)
  totalresult$t1_pstats_med[i] <- mean(out_med)
}     

# t2 loop
for (i in 1:nrow(totalresult)){
  
  criteria_t2 <- totalresult$team2[i]
  
  out_mean <- c(0)
  out_med <- c(0)
  
  tmp <- totalresult[i+1:nrow(totalresult),]
  
  sub_df <- subset(tmp, team1 %in% criteria_t2 | team2 %in% criteria_t2)
  
  if (nrow(sub_df)>0){
    if (nrow(sub_df)>5){
      sub_df <- sub_df[1:5,]  
    }
    for (j in 1:nrow(sub_df)){ 
      if (sub_df$team1[j]==criteria_t2){
        # get team1 player stats & concat
        stats <- sub_df$team1_player_stats[j]
        stats <- stats_convert(sub_df$team1_player_stats[j])
        out_mean[j] <- mean(stats)
        out_med[j] <- median(stats)
        
      } else if (sub_df$team2[j]==criteria_t2) {
        # get team2 player stats & concat
        stats <- sub_df[j,'team2_player_stats']
        stats <- stats_convert(sub_df$team2_player_stats[j])
        out_mean[j] <- mean(stats)
        out_med[j] <- median(stats)
      }}}
  
  totalresult$t2_pstats_mean[i] <- mean(out_mean)
  totalresult$t2_pstats_med[i] <- mean(out_med)
}
