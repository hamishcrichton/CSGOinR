install.packages('plyr')
install.packages('dplyr')

library(stringr)
library(plyr)
library(dplyr)

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
  if (pos== "T") return (1)
  if (pos== "N") return (na_val)
} 

form_value <- function(form_as_string, na_val){
  first <- form_converter(substring(form_as_string, 1, 1), na_val)
  second <- form_converter(substring(form_as_string, 2, 2), na_val)
  third <- form_converter(substring(form_as_string, 3, 3), na_val)
  fourth <- form_converter(substring(form_as_string, 4, 4), na_val)
  fifth <- form_converter(substring(form_as_string, 5, 5), na_val)
  
  out = sum(first, second, third, fourth, fifth) 
}

totalresult$team1_form <- str_replace_all(totalresult$team1_form, "A", "")
totalresult$team1_form <- str_replace_all(totalresult$team1_form, "ie", "")
totalresult$team2_form <- str_replace_all(totalresult$team2_form, "A", "")
totalresult$team2_form <- str_replace_all(totalresult$team2_form, "ie", "")


# loop to test different values for na
#t1
totalresult$t1_form_na_0.2<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team1_form, 1 / 5)
totalresult$t1_form_na_0.4<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team1_form, 2 / 5)
totalresult$t1_form_na_0.6<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team1_form, 3 / 5)
totalresult$t1_form_na_0.8<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team1_form, 4 / 5)
totalresult$t1_form_na_1<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team1_form, 5 / 5)
totalresult$t1_form_na_1.2<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team1_form, 6 / 5)
totalresult$t1_form_na_1.4<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team1_form, 7 / 5)
totalresult$t1_form_na_1.6<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team1_form, 8 / 5)
totalresult$t1_form_na_1.8<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team1_form, 9 / 5)
totalresult$t1_form_na_2<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team1_form, 10 / 5)

#t2
totalresult$t2_form_na_0.2<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team2_form, 1 / 5)
totalresult$t2_form_na_0.4<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team2_form, 2 / 5)
totalresult$t2_form_na_0.6<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team2_form, 3 / 5)
totalresult$t2_form_na_0.8<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team2_form, 4 / 5)
totalresult$t2_form_na_1<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team2_form, 5 / 5)
totalresult$t2_form_na_1.2<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team2_form, 6 / 5)
totalresult$t2_form_na_1.4<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team2_form, 7 / 5)
totalresult$t2_form_na_1.6<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team2_form, 8 / 5)
totalresult$t2_form_na_1.8<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team2_form, 9 / 5)
totalresult$t2_form_na_2<- Vectorize(form_value, vectorize.args = c("form_as_string", "na_val"))(totalresult$team2_form, 10 / 5)


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

# outcome label
create_outcome_label <- function(t1_score, t2_score) {
  if (t1_score > t2_score) return (1)
  else if (t1_score < t2_score) return (2)
  else return (0)  
}

totalresult$outcome_label <- Vectorize(create_outcome_label, vectorize.args = c("t1_score", "t2_score"))(t1_sc, t2_sc)

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

totalresult$pstats_mean_dif <- totalresult$t1_pstats_mean - totalresult$t2_pstats_mean
totalresult$pstats_med_dif <- totalresult$t1_pstats_med - totalresult$t2_pstats_med

totalresult$form_na_1_dif <- totalresult$t1_form_na_0.2 - totalresult$t2_form_na_0.2
totalresult$form_na_2_dif <- totalresult$t1_form_na_0.4 - totalresult$t2_form_na_0.4
totalresult$form_na_3_dif <- totalresult$t1_form_na_0.6 - totalresult$t2_form_na_0.6
totalresult$form_na_4_dif <- totalresult$t1_form_na_0.8 - totalresult$t2_form_na_0.8
totalresult$form_na_5_dif <- totalresult$t1_form_na_1 - totalresult$t2_form_na_1
totalresult$form_na_6_dif <- totalresult$t1_form_na_1.2 - totalresult$t2_form_na_1.2
totalresult$form_na_7_dif <- totalresult$t1_form_na_1.4 - totalresult$t2_form_na_1.4
totalresult$form_na_8_dif <- totalresult$t1_form_na_1.6 - totalresult$t2_form_na_1.6
totalresult$form_na_9_dif <- totalresult$t1_form_na_1.8 - totalresult$t2_form_na_1.8
totalresult$form_na_10_dif <- totalresult$t1_form_na_2 - totalresult$t2_form_na_2


out_df <- select(totalresult, -c(head2head, 
                                 team1_form, 
                                 team2_form, 
                                 team1_rank, 
                                 team2_rank, 
                                 t1_rounds_won_offence, 
                                 t2_rounds_won_offence, 
                                 t1_rounds_won_defence,
                                 t2_rounds_won_defence,
                                 team1_player_stats,
                                 team2_player_stats,
                                 t1_pstats_mean,
                                 t2_pstats_mean,
                                 t1_pstats_med,
                                 t2_pstats_med,
                                 t1_form_na_0.2,
                                 t1_form_na_0.4,
                                 t1_form_na_0.6,
                                 t1_form_na_0.8,
                                 t1_form_na_1,
                                 t1_form_na_1.2,
                                 t1_form_na_1.4,
                                 t1_form_na_1.6,
                                 t1_form_na_1.8,
                                 t1_form_na_2,
                                 t2_form_na_0.2,
                                 t2_form_na_0.4,
                                 t2_form_na_0.6,
                                 t2_form_na_0.8,
                                 t2_form_na_1,
                                 t2_form_na_1.2,
                                 t2_form_na_1.4,
                                 t2_form_na_1.6,
                                 t2_form_na_1.8,
                                 t2_form_na_2))

write.csv(out_df,"C:\\Users\\dowoo\\OneDrive\\Desktop\\scrape_transform_csgo_results.csv", row.names = FALSE)
