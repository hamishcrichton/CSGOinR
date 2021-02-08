# Title     : TODO
# Objective : TODO
# Created by: hamis
# Created on: 10/01/2021
library("elo")
library("vctrs")
library("rvest")
library("magrittr")
library("stringr")
library("dplyr")
library("tidyr")
library("stringr")
library("EloRating")
library("reshape")
library("tidyverse")
library("ggbeeswarm")
library("ggforce")
library("httr")


clean_data <- function (file_loc){

totalresult <- read.csv("test_case.csv", header = TRUE)
##Reformatting the Dataframe
#TODO: reformat this using Regex- esp. August
colnames(totalresult) <- c('Date', 'team1', 'team1_score', 'team2', 'team2_score', 'head2head', 'team1_form', 'team2_form', 'team1_rank', 'team2_rank', 't1_rounds_won_offence', 't2_rounds_won_offence', 't1_rounds_won_defence', 't2_rounds_won_defence', 'team1_player_stats', 'team2_player_stats')
totalresult$Date <- gsub('Results for ','', totalresult$Date)
totalresult$Date <- gsub('st','', totalresult$Date)
totalresult$Date <- gsub('Augu','August', totalresult$Date)
totalresult$Date <- gsub('nd','', totalresult$Date)
totalresult$Date <- gsub('rd','', totalresult$Date)
totalresult$Date <- gsub('th','', totalresult$Date)
totalresult$Date <- gsub('of ','', totalresult$Date)
#write.csv(totalresult, 'total_results_again.csv', row.names = FALSE)
totalresult$Date <- as.Date(totalresult$Date, format= '%d %B %Y')
totalresult <- totalresult[order(totalresult$Date, decreasing = FALSE),]

return(totalresult)
}

new_df <- clean_data()

##WRITE TO CSV
write.csv(totalresult, 'total_results_again.csv', row.names = FALSE)