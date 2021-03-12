# Title     : TODO
# Objective : TODO
# Created by: Hamish
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

#Read in the data
if (!exists('totalresult')) {
 totalresult <- read.csv("elo_frame.csv", header = TRUE)
 totalresult <- as.data.frame.matrix(totalresult)}

#ELO rating process
seqcheck(winner = totalresult$winner, loser = totalresult$loser, Date = totalresult$Date)
res <- elo.seq(winner = totalresult$winner, loser = totalresult$loser, Date = totalresult$Date, draw = totalresult$Draw, runcheck = FALSE, k=42)

#HYPERPARAMETER OPTIMISATION
optimise_for_k <- function(elo.seq_output) {
ores <- optimizek(elo.seq_output, krange = c(10, 500), resolution = 491)

plot(ores$complete$k, ores$complete$loglik, type = "l", las = 1, xlab = bquote(italic(k)), ylab = "log likelihood")
abline(v = ores$best$k, col = "red")

return(ores$best)
}


if (!exists('scrape_transform')) {
 scrape_transform <- read.csv("scrape_transform_csgo_results.csv", header = TRUE)
 scrape_transform <- as.data.frame.matrix(scrape_transform)}

scrape_transform$Date <- as.Date(scrape_transform$Date)

#Extract ELO for both teams for the date of their match
scrape_transform$team1_ELO <- extract_elo(res, extractdate = scrape_transform$Date, IDs = scrape_transform$team1)
scrape_transform$team2_ELO <- extract_elo(res, extractdate = scrape_transform$Date, IDs = scrape_transform$team2)

#Create helper variables to identify Offense/Defence IDs
scrape_transform$team1_o <- paste0("o_", scrape_transform$team1)
scrape_transform$team1_d <- paste0("d_", scrape_transform$team1)
scrape_transform$team2_o <- paste0("o_", scrape_transform$team2)
scrape_transform$team2_d <- paste0("d_", scrape_transform$team2)

#Extract Offense/ Defence ELOs for both teams on the date of their match
scrape_transform$team1_elo_o <- extract_elo(res, extractdate = scrape_transform$Date, IDs = scrape_transform$team1_o)
scrape_transform$team1_elo_d <- extract_elo(res, extractdate = scrape_transform$Date, IDs = scrape_transform$team1_d)
scrape_transform$team2_elo_o <- extract_elo(res, extractdate = scrape_transform$Date, IDs = scrape_transform$team2_o)
scrape_transform$team2_elo_d <- extract_elo(res, extractdate = scrape_transform$Date, IDs = scrape_transform$team2_d)

#elo difs
orig_data$elo_dif <- orig_data$team1_ELO - orig_data$team2_ELO
orig_data$elo_t1od_dif <- orig_data$team1_elo_o - orig_data$team2_elo_d
orig_data$elo_t1do_dif <- orig_data$team1_elo_d - orig_data$team2_elo_o

scrape_transform <- subset(scrape_transform, select = -c(team1_ELO,
                                                         team2_ELO,
                                                         team1_elo_o,
                                                         team2_elo_d,
                                                         team1_elo_d,
                                                         team2_elo_o))

#Save the result down
write.csv(scrape_transform, 'too_many_columns.csv', row.names = FALSE)

#Extract elo values for participants at the start of the tournament
#pre_tournament_elo <- extract_elo(res, extractdate = "2020-12-07", IDs = c("mousesports", "OG", "G2", "FURIA", "Natus_Vincere", "BIG", "Astralis", "Vitality"))
#summary(res)

#write.csv(pre_tournament_elo, 'pre_tournament_elo.csv', row.names = TRUE)
#eloplot <- eloplot(eloobject = res, ids = c("mousesports", "OG", "G2", "FURIA", "Natus_Vincere", "BIG", "Astralis", "Vitality"), from = "2018-01-01", to = "2020-12-07")
