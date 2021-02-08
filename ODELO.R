# Title     : TODO
# Objective : TODO
# Created by: hamis
# Created on: 29/01/2021
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

#elo_frame <- read.csv("elo_frame.csv", header = TRUE)
#elo_frame <- as.data.frame.matrix(elo_frame)
#elo_frame <- elo_frame[order(elo_frame$Date, decreasing = FALSE),]

#ELO rating process
#seqcheck(winner = elo_frame$winner, loser = elo_frame$loser, Date = elo_frame$Date, draw = elo_frame$Draw)
#res <- elo.seq(winner = elo_frame$winner, loser = elo_frame$loser, Date = elo_frame$Date, draw = elo_frame$Draw, runcheck=FALSE)

elo_total_frame <- read.csv("elo_total_frame.csv", header = TRUE)
elo_total_frame <- as.data.frame.matrix(elo_total_frame)
elo_total_frame <- elo_total_frame[order(elo_total_frame$Date, decreasing = FALSE),]


seqcheck(winner = elo_total_frame$winner, loser = elo_total_frame$loser, Date = elo_total_frame$Date, draw = elo_total_frame$Draw)
rest <- elo.seq(winner = elo_total_frame$winner, loser = elo_total_frame$loser, Date = elo_total_frame$Date, draw = elo_total_frame$Draw, runcheck=FALSE)

#Extract elo values for participants at the start of the tournament
pre_tournament_elo <- extract_elo(rest, extractdate = "2020-12-07", IDs = c("mousesports", "OG", "G2", "FURIA", "Natus_Vincere", "BIG", "Astralis", "Vitality"))
summary(rest)