# Title     : Applying machine learning to eSports
# Objective : Beat the bookies: predict each match result from BLAST Premier Global Finals 2020
# Created by: hamis
# Created on: 16/12/2020


#Manage packages
if (!("elo" %in% installed.packages())) {
 install.packages("elo")
}
if (!("vctrs" %in% installed.packages())) {
 install.packages("vctrs")
}
if (!("rvest" %in% installed.packages())) {
 install.packages("rvest")
}
if (!("magrittr" %in% installed.packages())) {
 install.packages("magrittr")
}
if (!("stringr" %in% installed.packages())) {
 install.packages("stringr")
}
if (!("dplyr" %in% installed.packages())) {
 install.packages("dplyr")
}
if (!("tidyr" %in% installed.packages())) {
 install.packages("tidyr")
}
if (!("stringr" %in% installed.packages())) {
 install.packages("stringr")
}
if (!("EloRating" %in% installed.packages())) {
 install.packages("EloRating")
}
if (!("reshape" %in% installed.packages())) {
 install.packages("reshape")
}

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


#starting url
url <- "https://www.hltv.org/results"


### SCRAPING FUNCTIONS

#Return Date, winner, loser
scrape_Dwl_one_page <- function (s_tree) {

day_results_nodes <- s_tree %>%
 html_nodes(xpath="//div[contains(@class, 'results-sublist')]")
for (j in seq_along(day_results_nodes)) {
 i <- day_results_nodes[[j]]
 date <- i %>%
  html_node('.standard-headline') %>%
  html_text()

 winner <- i %>%
  html_nodes('.team-won') %>%
  html_text()

 loser <- i %>%
  html_nodes('.team:not(.team-won)') %>%
  html_text()

 gameresult <- cbind(Date = date, winner, loser)

 if (exists("totalresult")) {
  totalresult <- rbind(totalresult, gameresult)
 } else {
  totalresult <- gameresult
 }
}
return(totalresult)
}


#TODO improve the 'while' to incorporate a try; write this as a function.
#s <- rvest::html_session(url)
#s_tree <- xml2::read_html(s)
#totalresult <- scrape_Dwl_one_page(s_tree)
#
#while (TRUE) {
# s <- s %>% follow_link(xpath = '//div[1]/div[2]/div[2]/a[2]')
# s_tree <- xml2::read_html(s)
# totalresult <- scrape_Dwl_one_page(s_tree)
#}

#totalresult <- as.data.frame(totalresult)

#Single truth for data
#write.csv(totalresult, 'gold_standard.csv', row.names = FALSE)



#totalresult <- read.csv("C:\\Users\\hamis\\PycharmProjects\\CSGOinR\\gold_standard.csv", header = TRUE)
##Reformatting the Dataframe
#TODO: reformat this using Regex- esp. August
#totalresult$Date <- gsub('Results for ','', totalresult$Date)
#totalresult$Date <- gsub('st','', totalresult$Date)
#totalresult$Date <- gsub('Augu','August', totalresult$Date)
#totalresult$Date <- gsub('nd','', totalresult$Date)
#totalresult$Date <- gsub('rd','', totalresult$Date)
#totalresult$Date <- gsub('th','', totalresult$Date)
##write.csv(totalresult, 'total_results_again.csv', row.names = FALSE)
#totalresult$Date <- as.Date(totalresult$Date, format= '%B %d %Y')
#totalresult <- totalresult[order(totalresult$Date, decreasing = FALSE),]
#

##WRITE TO CSV
#write.csv(totalresult, 'total_results_again.csv', row.names = FALSE)



#Read in the data
if (!exists('totalresult')) {
 totalresult <- read.csv("C:\\Users\\hamis\\PycharmProjects\\CSGOinR\\total_results_again.csv", header = TRUE)
 totalresult <- as.data.frame.matrix(totalresult)
 totalresult$winner <- gsub('Natus Vincere','Natus_Vincere', totalresult$winner)
 totalresult$loser <- gsub('Natus Vincere','Natus_Vincere', totalresult$loser)


 #ELO rating process
 seqcheck(winner = totalresult$winner, loser = totalresult$loser, Date = totalresult$Date)
 res <- elo.seq(winner = totalresult$winner, loser = totalresult$loser, Date = totalresult$Date, runcheck = FALSE)
 pre_tournament_elo <- extract_elo(res, extractdate = "2020-12-07", IDs = c("mousesports", "OG", "G2", "FURIA", "Natus_Vincere", "BIG", "Astralis", "Vitality"))
 summary(res)

 write.csv(pre_tournament_elo, 'pre_tournament_elo.csv', row.names = FALSE)
 eloplot <- eloplot(eloobject = res, ids = c("mousesports", "OG", "G2", "FURIA", "Natus_Vincere", "BIG", "Astralis", "Vitality"), from = "2018-01-01", to = "2020-12-07")
}

#FIRST BRACKET
match1 <- winprob(pre_tournament_elo['Vitality'], pre_tournament_elo['mousesports'])
match2 <- winprob(pre_tournament_elo['Natus_Vincere'], pre_tournament_elo['Astralis'])
match3 <- winprob(pre_tournament_elo['G2'], pre_tournament_elo['FURIA'])
match4 <- winprob(pre_tournament_elo['OG'], pre_tournament_elo['BIG'])
first_round_preds <- c(match1, match2, match3, match4)
rm(match1, match2, match3, match4)

#UPDATE ELO based on first bracket results
m1 <- e.single(pre_tournament_elo['Vitality'], pre_tournament_elo['mousesports'],1)
m2 <- e.single(pre_tournament_elo['Natus_Vincere'], pre_tournament_elo['Astralis'], 1)
m3 <- e.single(pre_tournament_elo['G2'], pre_tournament_elo['FURIA'], 1)
m4 <- e.single(pre_tournament_elo['OG'], pre_tournament_elo['BIG'], 2)

postround1elo <- c(m1, m2, m3, m4)
rm(m1, m2, m3, m4)

#SECOND BRACKET
match5 <- winprob(postround1elo['Vitality'], postround1elo['Natus_Vincere'])
match6 <- winprob(postround1elo['mousesports'], postround1elo['Astralis'])
match7 <- winprob(postround1elo['G2'], postround1elo['BIG'])
match8 <- winprob(postround1elo['OG'], postround1elo['FURIA'])

#UPDATE ELO
m1 <- e.single(postround1elo['Vitality'], postround1elo['Natus_Vincere'],1)
m2 <- e.single(postround1elo['mousesports'], postround1elo['Astralis'], 2)
m3 <- e.single(postround1elo['G2'], postround1elo['BIG'], 2)
m4 <- e.single(postround1elo['OG'], postround1elo['FURIA'], 2)

postround2elo <- c(m1, m2, m3, m4)
rm(m1, m2, m3, m4)

#THIRD BRACKET
match9 <- winprob(postround2elo['G2'], postround2elo['Astralis'])
match10 <- winprob(postround2elo['Natus_Vincere'], postround2elo['FURIA'])

#UPDATE ELO
m1 <- e.single(postround2elo['G2'], postround2elo['Astralis'],2)
m2 <- e.single(postround2elo['Natus_Vincere'], postround2elo['FURIA'], 1)

postround3elo <- c(m1, m2)
rm(m1, m2)

#FOURTH BRACKET
match11 <- winprob(postround3elo['Natus_Vincere'], postround3elo['Astralis'])
match12 <- winprob(postround2elo['Vitality'], postround2elo['BIG'])

#UPDATE ELO
m1 <- e.single(postround3elo['Natus_Vincere'], postround3elo['Astralis'],2)
m2 <- e.single(postround2elo['Vitality'], postround2elo['BIG'], 1)

postround4elo <- c(m1, m2)
rm(m1, m2)

#FIFTH BRACKET
match13 <- winprob(postround4elo['BIG'], postround4elo['Astralis'])
#UPDATE ELO
postround5elo <- e.single(postround4elo['BIG'], postround4elo['Astralis'],2)


#SIXTH BRACKET
match14 <- winprob(postround4elo['Vitality'], postround5elo['Astralis'])

teams <- c("mousesports", "OG", "G2", "FURIA", "Natus_Vincere", "BIG", "Astralis", "Vitality")
for (j in teams) {
      for (i in teams) {
            if (i == j) {
             k <- NA
            } else {
             k <- round(as.numeric(winprob(pre_tournament_elo[j], pre_tournament_elo[i])), digits=4)}
            prob <- cbind(i, j, k)

            if (exists("matrixhelper")) {
             matrixhelper <- rbind(matrixhelper, prob)
            } else {
             matrixhelper <- prob}
       }
}
matrixhelper
rm(i,k,j, prob)


matrixhelper <- as.data.frame(matrixhelper)
matrixhelper

#matrixhelper[k] <- as.numeric(matrixhelper[k])
#TODO: Does this behave as expected? Deviations noted from first_round_preds
subjmeans <- cast(matrixhelper, i~j)


#TODO: Add a scatter of score (0-2, 1-2, 2-1, 2-0) against elo difference
#matrixhelper <- as.data.frame.table(matrixhelper)
#matrix <- matrixhelper %>%
#    pivot_wider(names_from = j, values_from = k)


#HYPERPARAMETER OPTIMISATION
optimise_for_k <- function(elo.seq_output) {
ores <- optimizek(elo.seq_output, krange = c(10, 500), resolution = 491)

plot(ores$complete$k, ores$complete$loglik, type = "l", las = 1, xlab = bquote(italic(k)), ylab = "log likelihood")
abline(v = ores$best$k, col = "red")

return(ores$best)
}

