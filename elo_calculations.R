# Title     : TODO
# Objective : TODO
# Created by: hamis
# Created on: 10/01/2021


#Read in the data
if (!exists('totalresult')) {
 totalresult <- read.csv("total_results_again.csv", header = TRUE)
 totalresult <- as.data.frame.matrix(totalresult)
 totalresult <- gsub(' ','_', totalresult)


 #ELO rating process
 seqcheck(winner = totalresult$winner, loser = totalresult$loser, Date = totalresult$Date)
 res <- elo.seq(winner = totalresult$winner, loser = totalresult$loser, Date = totalresult$Date, runcheck = FALSE, k=39)

 #Extract elo values for participants at the start of the tournament
 pre_tournament_elo <- extract_elo(res, extractdate = "2020-12-07", IDs = c("mousesports", "OG", "G2", "FURIA", "Natus_Vincere", "BIG", "Astralis", "Vitality"))
 summary(res)

 write.csv(pre_tournament_elo, 'pre_tournament_elo.csv', row.names = TRUE)
 eloplot <- eloplot(eloobject = res, ids = c("mousesports", "OG", "G2", "FURIA", "Natus_Vincere", "BIG", "Astralis", "Vitality"), from = "2018-01-01", to = "2020-12-07")
}


#HYPERPARAMETER OPTIMISATION
optimise_for_k <- function(elo.seq_output) {
ores <- optimizek(elo.seq_output, krange = c(10, 500), resolution = 491)

plot(ores$complete$k, ores$complete$loglik, type = "l", las = 1, xlab = bquote(italic(k)), ylab = "log likelihood")
abline(v = ores$best$k, col = "red")

return(ores$best)
}

optimise_for_k(res)