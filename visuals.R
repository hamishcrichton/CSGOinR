# Title     : TODO
# Objective : TODO
# Created by: hamis
# Created on: 10/01/2021

#Create a matrix of ex-ante win probabilities.
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

#TODO: Does this behave as expected? Deviations noted from first_round_preds
subjmeans <- cast(matrixhelper, i~j)


#TODO: Add a scatter of score (0-2, 1-2, 2-1, 2-0) against elo difference
em1 <- c((pre_tournament_elo['Vitality'] - pre_tournament_elo['mousesports']), "2-0")
em2 <- c((pre_tournament_elo['Natus_Vincere'] - pre_tournament_elo['Astralis']), "2-1")
em3 <- c((pre_tournament_elo['G2'] - pre_tournament_elo['FURIA']), "2-1")
em4 <- c((pre_tournament_elo['OG'] - pre_tournament_elo['BIG']), "0-2")

em5 <- c((postround1elo['Vitality'] - postround1elo['Natus_Vincere']), "2-0")
em6 <- c((postround1elo['mousesports'] - postround1elo['Astralis']), "0-2")
em7 <- c((postround1elo['G2'] - postround1elo['BIG']), "1-2" )
em8 <- c((postround1elo['OG'] - postround1elo['FURIA']), "1-2")

em9 <- c((postround2elo['G2'] - postround2elo['Astralis']), "0-2")
em10 <- c((postround2elo['Natus_Vincere'] - postround2elo['FURIA']), "2-0")

em11 <- c((postround3elo['Natus_Vincere'] - postround3elo['Astralis']), "0-2")
em12 <- c((postround2elo['Vitality'] - postround2elo['BIG']), "2-1")

em13 <- c((postround4elo['BIG'] - postround4elo['Astralis']), "0-2" )
em14 <- c((postround4elo['Vitality'] - postround5elo['Astralis']), "2-1")


elo_difference_result <- rbind(em1, em2, em3, em4, em5, em6, em7, em8, em9, em10, em11, em12, em13, em14)
#elo_difference_result %>%
#  rename()
#
#ggplot(data = elo_difference_result) +
#  aes(y = body_mass_g, x = species) +
#  geom_beeswarm(cex = 0.5) +
#  coord_flip()

#matrixhelper <- as.data.frame.table(matrixhelper)
#matrix <- matrixhelper %>%
#    pivot_wider(names_from = j, values_from = k)


