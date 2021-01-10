# Title     : TODO
# Objective : TODO
# Created by: hamis
# Created on: 10/01/2021

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