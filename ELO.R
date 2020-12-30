# Title     : TODO
# Objective : TODO
# Created by: hamis
# Created on: 16/12/2020

###############################################################################
#                       Elo rating system implementation in R                 #
###############################################################################
#
# INTRODUCTION
# The ELO rating system is a method for calculating the relative skill levels
# of players in Player-Versus-Player games. This rating system is used on
# chess and also in a number of games, videogames, etc.
# It was originally invented as an improved chess rating system in the 1960s
# and established at the FIDE-congress 1970 in Siegen (Germany).
#
##########
# FORMULA
# ELO rating formula after a PlayerA-VS-PlayerB match:
#             RAn = RA + K * (SA - EA)
# WHERE:
# - RAn new ELO rating for player A (rating after the match);
# - RA, current elo rating (rating before the match);
# - K, weighting factor (weight for each victory, draw and defeat);
# - SA, match result of player A (1 for a victory, 0.5 for a draw, 0 for a defeat);
# - EA, expected score for player A. It is a value between 0 and 1 ().
# (Similary the Rating for player B is: RBn = RB + K * (SB - EB))
#
# THE EA (expected score) can be computed by:
# EA = 1 / ( 1 +  10^( (RB - RA) / 400 ) )
# Similarly the expected score for Player B (EB) is:
# EB = 1 / ( 1 +  10^( (RA - RB) / 400 ) )
# WHERE:
# RA, current elo rating of player A.
# RB, current elo rating of player B.
# The EA is allways a value between [0, 1].
#
#############
# EXAMPLE:
# Player A rating: 1200
# Player B rating: 1000
# k = 20
#
# What is the Player A rating, if he Wins?
# EA = 1 / ( 1 + 10^( (1000-1200)/400) ) = 0.7597
# Rn = 1200 + 20 * (1 - 0.7597) = 1204.805
#
# What is the Player A rating, if he draws?
# Rn = 1200 + 20 * (0.5 - 0.7597) = 1194.805
#
# What is the Player A rating,, if he loses?
# Rn = 1200 + 20 * (0 - 0.7597) = 1184.805
#
#
# What is the Player B rating, if he Wins?
# EA = 1 / ( 1 + 10^( (1200-1000)/400) ) = 0.2403
# Rn = 1000 + 20 * (1 - 0.2403) = 1015.195
#
# What is the Player A rating, if he draws?
# Rn = 1000 + 20 * (0.5 - 0.2403) = 1005.194
#
# What is the Player A rating, if he loses?
# Rn = 1000 + 20 * (0 - 0.2403) = 995.1940
############
# REFERENCES
#
# - http://en.wikipedia.org/wiki/Elo_rating_system
# - http://bzstats.strayer.de/bzinfo/elo/?lang=en
# - http://www.clubedexadrez.com.br/portal/cxtoledo/calculo.htm
############
#
# @author PSantos
# @date Tue May 27 03:12:29 2014
###############################################################################

#
# This function computes the ELO rating for two players.
#
# @playerARating current elo rating for player A.
# @PlayerBRating current elo rating for player B.
# @K weighting factor.
# @return a data frame with ELO rating for both players in case of
# win, draw, and losse. This is a 2 (rows) by 4 (columns) data frame.
#
# Example:
# > eloRating(1200,1000,20)
#         chanceToWin playerAWins draw playerBWins
# playerA          76        1205 1195        1185
# playerB          24         995 1005        1015
#
eloRating=function(playerARating, PlayerBRating, k=32) {

  # Expected score for player A and for player B.
  EA <- (1 / (1 + 10^((PlayerBRating - playerARating)/400)))
  EB <- (1 / (1 + 10^((playerARating - PlayerBRating)/400)))

  # RAn = RA + K * (SA - EA)
  newRatingPlyAWins  <- playerARating + k * (1 - EA)
  newRatingPlyADraws <- playerARating + k * (0.5 - EA)
  newRatingPlyADefeated  <- playerARating + k * (0 - EA)

  # RBn = RB + K * (SB - EB)
  newRatingPlyBWins  <- PlayerBRating + k * (1 - EB)
  newRatingPlyBDraws <- PlayerBRating + k * (0.5 - EB)
  newRatingPlyBDefeated  <- PlayerBRating + k * (0 - EB)

  chanceToWin <- round(data.frame(chanceToWin=c(EA, EB)) * 100, digits=0)
  playerAWins  <- round(data.frame(playerAWins=c(newRatingPlyAWins, newRatingPlyBDefeated)), digits=0)
  playerDraw  <- round(data.frame(draw=c(newRatingPlyADraws, newRatingPlyBDraws)), digits=0)
  playerBWins  <- round(data.frame(playerBWins=c(newRatingPlyADefeated, newRatingPlyBWins)), digits=0)

  df <- cbind(chanceToWin, playerAWins, playerDraw, playerBWins)
  rownames(df) <- c('playerA', 'playerB')
  return(df)
}

UpdateeloRating <- function(TeamARating, TeamBRating, TeamAresult, k=32) {
  # Expected win % for player A and for player B.
  EA <- (1 / (1 + 10^((TeamBRating - TeamARating)/400)))
  EB <- (1 / (1 + 10^((TeamARating - TeamBRating)/400)))

  # RAn = RA + K * (SA - EA)
  newRatingAWins  <- TeamARating + k * (1 - EA)
  newRatingADraws <- TeamARating + k * (0.5 - EA)
  newRatingADefeated  <- TeamARating + k * (0 - EA)

  # RBn = RB + K * (SB - EB)
  newRatingBWins  <- TeamBRating + k * (1 - EB)
  newRatingBDraws <- TeamBRating + k * (0.5 - EB)
  newRatingBDefeated  <- TeamBRating + k * (0 - EB)

  if (TeamAresult == 'W') {
    TeamARating <- newRatingAWins
    TeamBRating <- newRatingBDefeated
  } else if (TeamAresult == 'L') {
    TeamARating <- newRatingADefeated
    TeamBRating <- newRatingBWins
  }
  new_ratings <- c(TeamARating, TeamBRating)
  return(new_ratings)
}

Chanceofwin <- function(TeamARating, TeamBRating) {
  EA <- (1 / (1 + 10^((TeamBRating - TeamARating)/400)))
  EB <- (1 / (1 + 10^((TeamARating - TeamBRating)/400)))
  chanceToWin <- round(data.frame(chanceToWin=c(EA, EB)) * 100, digits=0)
  return(chanceToWin)
}

VitalityRating <- 1200
mousesportsRating <- 1200
NaViRating <- 1200
AstralisRating <- 1200
FURIARating <- 1200
G2Rating <- 1200
BIGRating <- 1200
OGRating <- 1200


VitalityRating <- UpdateeloRating(VitalityRating, mousesportsRating, 'W', k=32)[1]
mousesportsRating <- UpdateeloRating(VitalityRating, mousesportsRating, 'W', k=32)[2]

VitalityRating
mousesportsRating



