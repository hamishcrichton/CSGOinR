# Title     : TODO
# Objective : TODO
# Created by: hamis
# Created on: 16/01/2021

clean_data <- function (dataframe){

##Reformatting the Dataframe
#TODO: reformat this using Regex- esp. August
dataframe$Date <- gsub('Results for ','', dataframe$Date)
dataframe$Date <- gsub('st','', dataframe$Date)
dataframe$Date <- gsub('Augu','August', dataframe$Date)
dataframe$Date <- gsub('nd','', dataframe$Date)
dataframe$Date <- gsub('rd','', dataframe$Date)
dataframe$Date <- gsub('th','', dataframe$Date)
dataframe$Date <- as.Date(dataframe$Date, format= '%B %d %Y')
dataframe <- totalresult[order(dataframe$Date, decreasing = FALSE),]
return(dataframe)
}

totalresult <- read.csv("scrape_transform_csgo_results.csv", header = TRUE)


###PSEUDO CODE for offense/defense ratings.
#if gold_standard['t1_rounds_won_offence'] > gold_standard['t2_rounds_won_defence']
#add a new row to the o/d list -> date, o_t1, d_t2, FALSE
# else if t1o < t2d -> date, d_t2, o_t1, FALSE
# else if t1o = t2d -> date, t1o, t2d, TRUE


for (row in seq_len(nrow(totalresult))) {
  date <- totalresult[row, "Date"]

  t1 <- totalresult[row, "team1"]
  t2 <- totalresult[row, "team2"]

  t1o <- totalresult[row, "t1_rounds_won_offence"]
  t2d  <- totalresult[row, "t2_rounds_won_defence"]

  t2o <- totalresult[row, "t2_rounds_won_offence"]
  t1d <- totalresult[row, "t1_rounds_won_defence"]

  t1score <- totalresult[row, "team1_score"]
  t2score <- totalresult[row, "team2_score"]


  if (t1o > t2d) {
    brow <- c(date, paste("o",t1, sep='_'), paste("d",t2, sep='_'), "FALSE")
  } else if (t2d > t1o) {
    brow <- c(date, paste("d",t2, sep='_'), paste("o",t1, sep='_'), "FALSE")
  } else if (t1o == t2d) {
    brow <- c(date, paste("o",t1, sep='_'), paste("d",t2, sep='_'), "TRUE")
  }

  if (t1d > t2o) {
    crow <- c(date, paste("d",t1, sep='_'), paste("o",t2, sep='_'), "FALSE")
  } else if (t2o > t1d) {
    crow <- c(date, paste("o",t2, sep='_'), paste("d",t1, sep='_'), "FALSE")
  } else if (t1d == t2o) {
    crow <- c(date, paste("d",t1, sep='_'), paste("o",t2, sep='_'), "TRUE")
  }

  if (exists("elo_frame")) {
    elo_frame <- rbind(elo_frame, brow, crow)
  } else {
    elo_frame <- rbind(brow, crow)
    #colnames(elo_frame) <- c('date', 'winner', 'loser', 'draw')
  }

  if (t1score > t2score) {
    prow <- c(date, t1, t2, "FALSE")
  } else if (t2score > t1score) {
    prow <- c(date, t2, t1, "FALSE")
  } else if (t1score == t2score) {
    prow <- c(date, t1, t2, "TRUE")
  }
  if (exists("elo_total_frame")) {
    elo_total_frame <- rbind(elo_total_frame, prow)
  } else {
    elo_total_frame <- prow
    #colnames(elo_frame) <- c('date', 'winner', 'loser', 'draw')
  }
}
#elo_frame <- elo_frame[order("Date")]
#elo_total_frame <- elo_total_frame[order("Date")]
colnames(elo_frame) <- c('Date', 'winner', 'loser', 'Draw')
colnames(elo_total_frame) <- c('Date', 'winner', 'loser', 'Draw')
write.csv(elo_total_frame, 'elo_total_frame.csv', row.names = FALSE)
#elo_frame <- clean_data(elo_frame)
write.csv(elo_frame, 'elo_frame.csv', row.names = FALSE)
