# Title     : TODO
# Objective : TODO
# Created by: hamis
# Created on: 10/01/2021

clean_data <- function (file_loc){

totalresult <- read.csv(gold_standard, header = TRUE)
##Reformatting the Dataframe
#TODO: reformat this using Regex- esp. August
totalresult$Date <- gsub('Results for ','', totalresult$Date)
totalresult$Date <- gsub('st','', totalresult$Date)
totalresult$Date <- gsub('Augu','August', totalresult$Date)
totalresult$Date <- gsub('nd','', totalresult$Date)
totalresult$Date <- gsub('rd','', totalresult$Date)
totalresult$Date <- gsub('th','', totalresult$Date)
#write.csv(totalresult, 'total_results_again.csv', row.names = FALSE)
totalresult$Date <- as.Date(totalresult$Date, format= '%B %d %Y')
totalresult <- totalresult[order(totalresult$Date, decreasing = FALSE),]
return(totalresult)
}

#totalresult <- clean_data(gold_standard)

##WRITE TO CSV
#write.csv(totalresult, 'total_results_again.csv', row.names = FALSE)