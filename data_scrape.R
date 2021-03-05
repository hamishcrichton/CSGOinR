# Title     : Applying machine learning to eSports
# Objective : Beat the bookies: predict each match result from BLAST Premier Global Finals 2020
# Created by: hamis
# Created on: 16/12/2020


#manage packages
#TODO: which packages are actually used in the scraping?
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

#starting url
url <- "https://www.hltv.org/results"

#For one page of results, visit each link and scrape the required information
heavy_scrape_one_page <- function (s_tree) {

match_links <- s_tree %>%
  html_nodes(".result-con ") %>%
  html_node(".a-reset") %>%
  html_attr("href")

for (k in match_links) {
  k <- paste0("https://www.hltv.org",k)
  s <- rvest::html_session(k)
  if (status_code(s) == 500) {next}
  s_tree <- xml2::read_html(s)


  ### Scrape the results from one page
  date <- s_tree %>%
   html_node('.date') %>%
   html_text()
  ###data-time-format="do 'of' MMMM y"

  team1 <- s_tree %>%
   html_node('.team1-gradient .teamName') %>%
   html_text()
  team1 <- gsub(' ','_', team1)

  team1_score <- s_tree %>%
    html_node('.team1-gradient') %>%
    html_node('.won, .lost, .tie') %>%
    html_text()

  team2 <- s_tree %>%
    html_node('.team2-gradient .teamName') %>%
    html_text()

  team2_score <- s_tree %>%
    html_node('.team2-gradient') %>%
    html_node('.won, .lost, .tie') %>%
    html_text()

  form <- s_tree %>%
    html_nodes('.past-matches td.spoiler.result') %>%
    html_attr('class')
  form <- gsub('spoiler result ','', form)
  form <- gsub('won',"W", form)
  form <- gsub('lost',"L", form)
  team1_form <- paste(form[1:5], collapse ='')
  team2_form <- paste(form[6:10], collapse ='')
  rm(form)

  team_rankings <- s_tree %>%
    html_nodes('.teamRanking') %>%
    html_text()
  team1_rank <- team_rankings[1]
  team2_rank <- team_rankings[2]
  rm(team_rankings)

  head2head <- s_tree %>%
    html_nodes('.head-to-head .bold') %>%
    html_text()
  head2head <- paste(head2head[1], head2head[3], sep=" : ")

  t1_rounds_won_offence <- 0
  t2_rounds_won_offence <- 0
  t1_rounds_won_defence <- 0
  t2_rounds_won_defence <- 0

  out <- tryCatch(
      {
          first_through <- s_tree %>%
            html_nodes('.results-center-half-score') %>%
            html_nodes('.ct, .t')

          for (i in first_through) {
            j <- i %>%
              html_text()
            k <- i %>%
              html_attrs()

          if (exists("first_round")) {
            first_round <- rbind(first_round, (paste(j, k)))
          } else {
            first_round <- paste(j,k)
          }
        }
            #now run through the odd numbers for team1 wins
            #if ends in ct, add to ct rounds won
            #even numbers are team2 wins (=team1 losses)
            t1_rounds_won_offence <- 0
            t2_rounds_won_offence <- 0
            t1_rounds_won_defence <- 0
            t2_rounds_won_defence <- 0

            for (l in seq_along(first_round)) {
              if ((l %% 2 == 1) & grepl('ct$', first_round[l])) {
                t1_rounds_won_defence <- as.numeric(t1_rounds_won_defence) + as.numeric(str_split(first_round[l],' ')[[1]][1])
              } else if ((l %% 2 == 1) & grepl(' t$', first_round[l])){
                t1_rounds_won_offence <- as.numeric(t1_rounds_won_offence) + as.numeric(str_split(first_round[l],' ')[[1]][1])
              } else if ((l %% 2 == 0) & grepl('ct$', first_round[l])){
                t2_rounds_won_defence <- as.numeric(t2_rounds_won_defence) + as.numeric(str_split(first_round[l],' ')[[1]][1])
              } else if ((l %% 2 == 0) & grepl(' t$', first_round[l])){
                t2_rounds_won_offence <- as.numeric(t2_rounds_won_offence) + as.numeric(str_split(first_round[l],' ')[[1]][1])
              }
            }
            rounds_won <- c(t1_rounds_won_offence, t2_rounds_won_offence, t1_rounds_won_defence, t2_rounds_won_defence)
            rm(first_through, first_round, l)
            #return(rounds_won)
      },
      error=function(cond) {
        # Choose a return value in case of error

        #return(totalresult)
        }
  )

    out <- tryCatch(
      {   player_stats <- s_tree %>%
            html_nodes('.stats-content') %>%
            html_nodes(xpath='//div[@id = "all-content"]') %>%
            html_nodes(xpath='//table[@class = "table totalstats"]') %>%
            html_nodes('.rating') %>%
            html_text()

          player_stats <- paste(player_stats, collapse = ', ')

          statsplaceholder <- str_split(player_stats,'Rating2.0,')
          team1_player_stats <- statsplaceholder[[1]][2]
          team2_player_stats <- statsplaceholder[[1]][3]
          rm(player_stats)
          #return(player_stats)
      },
      error=function(cond) {
        # Choose a return value in case of error
        player_stats <- c('N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N')
        #return(player_stats)
        }

  )

  match_result <- c(date, team1, team1_score, team2, team2_score, head2head, team1_form, team2_form, team1_rank, team2_rank, t1_rounds_won_offence, t2_rounds_won_offence, t1_rounds_won_defence, t2_rounds_won_defence, team1_player_stats, team2_player_stats)
   if (exists("totalresult")) {
  totalresult <- rbind(totalresult, match_result)
 } else {
  totalresult <- match_result
 }

  Sys.sleep(1)
}
  return(totalresult)
}


#Testing heavy_scrape_one_page
#url <- 'https://www.hltv.org/results?offset=1300'
#s <- rvest::html_session(url)
#s_tree <- xml2::read_html(s)
#heavy_scrape_one_page(s_tree)

#Move between pages of results, running heavy_scrape_one_page on each. Catches error when pages run out.
heavy_scrape <- function(url) {
  s <- rvest::html_session(url)
  s_tree <- xml2::read_html(s)
  totalresult <- heavy_scrape_one_page(s_tree)
  out <- tryCatch(
      {# Loop until error: Move to the next page; scrape that page; collate the results
          #message("This is the 'try' part")
          while (TRUE) {
            s <- s %>% follow_link(xpath = '//div[1]/div[2]/div[2]/a[2]')
            s_tree <- xml2::read_html(s)
            if (status_code(s) == 500) {next}
            more_results <- heavy_scrape_one_page(s_tree)
            totalresult <- rbind(totalresult, more_results)
            if (length(totalresult) %% 16000 ==0) {
              write.csv(totalresult, 'test_case_total.csv', row.names = FALSE)
            }
          }
      }

  )
  #Change column names to be legible
  colnames(totalresult) <- c('date', 'team1', 'team1_score', 'team2', 'team2_score', 'head2head', 'team1_form', 'team2_form', 'team1_rank', 'team2_rank', 't1_rounds_won_offence', 't2_rounds_won_offence', 't1_rounds_won_defence', 't2_rounds_won_defence', 'team1_player_stats', 'team2_player_stats')

  return(totalresult)
}

#Main function
totalresult <- heavy_scrape(url)

#Single truth for data: write to csv to remove the need to repeat
#write.csv(totalresult, 'gold_standard.csv', row.names = FALSE)

