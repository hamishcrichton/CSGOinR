# Title     : TODO
# Objective : TODO
# Created by: hamis
# Created on: 09/01/2021

scrape_Dwl_one_page <- function (s_tree) {

day_results_nodes <- s_tree %>%
  html_nodes(xpath="//div[contains(@class, 'results-sublist')]")
for (j in seq_along(day_results_nodes)) {

 date <- day_results_nodes[[j]] %>%
  html_node('.standard-headline') %>%
  html_text()

 winner <- day_results_nodes[[j]] %>%
  html_nodes('.team-won') %>%
  html_text()

 loser <- day_results_nodes[[j]] %>%
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


scrape_Dwl_all_pages <- function(url) {
  s <- rvest::html_session(url)
  s_tree <- xml2::read_html(s)
  totalresult <- scrape_Dwl_one_page(s_tree)

  out <- tryCatch(
      {# Just to highlight: if you want to use more than one R expression in the "try" part then you'll have to use curly brackets. 'tryCatch()' will return the last evaluated expression in case the "try" part was completed successfully
          message("This is the 'try' part")
          while (TRUE) {
            s <- s %>% follow_link(xpath = '//div[1]/div[2]/div[2]/a[2]')
            s_tree <- xml2::read_html(s)
            totalresult <- scrape_Dwl_one_page(s_tree)
          }

          # The return value of `readLines()` is the actual value that will be returned in case there is no condition (e.g. warning or error).
          # You don't need to state the return value via `return()` as code in the "try" part is not wrapped inside a function (unlike that for the condition handlers for warnings and error below)
      },
      error=function(cond) {
          # Choose a return value in case of warning
          totalresult <- as.data.frame(totalresult)
          return(totalresult)
      }
  )
}

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
