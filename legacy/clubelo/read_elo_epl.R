setwd('~/Documents/work/football/')
load('football_elo_epl.RData')
require(tidyverse)
clublist <- allelo %>% filter(Country=='ENG') %>% distinct(Club) %>% pull(Club)

allelo <- allelo[0,]

for(i in 1:length(clublist)) {
  a <- tryCatch({
    url <- paste('http://api.clubelo.com/',gsub(' ', '',tolower(clublist[i])), sep='')
    allelo <- allelo %>% bind_rows(read_csv(url))
  }, 
  error= function(cond) {
    print(cond)
    if(grepl('timed out', cond) | grepl('Timeout', cond) |  grepl('curl', cond) | grepl('504', cond) | grepl('502', cond)) { # we've hit rate limit, so wait
      Sys.sleep(15)  # fifteen seconds is enough of a wait
      i <- i - 1
    } else
      print(paste('unable to process', newGames[i]))
  }, 
  finally={
    print(paste(i, clublist[i], Sys.time()))
    i <- i + 1
  }
  )
  
}

save(allelo, file='football_elo_epl.RData')
