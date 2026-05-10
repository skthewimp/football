load('football_elo.RData')
require(tidyverse)
clublist <- unique(allelo$Club)

allelo <- allelo[0,]

for(club in clublist) {
  url <- paste('http://api.clubelo.com/',gsub(' ', '',tolower(club)), sep='')
  allelo <- allelo %>% bind_rows(read_csv(url))
  print(paste(club, Sys.time()))
}

save(allelo, file='football_elo.RData')
