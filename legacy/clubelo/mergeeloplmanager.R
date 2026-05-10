load('football_elo_epl.RData')
tmp <- tempfile()
require(RCurl)
download.file('https://en.wikipedia.org/wiki/List_of_Premier_League_managers', tmp, method='curl')
require(rvest)
mgrs <- read_html(tmp) %>% html_nodes('table') %>% .[[2]] %>% html_table(fill=T) %>% .[,1:5]
mgrs <- mgrs %>% 
  as_tibble() %>% 
  mutate(From=as.Date(From, '%d %B %Y'), To=as.Date(Until, '%d %B %Y')) %>%
  group_by(Name, Club) %>%
  mutate(To=coalesce(To, lead(From,1)), From=coalesce(From, lag(To,1)), To=coalesce(To, Sys.Date())) %>%
  select(-Until)

download.file('https://en.wikipedia.org/wiki/List_of_EFL_Championship_managers', tmp, method='curl')
mgrs2 <- read_html(tmp) %>% html_nodes('table') %>% .[[2]] %>% html_table(fill=T) %>% .[,1:5]
mgrs2 <- mgrs2 %>% 
  as_tibble() %>% 
  rename(Club=`Championship club`) %>%
  mutate(From=as.Date(From, '%d %B %Y'), To=as.Date(Until, '%d %B %Y')) %>%
  group_by(Name, Club) %>%
  mutate(To=coalesce(To, lead(From,1)), From=coalesce(From, lag(To,1)), To=coalesce(To, Sys.Date())) %>%
  select(-Until)
mgrs <- mgrs %>%
  bind_rows(mgrs2) %>%
  distinct()



clubRename <- mgrs %>% 
  ungroup() %>% 
  distinct(Club) %>% 
  filter(!Club %in% allelo$Club) %>%
  mutate(Nickname=word(Club,1)) %>%
  filter(!Nickname %in% allelo$Club) %>%
  mutate(Nickname=case_when(
    Club=='Manchester City' ~ 'Man City',
    Club=='Manchester United' ~ 'Man United', 
    Club=='Queens Park Rangers' ~ 'QPR',
    Club=='Nottingham Forest' ~ 'Forest',
    Club=='Sheffield Wednesday' ~ 'Sheffield Weds',
    Club=='Tottenham Hotspur'  ~ 'Tottenham',
    Club=='West Ham United' ~ 'West Ham',
    Club=='West Bromwich Albion' ~ 'West Brom',
    Club=='Wolverhampton Wanderers' ~ 'Wolves'
  ))

mgrs <- mgrs %>%
  ungroup() %>%
  left_join(clubRename, by='Club') %>%
  mutate(Club=coalesce(Nickname, Club)) %>%
  select(-Nat., -Nickname)

mgrelo <- mgrs %>%
  arrange(Club, From) %>%
  group_by(Club) %>%
  mutate(Index=1:n(), Regime=as.numeric(To-From, units='days')) %>%
  ungroup() %>%
  filter(To >= as.Date('1937-01-01') & Regime >= 30) %>%
  mutate(
    FirstWord=word(Club, 1),
    InElo=Club %in% (allelo %>% filter(Country=='ENG') %>% distinct(Club) %>% pull(Club)),
    Club=ifelse(InElo, Club, FirstWord),
    Parity=Index %% 2, 
    LastName=word(Name,-1)
    ) %>%
  select(Club, From, To, Index, Parity, Name) %>%
  inner_join(allelo %>% filter(Country=='ENG') %>% select(Club, Level, Elo, From1=From, To1=To), by='Club') %>%
  filter(From1 >= From & From1 <= To)

mgrelo <- mgrelo %>%
  bind_rows(
    allelo %>% 
      filter(Country=='ENG' & Club %in% mgrelo$Club) %>% 
      select(Club, Level, Elo, From1=From, To1=To) %>%
      anti_join(
        mgrelo %>%
          select(Club, From1=From, To1=To),
        by = c("Club",  "From1", "To1")
      )
  ) %>%
  group_by(Club, Name, From) %>%
  mutate(MaxElo=max(Elo), MinElo=min(Elo), AvgElo=ifelse(Parity, MaxElo-10, MinElo+10)) %>%
  group_by(Club) %>%
  arrange(From1) %>%
  mutate(Relegation=Level==lag(Level,1)+1, Promotion=Level==lag(Level,1)-1) %>%
  ungroup() %>%
  arrange(Club, From1) %>%
  distinct(Club, From1, .keep_all = T)

mgrelo %>% 
  group_by(Club) %>%
  mutate(Name=coalesce(Name,''), prevName=coalesce(lag(Name,1),''), change=Name!=prevName, Index=cumsum(change)) %>%
  group_by(Club, Index) %>%
  mutate(
    From=min(From1),
    To=max(To1),
    AvgElo=mean(Elo),
    Parity=Index %% 2,
    MaxElo=max(Elo), 
    MinElo=min(Elo), 
    AvgElo=ifelse(Parity, MaxElo-10, MinElo+10)
  ) %>% 
  ungroup() ->
  mgrelo

  
  
save(mgrelo, file='elo/plmanagerelo.RData')
