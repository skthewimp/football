pdf('~/Documents/work/Mint/solksjaer.pdf', 24, 13.5)
mgrelo %>%
  filter(Club=="Man United" & From1 >= as.Date('2008-07-01') & From1 <= Sys.Date()) %>%
  mutate(From=pmax(From, as.Date('2008-07-01')), Mid=From + as.numeric(To-From, units='days')/2) %>%
  ggplot() + 
  geom_rect(
    data=. %>% distinct(Name, Club, From, To, MinElo, MaxElo, Mid, AvgElo, Index), 
    aes(xmin=From, xmax=To, ymin=MinElo, ymax=MaxElo, fill=factor(Index%%9+1)), alpha=0.5, lwd=1) + 
  geom_vline(xintercept=as.Date('2019-03-28'), lty=2, col='red') +
  geom_line(aes(x=From1, y=Elo), lwd=0.5) + 
  geom_text(
    data=. %>% distinct(Name, Club, From, To, MinElo, MaxElo, Mid, AvgElo,Name),
    aes(x=Mid, y=AvgElo, label=Name),
    fontface='bold', col='dark red'
    ) + 
  theme_bw() + scale_x_date('', date_breaks ='1 year', labels=function(x) format(x, '%Y')) + ylab("Elo Rating") +
  scale_fill_brewer(palette="Set3") + theme(legend.position = 'none') + ggtitle("Manchester United: Alex Ferguson and beyond", subtitle = 'Elo Ratings since 2008-9 season')
dev.off() 



mgrelo %>% 
  group_by(Club) %>% 
  filter(Name==Name[From1==max(From1)][1] ) %>% 
  filter(max(From1) >= as.Date('2019-09-01') & Level[From1==max(From1)][1]==1 ) %>% 
  group_by(Club, Name) %>% 
  summarise(
    Highest=max(Elo), 
    Lowest=min(Elo),
    Start=min(From),
    Latest=Elo[From1==max(From1)][1]
  ) %>%
  filter(Club !='Watford') %>%
  mutate(
    Drop=Highest-Latest, 
    Relative=(Latest-Lowest)/(Highest-Lowest)
  ) %>% 
  arrange(Drop)

mgrelo %>% 
  group_by(Club) %>% 
  filter(Name==Name[From1==max(From1)][1] ) %>% 
  filter(max(From1) >= as.Date('2019-09-01') & Level[From1==max(From1)][1]==1 ) %>% 
  ggplot(aes(x=From1, y=Elo)) + geom_line() + facet_wrap(~Club, scales='free')