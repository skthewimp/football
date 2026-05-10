load('epl_elo.RData')
pdf('~/Documents/work/Mint/premier league elo.pdf', 24, 13.5)
allelo %>% filter(Club %in% c("Man City", "Liverpool") & From >= as.Date('2018-08-01') & From <as.Date('2019-07-01')) %>% ggplot(aes(x=From, y=Elo, col=Club)) + geom_point() + geom_line(lwd=0.5) + theme_minimal() + ggtitle("ELO Ratings: 2018-19 English Premier League",subtitle="Liverpool Caught Up With Manchester City Only At the End Of Last Season") + xlab('')


allelo %>% group_by(Club) %>% filter(From <= as.Date('2019-07-01')) %>%  filter(From==max(From))  %>% mutate(Rank=as.numeric(Rank)) %>% arrange(Rank) %>% ungroup() %>% head(20) %>% select(Club, Elo) %>% mutate(Rank=1:n()) %>% ggplot(aes(x=Rank, y=Elo, label=Club)) + geom_text(fontface='bold', size=3.5, nudge_y=5) + geom_point() + theme_minimal() + scale_x_continuous("Elo Ranking in Premier League") + ylab("Elo Rating") + ggtitle("There are five neat clusters of Premier League teams going by Elo Ratings")

dev.off()

allelo %>% filter(Club %in% c("Man City", "Liverpool") & From >= as.Date('2018-08-01') & From <as.Date('2019-07-01')) %>% write_delim(pipe('pbcopy'), '\t')

allelo %>% group_by(Club) %>% filter(From <= as.Date('2019-07-01')) %>%  filter(From==max(From))  %>% mutate(Rank=as.numeric(Rank)) %>% arrange(Rank) %>% ungroup() %>% head(20) %>% select(Club, Elo) %>% mutate(Rank=1:n()) %>% write_delim(pipe('pbcopy'), '\t')
