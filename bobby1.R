library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)

dat = read.csv("us_states_covid19_daily.csv", stringsAsFactors = FALSE)
#View(dat)
str(dat)
dat$date = ymd(dat$date)
dat$date <- rev(dat$date)



trend <- dat %>% 
  group_by(date)  
ggplot(trend,aes(x= date,y = positiveIncrease,fill = state)) +geom_bar(stat= 'identity') +xlab("Date") +ylab("Cases")+
  ggtitle("New reported cases by day in the United States") +theme(legend.position = 'none') 
ggplot(trend,aes(x= date,y = deathIncrease,fill = state)) +geom_bar(stat= 'identity') +ylim(0,3000)+xlab("Date") +ylab("Deaths")+
  ggtitle("New reported deaths by day in the United States") +theme(legend.position = 'none') 
ggplot(trend,aes(x= date,y = positive,fill = state)) +geom_bar(stat= 'identity') +xlab("Date") +ylab("Total Cases")+
  ggtitle("Total Cases by day in the United States") +theme(legend.position = 'none')
ggplot(trend,aes(x= date,y = positive,fill = state)) +geom_point()
