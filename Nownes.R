library(lubridate)

dat = read.csv("us_states_covid19_daily.csv", stringsAsFactors = FALSE)
dist = (read.csv("social_distancing.csv", header=F)[-c(1:3,56:66),])
headers = read.csv("social_distancing.csv", header = F,stringsAsFactors = F)[c(3),]
colnames(dist) =c(headers[1,])

dat$date = ymd(dat$date)
dat$state = as.factor(dat$state)

dist$Location = factor(dist$Location,levels= dist$Location)
str(dat)
str(dist)
