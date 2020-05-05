library(lubridate)
library(dplyr)
library(tidyverse)
library(plotly)

#Looking at Covid in Iowa

dat = read.csv("us_states_covid19_daily.csv", stringsAsFactors = FALSE)
View(dat)
str(dat)
dat$date = ymd(dat$date)


iowa <- dat %>%
  select(date, state, positive, death, totalTestResults) %>%
  filter(state == 'IA')

#replacing NAs with 0 in death column for better visualization in plotly 
iowa[is.na(iowa)] = 0


iowaLine <- plot_ly(iowa, x = ~date, y = ~positive,type = 'scatter', mode = 'lines+markers', name = 'Positive Cases')

iowaLine <- iowaLine %>%
  add_trace(y= ~death, type = 'scatter', mode = 'lines+markers', name= 'Fatal') %>%
  layout(hovermode = 'x', title= "COVID-19 in Iowa over time", xaxis = list(title = "Date"), yaxis = list(title = " Number of Cases"))

iowaLine


counties = read.csv("covid-19_iowacounties.csv", stringsAsFactors = FALSE)
str(counties)

counties$Population <- as.numeric(gsub(",","",counties$Population))



IA_county<- counties %>%
  filter(Population >= 13500) %>%
  ggplot(counties, mapping= aes(x=County, y=Confirmed, fill=Confirmed/Population *1000)) + 
  geom_bar(stat="identity", position = "dodge") + coord_flip() + ylab("Number of Confirmed Cases") + ggtitle("Confirmed cases in Iowa by County")

IA_county


IA_county2<- counties %>%
  filter(Population >= 13500) %>%
  ggplot(counties, mapping= aes(x=County, y=Confirmed, fill=Population)) + 
  geom_bar(stat="identity", position = "dodge") + coord_flip() + ylab("Number of Confirmed Cases") + ggtitle("Confirmed cases in Iowa by County")

IA_county2



#looking at the increase in number of cases in iowa over time 

iowaTests<- plot_ly(iowa, x = ~date, y = ~totalTestResults,type = 'scatter', mode = 'lines+markers', name = 'Tests')

iowaTests<- iowaTests %>%
  add_trace(y= ~positive, type = 'scatter', mode = 'lines+markers', name= 'Positive Tests') %>%
  layout(hovermode = 'x', title= "Testing in Iowa over time", xaxis = list(title = "Date"), yaxis = list(title = " Number of Tests"))

iowaTests





#Looking at Covid Testing

US <- plot_ly(dat, x = ~state, y = ~positive, type = 'bar', mode = 'lines+markers', name = 'Positive Tests')
US <- US %>%
  add_trace(y=~totalTestResults, name="Total Tested") %>%
layout(title= "Total Number Tests vs Positive Tests in U.S", xaxis = list(title = "State"), yaxis = list(title = " Number of Tests"))

US

unique(dat$date)

dat %>% 
  group_by(state) %>%
  plot_ly(x=~date, y=~totalTestResults, group=~state, type = 'bar', color = ~state, mode='lines+markers') 

ggplot(dat, aes(x=date)) + geom_line(aes(y= positive), color='green') + geom_line(aes(y=totalTestResults), color='blue') + facet_wrap(~state) 






# now using the data john cleaned from his file 

dat2 = read.csv("us_states_covid19_daily.csv", stringsAsFactors = FALSE)
dat2$date = ymd(dat2$date)
names(dat2)[2] = "state_abbrev"
dat2$state_abbrev = as.factor(dat2$state_abbrev)
dat2$fips = as.factor(dat2$fips)
dat2 = dat2 %>%
  filter(fips != '60' & fips != '66' & fips != '69' & fips != '72' & fips != '78')
dat2$fips = factor(dat2$fips, levels= dat2$fips, labels=dat2$fips)

dist = (read.csv("social_distancing.csv", header=F)[-c(1:4,56:66),])
dist_headers = read.csv("social_distancing.csv", header = F,stringsAsFactors = F)[c(3),]
colnames(dist) =c(dist_headers[1,])
names(dist)[1] = "state"
dist$state = factor(dist$state,levels= dist$state,labels = dist$state)

pop = read.csv("state_population.csv", header = F)[-c(1:9,61:67),]
pop_headers = read.csv("state_population.csv", header = F,stringsAsFactors = F)[c(4),]
colnames(pop) = c(pop_headers[1,])
names(pop)[1] = "state"
pop$state = sub('.', '', pop$state)
pop$state = as.factor(pop$state)
pop$`2019` = as.numeric(gsub(",", "", pop$`2019`))
pop$`2019` = as.numeric(pop$`2019`)

Fips = read.csv("fips.csv", header=F, stringsAsFactors = F)[-c(1:5),]
Fips_headers = read.csv("fips.csv",header=F,stringsAsFactors = F)[c(5),]
colnames(Fips) = c(Fips_headers[1,])
Fips = Fips %>%
  filter(`Summary Level` == '040') %>%
  select(`State Code (FIPS)`, state = `Area Name (including legal/statistical area description)`) %>%
  filter(state != 'Puerto Rico')
names(Fips)[1] = "fips"
Fips$fips = gsub("^0","",Fips$fips)
Fips$fips = as.factor(Fips$fips)
Fips$state = factor(Fips$state, levels = Fips$state, labels = Fips$state)


dist = pop %>%
  select("state","2019") %>%
  inner_join(dist, by = "state")
names(dist)[2] = "population"
dist$state = as.factor(dist$state)

dist = Fips %>%
  inner_join(dist, by = "state")

dat2 = dat2 %>%
  inner_join(dist, by = "fips")
dat2$fips = as.factor(dat2$fips)

View(dat2)

plot<- ggplot(dat2, aes(x=date)) + geom_line(aes(y=positive/population *1000, color='Positive Tests')) + geom_line(aes(y=totalTestResults/population * 1000, color='Tests')) + facet_wrap(~state) +   ggtitle("State Testing Over Time") + ylab('Number of Cases per 1000') + xlab('Date') 
plot




