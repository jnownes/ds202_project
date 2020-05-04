library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)

setwd("C:/Users/Kobe/Documents/DS202/ds202_project")

# Using the data frame that John Nownes cleaned up for us
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

# What is the increase in the amount of people tested in each state over time?
plot <- ggplot(dat2, aes(x = date)) + geom_line(aes(y = totalTestResults, color = state_abbrev)) + ggtitle("State Testing Overtime") + ylab('Number of Tests') + xlab('Date') 
plot

# Which state is likely to have the most/least confirmed cases in 2 weeks time depending on the current growth rate?
# Most confirmed cases in 2 weeks
newYork <- dat2 %>%
  select(date, state_abbrev, positive, death, totalTestResults) %>%
  filter(state_abbrev == 'NY')

newYork[is.na(newYork)] = 0

newYorkTrend <- plot_ly(newYork, x = ~date, y = ~totalTestResults, type = 'scatter', mode = 'lines+markers', name = 'Tests Given')

newYorkTrend <- newYorkTrend %>%
  add_trace(y = ~positive, type = 'scatter', mode = 'lines+markers', name = 'Positive') %>%
  add_trace(y= ~death, type = 'scatter', mode = 'lines+markers', name = 'Deaths') %>%
  layout(hovermode = 'x', title = "COVID-19 Trend in New York", xaxis = list(title = "Date"), yaxis = list(title = "Total"))

newYorkTrend

# Least confirmed cases in two weeks
alaska <- dat2 %>%
  select(date, state_abbrev, positive, death, totalTestResults) %>%
  filter(state_abbrev == 'AK')

alaska[is.na(alaska)] = 0

alaskaTrend <- plot_ly(alaska, x = ~date, y = ~totalTestResults, type = 'scatter', mode = 'lines+markers', name = 'Tests Given')

alaskaTrend <- alaskaTrend %>%
  add_trace(y = ~positive, type = 'scatter', mode = 'lines+markers', name = 'Positive') %>%
  add_trace(y= ~death, type = 'scatter', mode = 'lines+markers', name = 'Deaths') %>%
  layout(hovermode = 'x', title = "COVID-19 Trend in Alaska", xaxis = list(title = "Date"), yaxis = list(title = "Total"))

alaskaTrend

wyoming <- dat2 %>%
  select(date, state_abbrev, positive, death, totalTestResults) %>%
  filter(state_abbrev == 'WY')

wyoming[is.na(wyoming)] = 0

wyomingTrend <- plot_ly(wyoming, x = ~date, y = ~totalTestResults, type = 'scatter', mode = 'lines+markers', name = 'Tests Given')

wyomingTrend <- wyomingTrend %>%
  add_trace(y = ~positive, type = 'scatter', mode = 'lines+markers', name = 'Positive') %>%
  add_trace(y= ~death, type = 'scatter', mode = 'lines+markers', name = 'Deaths') %>%
  layout(hovermode = 'x', title = "COVID-19 Trend in Wyoming", xaxis = list(title = "Date"), yaxis = list(title = "Total"))

wyomingTrend