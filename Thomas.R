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
  select(date, state, positive, death) %>%
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
  ggplot(counties, mapping= aes(x=County, y=Confirmed, fill=Population)) + 
  geom_bar(stat="identity", position = "dodge") + coord_flip() + ylab("Number of Confirmed Cases") + ggtitle("Confirmed cases in Iowa by County")

IA_county



#Looking at Covid Testing

US <- plot_ly(dat, x = ~state, y = ~positive, type = 'bar', mode = 'lines+markers', name = 'Positive Tests')
US <- US %>%
  add_trace(y=~totalTestResults, name="Total Tested") %>%
layout(title= "Total Number Tests vs Positive Tests in U.S", xaxis = list(title = "State"), yaxis = list(title = " Number of Tests"))

US

dat %>% 
  group_by(state) %>%
  plot_ly(x=~date, y=~totalTestResults, group=~state, type = 'bar', color = ~state, mode='lines+markers') 




