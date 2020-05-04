library(lubridate)
library(dplyr)
library(ggplot2)

dat = read.csv("us_states_covid19_daily.csv", stringsAsFactors = FALSE)
dat$date = ymd(dat$date)
names(dat)[2] = "state_abbrev"
dat$state_abbrev = as.factor(dat$state_abbrev)
dat$fips = as.factor(dat$fips)
dat = dat %>%
  filter(fips != '60' & fips != '66' & fips != '69' & fips != '72' & fips != '78')
dat$fips = factor(dat$fips, levels= dat$fips, labels=dat$fips)

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

dat = dat %>%
  inner_join(dist, by = "fips")
dat$fips = as.factor(dat$fips)


# https://www.littler.com/publication-press/publication/stay-top-stay-home-list-statewide
# Hard coding in the dates that the stay at home orders were enacted
states = c("Alabama", "Alaska", "Arizona", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "Tennessee", "Texas", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin")
dates = c("April 4, 2020", "March 28, 2020", "March 31, 2020", "March 19, 2020", "March 26, 2020", "March 23, 2020", "March 24, 2020", "April 1, 2020", "April 3, 2020", "April 3, 2020", "March 25, 2020", "March 25, 2020", "March 21, 2020", "March 24, 2020", "March 30, 2020", "March 26, 2020", "March 23, 2020", "April 2, 2020", "March 30, 2020", "March 24, 2020", "March 24, 2020", "March 27, 2020", "April 3, 2020", "April 6, 2020", "March 28, 2020", "April 1, 2020", "March 27, 2020", "March 21, 2020", "March 23, 2020", "March 22, 2020", "March 30, 2020", "March 23, 2020", "March 24, 2020", "March 23, 2020", "April 1, 2020", "March 28, 2020", "April 7, 2020", "March 31, 2020", "April 2, 2020", "March 25, 2020", "March 30, 2020", "March 23, 2020", "March 24, 2020", "March 25, 2020")
stay_at_home = data.frame("state" = states, "stay_at_home_date" = dates)
stay_at_home$stay_at_home_date = mdy(stay_at_home$stay_at_home_date)

dat = dat %>%
  left_join(stay_at_home, by = "state")


dat %>%
  ggplot(aes(x=date,y= positive/population)) + geom_point() + facet_wrap(~state)


plot_dat = dat %>%
  filter(date == '2020-04-28' | date == '2020-05-01') %>%
  group_by(state_abbrev) %>%
  mutate(increase_cases_last_3_days = diff(-positive)) %>%
  filter(date == '2020-05-01')

plot_dat$`3_days_ago` = plot_dat$positive - plot_dat$increase_cases_last_3_days

plot_dat$percent_increase = ((plot_dat$positive - plot_dat$`3_days_ago`)/(plot_dat$`3_days_ago`))*100


plot_dat$`Stay at Home Order` = as.character(plot_dat$`Stay at Home Order`)
plot_dat$`Stay at Home Order`[plot_dat$`Stay at Home Order` == '-'] = "Never Enacted"

plot_dat$`State Is Easing Social Distancing Measures` = as.character(plot_dat$`State Is Easing Social Distancing Measures`)
plot_dat$`State Is Easing Social Distancing Measures`[plot_dat$`State Is Easing Social Distancing Measures` == "-"] = "No"


library(plotly)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_dat$hover = plot_dat %>%
  with(paste(state,
             "\nMay 1:", round((positive/population)*1000,digits = 2), "cases per 1000 people",
             "\nApril 28:", round((`3_days_ago`/population)*1000,digits = 2), "cases per 1000 people",
             "\nPercentage Increase:", sprintf("%.2f%%", percent_increase),
             "\nStay at Home Order Status:", `Stay at Home Order`,
             "\nDate order enacted:", format(stay_at_home_date, format = "%B %d"),
             "\nEasing Social Restrictions:", `State Is Easing Social Distancing Measures`
  ))
             
fig = plot_dat %>%
  plot_geo(locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = ~(positive/population)*1000,
  text = ~hover,
  locations = ~state_abbrev,
  color = ~(positive/population)*1000,
  colors = 'Reds',
  hoverinfo = "text"
)
fig <- fig %>% colorbar(title = "5/1 Cases per 1000 People")
fig <- fig %>% layout(
  title = 'Choropleth Map',
  geo = g
)
fig


fig2 = plot_dat %>%
  plot_geo(locationmode = 'USA-states')
fig2 = fig2 %>% add_trace(
  z = ~percent_increase,
  text = ~hover,
  locations = ~state_abbrev,
  color = ~percent_increase,
  colors = 'Reds',
  hoverinfo = "text"
)
fig2 = fig2 %>% colorbar(title = "Percentage Increase in\nCases per 1000 People\nSince 4/28")
fig2 = fig2 %>% layout(
  title = 'Choropleth Map 2',
  geo = g
)
fig2


# Sources
# See proposal for raw data source (Kaggle)
# https://www.kff.org/health-costs/issue-brief/state-data-and-policy-actions-to-address-coronavirus/ (social distancing by state)
# US Census (population by state)
# https://www.littler.com/publication-press/publication/stay-top-stay-home-list-statewide (stay at home order enactment dates)