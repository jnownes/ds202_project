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
  mutate(increase_cases_last_3_days = diff(-positive),
         increase_tests_last_3_days = diff(-totalTestResults)) %>%
  filter(date == '2020-05-01')

plot_dat$`cases_3_days_ago` = plot_dat$positive - plot_dat$increase_cases_last_3_days
plot_dat$`tests_3_days_ago` = plot_dat$totalTestResults - plot_dat$increase_tests_last_3_days

plot_dat$cases_percent_increase = ((plot_dat$positive - plot_dat$`cases_3_days_ago`)/(plot_dat$`cases_3_days_ago`))*100
plot_dat$tests_percent_increase = ((plot_dat$totalTestResults - plot_dat$`tests_3_days_ago`)/(plot_dat$`tests_3_days_ago`))*100

plot_dat$positive_test_percent_last_3_days = (plot_dat$increase_cases_last_3_days)/(plot_dat$increase_tests_last_3_days)*100


plot_dat$`Stay at Home Order` = as.character(plot_dat$`Stay at Home Order`)
plot_dat$`Stay at Home Order`[plot_dat$`Stay at Home Order` == '-'] = "Never Enacted"
plot_dat$`Stay at Home Order` = factor(plot_dat$`Stay at Home Order`,levels = c("Never Enacted","Lifted","Rolled Back to High Risk Groups","High-Risk Groups","Statewide"))

plot_dat$`State Is Easing Social Distancing Measures` = as.character(plot_dat$`State Is Easing Social Distancing Measures`)
plot_dat$`State Is Easing Social Distancing Measures`[plot_dat$`State Is Easing Social Distancing Measures` == "-"] = "No"
plot_dat$`State Is Easing Social Distancing Measures` = factor(plot_dat$`State Is Easing Social Distancing Measures`, levels = c("No", "Yes"))



library(plotly)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_dat$hover = plot_dat %>%
  with(paste(state,
             
             "\n\nTotal cases by May 1:", round((positive/population)*1000,digits = 2), "cases per 1000 people",
             "\nTotal cases by April 28:", round((`cases_3_days_ago`/population)*1000,digits = 2), "cases per 1000 people",
             "\nPercentage increase in cases from 4/28-5/1:", sprintf("%.2f%%", cases_percent_increase),
             
             "\n\nTotal tests by May 1:", round((totalTestResults/population)*1000,digits = 2), "tests per 1000 people",
             "\nTotal tests by April 28:", round((`tests_3_days_ago`/population)*1000,digits = 2), "tests per 1000 people",
             "\nPercentage increase in tests from 4/28-5/1:",sprintf("%.2f%%",tests_percent_increase),
             
             "\n\nPercentage of positive tests from 4/28-5/1:", sprintf("%.2f%%",positive_test_percent_last_3_days),
             
             "\n\nStay at Home Order Status:", `Stay at Home Order`,
             "\nDate order enacted:", format(stay_at_home_date, format = "%B %d"),
             "\nEasing Social Restrictions:", `State Is Easing Social Distancing Measures`
  ))

library(RColorBrewer)
nfactor_orders = 5
orders <- brewer.pal(n = nfactor_orders,name = "Greys")
names(orders) = levels(plot_dat$`Stay at Home Order`)
plot_dat$test_color_orders = as.numeric(plot_dat$`Stay at Home Order`)
Z_Breaks = function(n){
  CUTS = seq(0,1,length.out=n+1)
  rep(CUTS,ifelse(CUTS %in% 0:1,1,2))
}

colorScale_orders = data.frame(z=Z_Breaks(nfactor_orders),
                         col=rep(orders,each=2),stringsAsFactors=FALSE)

nfactor_easing = 2
easing <- gray.colors(nfactor_easing, start = 0, end = 1)
names(easing) = levels(plot_dat$`State Is Easing Social Distancing Measures`)
plot_dat$test_color_easing = as.numeric(plot_dat$`State Is Easing Social Distancing Measures`)
Z_Breaks = function(n){
  CUTS = seq(0,1,length.out=n+1)
  rep(CUTS,ifelse(CUTS %in% 0:1,1,2))
}

colorScale_easing = data.frame(z=Z_Breaks(nfactor_easing),
                               col=rep(easing,each=2),stringsAsFactors=FALSE)

fig = plot_dat %>%
  plot_geo(locationmode = 'USA-states') %>%
  
  add_trace(
  type = "choropleth",
  name = "Cases/1000",
  z = ~(positive/population)*1000,
  text = ~hover,
  locations = ~state_abbrev,
  color = ~(positive/population)*1000,
  colorscale = 'Reds',
  hoverinfo = "text",
  colorbar = list(title = "Cases Per 1000 People", y = 0.8, len = .6)
) %>%
  
  add_trace(
  type = "choropleth",
  name = "Percentage Increase in Cases",
  z = ~cases_percent_increase,
  text = ~hover,
  locations = ~state_abbrev,
  color = ~cases_percent_increase,
  colorscale = 'Reds',
  hoverinfo = "text",
  colorbar = list(title = "Percentage Increase\nin Cases From 4/28-5/1", y = 0.8, len = .6)
) %>%
  
  add_trace(
    type = "choropleth",
    name = "Tests/1000",
    z = ~(totalTestResults/population)*1000,
    text = ~hover,
    locations = ~state_abbrev,
    color = ~(totalTestResults/population)*1000,
    colorscale = 'Greens',
    reversescale = TRUE,
    hoverinfo = "text",
    colorbar = list(title = "Tests Per 1000 People", y = .8, len = .6)
  ) %>%  
  
  add_trace(
    type = "choropleth",
    name = "Tests/1000 Percentage Increase",
    z = ~tests_percent_increase,
    text = ~hover,
    locations = ~state_abbrev,
    color = ~tests_percent_increase,
    colorscale = 'Greens',
    reversescale = TRUE,
    hoverinfo = "text",
    colorbar = list(title = "Percentage Increase\nin Tests From 4/28-5/1", y = .8, len = .6)
  ) %>%
  
  add_trace(
    type = "choropleth",
    name = "Positive Test Percentage",
    z = ~positive_test_percent_last_3_days,
    text = ~hover,
    locations = ~state_abbrev,
    color = ~positive_test_percent_last_3_days,
    colorscale = 'Reds',
    hoverinfo = "text",
    colorbar = list(title = "Percentage of Tests\nReturning Positive\nFrom 4/28-5/1", y = .8, len = .6)
  ) %>%
  
  add_trace(
    type = "choropleth",
    name = "Stay at Home Orders",
    locations = ~state_abbrev,
    text = ~hover,
    hoverinfo = "text",
    z = plot_dat$test_color_orders,
    colorscale=colorScale_orders,
    colorbar=list(title = "Stay at Home Order Status", tickvals=c(1.4,2.2,3,3.8,4.6), ticktext=names(orders), y = .8, len = .6)
  ) %>%
  
  add_trace(
    type = "choropleth",
    name = "Easing Social Restrictions",
    locations = ~state_abbrev,
    text = ~hover,
    hoverinfo = "text",
    z = plot_dat$test_color_easing,
    colorscale=colorScale_easing,
    colorbar=list(title = "Is the State Easing\nSocial Restrictions?", tickvals=c(1.25,1.75), ticktext=names(easing), y = .8, len = .6)
  ) %>%
  
  
  layout(
  geo = g,
  updatemenus = list(
    list(
      y = 0.8,
      buttons = list(
        list(
          method = "update",
          args = list(list(visible = c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                      list(title = "Total Cases/1000 People by May 1")),
          label = "Total Cases/1000"
        ),
        
        list(
          method = "update",
          args = list(list(visible = c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                      list(title = "Percentage Increase in Cases From April 28-May 1")),
          label = "Percentage Increase in Cases"),
        
        list(
          method = "update",
          args = list(list(visible = c(FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)),
                      list(title = "Total Tests/1000 People by May 1")),
          label = "Total Tests/1000"),  
        
        list(
            method = "update",
            args = list(list(visible = c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE)),
                        list(title = "Percentage Increase in Tests")),
            label = "Percentage Increase in Tests"),
        
        list(
          method = "update",
          args = list(list(visible = c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE)),
                      list(title = "Percentage of Testing Returning Positive From April 28-May 1")),
          label = "Positive Testing Percentage"),
        
        list(
          method = "update",
          args = list(list(visible = c(FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)),
                      list(title = "Stay at Home Order Status")),
          label = "Stay at Home Order Status"),
        
        list(
          method = "update",
          args = list(list(visible = c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)),
                      list(title = "States that are Easing\nSocial Restrictions")),
          label = "Easing Social Restrictions?")
      )
    )
  )
)
fig


# Sources
# See proposal for raw data source (Kaggle)
# https://www.kff.org/health-costs/issue-brief/state-data-and-policy-actions-to-address-coronavirus/ (social distancing by state)
# US Census (population by state)
# https://www.littler.com/publication-press/publication/stay-top-stay-home-list-statewide (stay at home order enactment dates)