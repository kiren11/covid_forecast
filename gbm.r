library(lubridate)
library(dplyr)
setwd("F:/Grad School/Epidemics/covid_forecast/") #change this to your directory

#getting data
covid_report=read.csv("covid_report.csv", fileEncoding="UTF-8-BOM")
covid_death_report=read.csv("covid_report_death_date_agegrp.csv", fileEncoding="UTF-8-BOM")
bedvent_report=read.csv("covid_report_bedvent_date.csv", fileEncoding="UTF-8-BOM")

#normalizing date columns
covid_report$date=ymd(substr(covid_report$DATE,1,10))
covid_death_report$date=ymd(substr(covid_death_report$date,1,10))
bedvent_report$date=ymd(substr(bedvent_report$DATE,1,10))

#sorting by date
covid_agg = covid_report %>% 
  group_by(date) %>%
  summarize(N = sum(COVID_COUNT))

covid_deaths_agg = covid_death_report %>% 
  group_by(date) %>%
  summarize(N = sum(covid_deaths))

bedvent_agg = bedvent_report %>% 
  group_by(date) %>%
  summarize(beds = sum(BEDS_ICU_OCCUPIED_COVID_19), vents = sum(VENTS_ALL_USE_COVID_19))