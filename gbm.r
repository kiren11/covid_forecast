library(lubridate)
library(dplyr)
library(gbm)
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
  summarize(cases = sum(COVID_COUNT))

covid_deaths_agg = covid_death_report %>% 
  group_by(date) %>%
  summarize(deaths = sum(covid_deaths))

bedvent_agg = bedvent_report %>% 
  group_by(date) %>%
  summarize(beds = sum(BEDS_ICU_OCCUPIED_COVID_19), vents = sum(VENTS_ALL_USE_COVID_19))

covid_agg<-arrange(covid_agg,date,cases) %>% 
  mutate(lag1=lag(cases),
         lag2=lag(cases,2),
         lag3=lag(cases,3),
         lag4=lag(cases,4),
         lag5=lag(cases,5),
         lag6=lag(cases,6),
         lag7=lag(cases,7),
         rolling_avg=(lag1+lag2+lag3+lag4+lag5+lag6+lag7)/7)

data_merge=merge(covid_agg,covid_deaths_agg,by="date",all.x=T)

for(i in 1:8) {
  print(ymd("2020-10-26") + days(i))
  model=gbm(cases~lag1+lag2+lag3+lag4+lag5+lag6+lag7+rolling_avg, data=data_merge)
  future = data.frame(
    date = c(as_date(ymd("2020-10-26") + days(i), origin="1970-01-01")),
    cases = c(NA),
    lag1 = c(data_merge$cases[nrow(data_merge)]),
    lag2 = c(data_merge$cases[nrow(data_merge)-1]),
    lag3 = c(data_merge$cases[nrow(data_merge)-2]),
    lag4 = c(data_merge$cases[nrow(data_merge)-3]),
    lag5 = c(data_merge$cases[nrow(data_merge)-4]),
    lag6 = c(data_merge$cases[nrow(data_merge)-5]),
    lag7 = c(data_merge$cases[nrow(data_merge)-6]),
    rolling_avg = c(NA),
    deaths = c(NA)
  )
  future$rolling_avg=(future$lag1+future$lag2+future$lag3+future$lag4+future$lag5+future$lag6+future$lag7)/7
  prediction = predict(model, newdata=future)
  future$cases=round(prediction)
  data_merge = rbind(data_merge, future)
}

print(tail(data_merge, 15))


