library(lubridate)
library(dplyr)
library(gbm)
library(plotrix)
setwd("F:/Grad School/Epidemics/covid_forecast/") 
today = ymd("2020-10-29") #change this to today's date
trees = 2000

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



#mutating data frames
covid_agg<-arrange(covid_agg,date,cases) %>% 
  mutate(lag1=lag(cases),
         lag2=lag(cases,2),
         lag3=lag(cases,3),
         lag4=lag(cases,4),
         lag5=lag(cases,5),
         lag6=lag(cases,6),
         lag7=lag(cases,7),
         lag8=lag(cases,8),
         lag9=lag(cases,9),
         lag10=lag(cases,10),
         lag11=lag(cases,11),
         lag12=lag(cases,12),
         lag13=lag(cases,13),
         lag14=lag(cases,14),
         lag15=lag(cases,15),
         lag16=lag(cases,16),
         lag17=lag(cases,17),
         lag18=lag(cases,18),
         lag19=lag(cases,19),
         lag20=lag(cases,20),
         lag21=lag(cases,21),
         lag22=lag(cases,22),
         lag23=lag(cases,23),
         lag24=lag(cases,24),
         lag25=lag(cases,25),
         lag26=lag(cases,26),
         lag27=lag(cases,27),
         lag28=lag(cases,28),
         lag29=lag(cases,29),
         lag30=lag(cases,30),
         rolling_avg=(lag1+lag2+lag3+lag4+lag5+lag6+lag7)/7,
         monthly_rolling_avg=(lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+lag13+lag14+lag15+lag16+lag17+lag18+
                           lag19+lag20+lag21+lag22+lag23+lag24+lag25+lag26+lag27+lag28+lag29+lag30)/30)

covid_deaths_agg<-arrange(covid_deaths_agg,date,deaths) %>%
  mutate(deaths=ifelse(is.na(deaths), 0, deaths),
         deaths_lag1=lag(deaths),
         deaths_lag2=lag(deaths,2),
         deaths_lag3=lag(deaths,3),
         deaths_lag4=lag(deaths,4),
         deaths_lag5=lag(deaths,5),
         deaths_lag6=lag(deaths,6),
         deaths_lag7=lag(deaths,7),
         deaths_rolling_avg=(deaths_lag1+deaths_lag2+deaths_lag3+deaths_lag4+deaths_lag5+deaths_lag6+deaths_lag7)/7)

bedvent_agg<-arrange(bedvent_agg,date,beds) %>%
  mutate(beds_lag1=lag(beds),
         beds_lag2=lag(beds,2),
         beds_lag3=lag(beds,3),
         beds_lag4=lag(beds,4),
         beds_lag5=lag(beds,5),
         beds_lag6=lag(beds,6),
         beds_lag7=lag(beds,7),
         beds_rolling_avg=(beds_lag1+beds_lag2+beds_lag3+beds_lag4+beds_lag5+beds_lag6+beds_lag7)/7,
         vents_lag1=lag(vents),
         vents_lag2=lag(vents,2),
         vents_lag3=lag(vents,3),
         vents_lag4=lag(vents,4),
         vents_lag5=lag(vents,5),
         vents_lag6=lag(vents,6),
         vents_lag7=lag(vents,7),
         vents_rolling_avg=(vents_lag1+vents_lag2+vents_lag3+vents_lag4+vents_lag5+vents_lag6+vents_lag7)/7,
         bedminusvent_lag1=lag(beds - vents),
         bedminusvent_lag2=lag(beds - vents,2),
         bedminusvent_lag3=lag(beds - vents,3),
         bedminusvent_lag4=lag(beds - vents,4),
         bedminusvent_lag5=lag(beds - vents,5),
         bedminusvent_lag6=lag(beds - vents,6),
         bedminusvent_lag7=lag(beds - vents,7),
         bedminusvent_rolling_avg=(bedminusvent_lag1+bedminusvent_lag2+bedminusvent_lag3+bedminusvent_lag4+bedminusvent_lag5+bedminusvent_lag6+bedminusvent_lag7)/7)

# f3 - tally of total covid cases per county (need to make this into rolling average)
#countycases <- covid_report %>% group_by(COUNTY_NAME) %>% tally()

# f4 - tally of total covid cases per age group (need to make this into rolling average)
#agecases <- covid_report %>% group_by(AGEGRP) %>% tally()

#merging data frames
data_merge=merge(covid_agg,covid_deaths_agg,by="date",all.x=T)
data_merge=merge(data_merge,bedvent_agg,by="date",all.x=T)
data_merge[is.na(data_merge)] <- 0

#predicting day-by-day
for(i in 1:7) {
  date = today + days(i)
  print(date)
  cases_model=gbm(cases~lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag14+rolling_avg+deaths_rolling_avg+bedminusvent_rolling_avg+monthly_rolling_avg, data=data_merge, n.trees=trees)
  deaths_model=gbm(deaths~deaths_lag1+deaths_lag2+deaths_lag3+deaths_lag4+rolling_avg+deaths_rolling_avg+bedminusvent_rolling_avg, data=data_merge, n.trees=trees)
  beds_model=gbm(beds~beds_lag1+beds_lag2+beds_lag3+beds_lag4+beds_rolling_avg+rolling_avg+deaths_rolling_avg+bedminusvent_rolling_avg, data=data_merge, n.trees=trees)
  vents_model=gbm(vents~vents_lag1+vents_lag2+vents_lag3+vents_lag4+vents_rolling_avg+rolling_avg+deaths_rolling_avg+bedminusvent_rolling_avg, data=data_merge, n.trees=trees)
  future = data.frame(
    date = c(as_date(date, origin="1970-01-01")),
    cases = c(NA),
    beds = c(NA),
    vents = c(NA),
    deaths = c(NA),
    lag1 = c(data_merge$cases[nrow(data_merge)]),
    lag2 = c(data_merge$cases[nrow(data_merge)-1]),
    lag3 = c(data_merge$cases[nrow(data_merge)-2]),
    lag4 = c(data_merge$cases[nrow(data_merge)-3]),
    lag5 = c(data_merge$cases[nrow(data_merge)-4]),
    lag6 = c(data_merge$cases[nrow(data_merge)-5]),
    lag7 = c(data_merge$cases[nrow(data_merge)-6]),
    lag8 = c(data_merge$cases[nrow(data_merge)-7]),
    lag9 = c(data_merge$cases[nrow(data_merge)-8]),
    lag10 = c(data_merge$cases[nrow(data_merge)-9]),
    lag11 = c(data_merge$cases[nrow(data_merge)-10]),
    lag12 = c(data_merge$cases[nrow(data_merge)-11]),
    lag13 = c(data_merge$cases[nrow(data_merge)-12]),
    lag14 = c(data_merge$cases[nrow(data_merge)-13]),
    lag15 = c(data_merge$cases[nrow(data_merge)-14]),
    lag16 = c(data_merge$cases[nrow(data_merge)-15]),
    lag17 = c(data_merge$cases[nrow(data_merge)-16]),
    lag18 = c(data_merge$cases[nrow(data_merge)-17]),
    lag19 = c(data_merge$cases[nrow(data_merge)-18]),
    lag20 = c(data_merge$cases[nrow(data_merge)-19]),
    lag21 = c(data_merge$cases[nrow(data_merge)-20]),
    lag22 = c(data_merge$cases[nrow(data_merge)-21]),
    lag23 = c(data_merge$cases[nrow(data_merge)-22]),
    lag24 = c(data_merge$cases[nrow(data_merge)-23]),
    lag25 = c(data_merge$cases[nrow(data_merge)-24]),
    lag26 = c(data_merge$cases[nrow(data_merge)-25]),
    lag27 = c(data_merge$cases[nrow(data_merge)-26]),
    lag28 = c(data_merge$cases[nrow(data_merge)-27]),
    lag29 = c(data_merge$cases[nrow(data_merge)-28]),
    lag30 = c(data_merge$cases[nrow(data_merge)-29]),
    deaths_lag1 = c(data_merge$deaths[nrow(data_merge)]),
    deaths_lag2 = c(data_merge$deaths[nrow(data_merge)-1]),
    deaths_lag3 = c(data_merge$deaths[nrow(data_merge)-2]),
    deaths_lag4 = c(data_merge$deaths[nrow(data_merge)-3]),
    deaths_lag5 = c(data_merge$deaths[nrow(data_merge)-4]),
    deaths_lag6 = c(data_merge$deaths[nrow(data_merge)-5]),
    deaths_lag7 = c(data_merge$deaths[nrow(data_merge)-6]),
    beds_lag1 = c(data_merge$beds[nrow(data_merge)]),
    beds_lag2 = c(data_merge$beds[nrow(data_merge)-1]),
    beds_lag3 = c(data_merge$beds[nrow(data_merge)-2]),
    beds_lag4 = c(data_merge$beds[nrow(data_merge)-3]),
    beds_lag5 = c(data_merge$beds[nrow(data_merge)-4]),
    beds_lag6 = c(data_merge$beds[nrow(data_merge)-5]),
    beds_lag7 = c(data_merge$beds[nrow(data_merge)-6]),
    vents_lag1 = c(data_merge$vents[nrow(data_merge)]),
    vents_lag2 = c(data_merge$vents[nrow(data_merge)-1]),
    vents_lag3 = c(data_merge$vents[nrow(data_merge)-2]),
    vents_lag4 = c(data_merge$vents[nrow(data_merge)-3]),
    vents_lag5 = c(data_merge$vents[nrow(data_merge)-4]),
    vents_lag6 = c(data_merge$vents[nrow(data_merge)-5]),
    vents_lag7 = c(data_merge$vents[nrow(data_merge)-6]),
    bedminusvent_lag1 = c(data_merge$beds[nrow(data_merge)] - data_merge$vents[nrow(data_merge)]),
    bedminusvent_lag2 = c(data_merge$beds[nrow(data_merge)-1] - data_merge$vents[nrow(data_merge)-1]),
    bedminusvent_lag3 = c(data_merge$beds[nrow(data_merge)-2] - data_merge$vents[nrow(data_merge)-2]),
    bedminusvent_lag4 = c(data_merge$beds[nrow(data_merge)-3] - data_merge$vents[nrow(data_merge)-3]),
    bedminusvent_lag5 = c(data_merge$beds[nrow(data_merge)-4] - data_merge$vents[nrow(data_merge)-4]),
    bedminusvent_lag6 = c(data_merge$beds[nrow(data_merge)-5] - data_merge$vents[nrow(data_merge)-5]),
    bedminusvent_lag7 = c(data_merge$beds[nrow(data_merge)-6] - data_merge$vents[nrow(data_merge)-6]),
    rolling_avg = c(NA),
    deaths_rolling_avg = c(NA),
    beds_rolling_avg = c(NA),
    vents_rolling_avg = c(NA),
    monthly_rolling_avg = c(NA),
    bedminusvent_rolling_avg = c(NA)
  )
  future$rolling_avg=(future$lag1+future$lag2+future$lag3+future$lag4+future$lag5+future$lag6+future$lag7)/7
  future$monthly_rolling_avg=(future$lag1+future$lag2+future$lag3+future$lag4+future$lag5+future$lag6+future$lag7+
                                future$lag8+future$lag9+future$lag10+future$lag11+future$lag12+future$lag13+future$lag14+
                                future$lag15+future$lag16+future$lag17+future$lag18+future$lag19+future$lag20+future$lag21+
                                future$lag22+future$lag23+future$lag24+future$lag25+future$lag26+future$lag27+future$lag28+future$lag29+future$lag30)/30
  future$deaths_rolling_avg=(future$deaths_lag1+future$deaths_lag2+future$deaths_lag3+future$deaths_lag4+future$deaths_lag5+future$deaths_lag6+future$deaths_lag7)/7
  future$beds_rolling_avg=(future$beds_lag1+future$beds_lag2+future$beds_lag3+future$beds_lag4+future$beds_lag5+future$beds_lag6+future$beds_lag7)/7
  future$vents_rolling_avg=(future$vents_lag1+future$vents_lag2+future$vents_lag3+future$vents_lag4+future$vents_lag5+future$vents_lag6+future$vents_lag7)/7
  future$bedminusvent_rolling_avg = (future$bedminusvent_lag1+future$bedminusvent_lag2+future$bedminusvent_lag3+future$bedminusvent_lag4+future$bedminusvent_lag5+future$bedminusvent_lag6+future$bedminusvent_lag7)/7
  prediction = predict(cases_model, newdata=future)
  future$cases=round(prediction)
  prediction = predict(deaths_model, newdata=future)
  future$deaths=round(prediction)
  prediction = predict(beds_model, newdata=future)
  future$beds=round(prediction)
  prediction = predict(vents_model, newdata=future)
  future$vents=round(prediction)
  data_merge = rbind(data_merge, future)
}

print(tail(data_merge, 7))
plot(data_merge$cases, type="o")

#summary of the model
#summary(cases_model)