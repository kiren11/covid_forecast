# run after gbm.r for calculating average error
cases_model=gbm(cases~lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag14+rolling_avg+deaths_rolling_avg+bedminusvent_rolling_avg+monthly_rolling_avg, data=data_merge)
deaths_model=gbm(deaths~deaths_lag1+deaths_lag2+deaths_lag3+deaths_lag4+rolling_avg+deaths_rolling_avg+bedminusvent_rolling_avg, data=data_merge)
beds_model=gbm(beds~beds_lag1+beds_lag2+beds_lag3+beds_lag4+beds_rolling_avg+rolling_avg+deaths_rolling_avg+bedminusvent_rolling_avg, data=data_merge)
vents_model=gbm(vents~vents_lag1+vents_lag2+vents_lag3+vents_lag4+vents_rolling_avg+rolling_avg+deaths_rolling_avg+bedminusvent_rolling_avg, data=data_merge)

#runs predictions for previous covid data and compares them to actual data
prediction = predict(cases_model, data=data_merge)
error = data_merge$cases[8:236] - prediction[8:236]
mean = mean(abs(error))
print(mean)

prediction = predict(deaths_model, data=data_merge)
error = data_merge$deaths[8:236] - prediction[8:236]
mean = mean(abs(error))
print(mean)

prediction = predict(beds_model, data=data_merge)
error = data_merge$beds[8:236] - prediction[8:236]
mean = mean(abs(error))
print(mean)

prediction = predict(vents_model, data=data_merge)
error = data_merge$vents[8:236] - prediction[8:236]
mean = mean(abs(error))
print(mean)
