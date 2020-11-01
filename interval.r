# run after gbm.r for calculating average error
sample <- sample.int(n = nrow(data_merge), size=200)
train <- data_merge[sample, ]
test  <- data_merge[-sample, ]
trees = 2000
cases_model=gbm(cases~lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag14+rolling_avg+deaths_rolling_avg+bedminusvent_rolling_avg+monthly_rolling_avg, data=train, n.trees=trees)
deaths_model=gbm(deaths~deaths_lag1+deaths_lag2+deaths_lag3+deaths_lag4+rolling_avg+deaths_rolling_avg+bedminusvent_rolling_avg, data=train, n.trees=trees)
beds_model=gbm(beds~beds_lag1+beds_lag2+beds_lag3+beds_lag4+beds_rolling_avg+rolling_avg+deaths_rolling_avg+bedminusvent_rolling_avg, data=train, n.trees=trees)
vents_model=gbm(vents~vents_lag1+vents_lag2+vents_lag3+vents_lag4+vents_rolling_avg+rolling_avg+deaths_rolling_avg+bedminusvent_rolling_avg, data=train, n.trees=trees)

#runs predictions for previous covid data and compares them to actual data
prediction = predict(cases_model, newdata=test)
error = test$cases - prediction
mean = mean(abs(error))
print(mean)

prediction = predict(deaths_model, newdata=test)
error = test$deaths - prediction
mean = mean(abs(error))
print(mean)

prediction = predict(beds_model, newdata=test)
error = test$beds - prediction
mean = mean(abs(error))
print(mean)

prediction = predict(vents_model, newdata=test)
error = test$vents - prediction
mean = mean(abs(error))
print(mean)