Appendix
Time Series Code in R
# Weekly Case Count Prediction
cr = read.csv("weekly_1418.csv")

data = ts(cr[1:222,2], start=c(2014,1), frequency = 52)
log_data = log(data)
plot(data, main="Weekly Case Count of Gun Violence in the US from 2014 Jan. to 2018 Mar."
     , ylab="Number", xlab="Year")
axis(1,at=seq(2014,2019,1/4), labels=NA)

train = ts(cr[1:182,2], start=c(2014,1), frequency = 52)
test = ts(cr[183:222,2], start=c(2017,26), frequency = 52)

# log transformation
log_cr = log(train)
acf(log_cr, lag.max = 150)
pacf(log_cr, lag.max = 150)

# seasonal difference
dlog_cr = diff(log_cr,52)
acf(dlog_cr, lag.max = 150)
pacf(dlog_cr, lag.max = 150)
plot(dlog_cr, main="Data after Taking Log Transformation and Seasonal Difference", ylab="Number", xlab="Year")

# regular difference
ddlog_cr = diff(dlog_cr)
par(mfrow = c(1,1))
acf(ddlog_cr, main="ACF for Transformed Data", lag.max = 150) 
pacf(ddlog_cr, main="PACF for Transformed Data", lag.max = 150) 

# check stationary
adf.test(ddlog_cr) #stationary

# build model without p,q,d orders
model_try = arima(ddlog_cr, order = c(0, 0, 0), seasonal = list(order=c(0, 0, 0), period=52))
model_try #aic = -165.49
coeftest(model_try)

model_try1 = arima(log_cr, order = c(0, 1, 0), seasonal = list(order=c(0, 1, 0), period=52))
model_try1 #aic = -167.48
par(mfrow = c(1,2))
acf(model_try1$residuals, main="ACF for Residual")
# MA(10)
pacf(model_try1$residuals, main="PACF for Residual")
# AR(4)

# MA(10)
ma = arima(log_cr, order = c(0, 1, 10), seasonal = list(order=c(0, 1, 0), period=52))
ma
z# AIC = -217.74
coeftest(ma) #ma5- ma8 not significant
Box.test(ma$residuals, type = 'Ljung',lag = 60)
# p-value = 0.0003114

# AR(4)
ar = arima(log_cr, order = c(4, 1, 0), seasonal = list(order=c(0, 1, 0), period=52))
ar
# AIC = -215.17
coeftest(ar) #all significant
Box.test(ar$residuals, type = 'Ljung',lag = 60)
# p-value = 0.0007609
abs(polyroot(c(1, -ar$coef[1:4])))
##

# Comparison fitted values and actual values

#MA(10) = ma
out_fit = ts(exp(log_cr-ma$residuals), start = c(2014,1), frequency = 52)
plot(train, ylim=c(min(train)-200, max(out_fit)+200), xlim = c(2014,2018),xlab = 'Year', ylab = 'Injured Number', main = 'Comparision between Fitted Values and Actual Values with ARIMA(0,1,10)*(0,1,0)',lwd=2)
points(out_fit, col = 'red', pch = 2,cex=0.5)
lines(out_fit, col = 'red')
legend.text=c("Actual values", "Fitted values")
legend("bottomright", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# AR(2)
ar_fit = ts(exp(log_cr-ar$residuals), start = c(2014,1), frequency = 52)
plot(train, ylim=c(min(train)-200, max(ar_fit)+200), xlim=c(2014,2018),xlab = 'Year', ylab = 'Injured Number', main = 'Comparision between Fitted Values and Actual Values with ARIMA(3,1,0)*(0,1,0)',lwd=2)
points(ar_fit, col = 'red', pch = 2,cex=0.5)
lines(ar_fit, col = 'red')
legend.text=c("Actual values", "Fitted values")
legend("bottomright", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# Prediction from 2017 (test data)
# MA(10)
ma_pred = predict(ma, 40)
ma_pred_ts = ts(exp(ma_pred$pred), start = c(2017,26), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(500,1750), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Prediction and Actual Values from 2020 to 2022 with ARIMA(0,1,1)*(0,1,0)',lwd=2)
points(ma_pred_ts, col = 'red', pch = 2,cex=0.5)
lines(ma_pred_ts, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)

# AR(4)
ar_pred = predict(ar, 40)
ar_pred_ts = ts(exp(ar_pred$pred), start = c(2017,26), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(500,1750), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Prediction and Actual Values from 2020 to 2022 with ARIMA(3,1,0)*(0,1,0)',lwd=2)
points(ar_pred_ts, col = 'red', pch = 2,cex=0.5)
lines(ar_pred_ts, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)

# add intervention variables
n = length(data)
n1 = 182
pulse = ts(c(rep(0, n1), 1, rep(0, n-n1-1)), start=2014, frequency=52) 
step=ts(c(rep(0, n1), rep(1, n-n1)), start=2014, frequency=52) 
# MA(10)
order=c(0,1,10)
seasonal=list(order=c(0,1,0), period=52)
# no intervention
ma00= arimax(log_data, order=order, seasonal=seasonal, method="ML")
ma00
# AIC = -269.18
# with step function
ma01= arimax(log_data, order=order, seasonal=seasonal, xtransf=data.frame(step), transfer=list(c(0,0)), method="ML")
ma01
# AIC = -269.29
# with pulse function
ma10= arimax(log_data, order=order, seasonal=seasonal, xtransf=data.frame(pulse), transfer=list(c(1,0)), method="ML")
ma10
# AIC = -267.12
# with step and pulse function
ma11= arimax(log_data, order=order, seasonal=seasonal, xtransf=data.frame(pulse, step), transfer=list(c(1,0), c(0,0)), method="ML")
ma11
# AIC = -272.74

# MA(10), step and pulse Function has the lowest AIC

# AR(4)
order1=c(4,1,0)
seasonal1=list(order=c(0,1,0), period=52)
# no intervention
ar00= arimax(log_data, order=order1, seasonal=seasonal1, method="ML")
ar00
# AIC = -259.23
# with step function
ar01= arimax(log_data, order=order1, seasonal=seasonal1, xtransf=data.frame(step), transfer=list(c(0,0)), method="ML")
ar01
# AIC = -257.3
# with pulse function
ar10= arimax(log_data, order=order1, seasonal=seasonal1, xtransf=data.frame(pulse), transfer=list(c(1,0)), method="ML")
ar10
# AIC = -255.54
# with step and pulse function
ar11= arimax(log_data, order=order1, seasonal=seasonal1, xtransf=data.frame(pulse, step), transfer=list(c(1,0), c(0,0)), method="ML")
ar11
# AIC = -254.87

# AR(4), No Intervention has the lowest AIC


# intervention variables
# MA(10)
ma_tc = filter(step,pulse , filter=ma11$coef[2],method='recursive',side=1)*ma11$coef[3]
plot(ma_tc, type='l', xlim=c(2014,2019), ylab='impact',main="Intervention Change with ARIMA(0,1,1)*(0,1,0)")

ma_pred1 = arima(log_data, order=order, seasonal=seasonal, xreg=data.frame(ma_tc))
ma_pred1 
# AIC = -267.18
ma_fit1 = ts(exp(log_data-ma11$residuals), start = c(2014,1), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(500,1750), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Fitted Values and Actual Values from 2020 to 2022 with ARIMA(0,1,1)*(0,1,0)',lwd=2)
points(ma_fit1, col = 'red', pch = 2,cex=0.5)
lines(ma_fit1, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)

# AR(4)
ar_tc = filter(step,pulse, filter=ar11$coef[4],method='recursive',side=1)*ar11$coef[5]
plot(ar_tc, type='l', xlim=c(2014,2019), ylab='impact',main="Intervention Change with ARIMA(3,1,0)*(0,1,0)")

ar_pred1 = arima(log_data, order=order, seasonal=seasonal, xreg=data.frame(ar_tc))
ar_pred1 
# AIC = -267.27
ar_fit1 = ts(exp(log_data-ar10$residuals), start = c(2014,1), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(0,2000), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Fitted Values and Actual Values from 2020 to 2022 with ARIMA(3,1,0)*(0,1,0)',lwd=2)
points(ar_fit1, col = 'red', pch = 2,cex=0.5)
lines(ar_fit1, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# prediction for future
h=44
pulse_new = ts(c(rep(0, n1), 1, rep(0, n+h-n1-1)), start=2017, frequency=52) 
step_new = ts(c(rep(0, n1), rep(1, n+h-n1)), start=2017, frequency=52) 
# MA(10)
ma_tc_new = filter(pulse_new, filter=ma11$coef[2],method='recursive',side=1)*ma11$coef[3]
ma_newdata=data.frame(tc = ma_tc_new[(n+1):(n+h)])
ma_pred_full=predict(ma_pred1, h, newxreg=ma_newdata)

# AR(3)
ar_tc_new = filter(pulse_new, filter=ar11$coef[4],method='recursive',side=1)*ar11$coef[5]
ar_newdata=data.frame(tc = ar_tc_new[(n+1):(n+h)])
ar_pred_full=predict(ar_pred1, h, newxreg=ar_newdata)

cr$date <- as.Date(cr$date, "%Y-%m-%d",)

time = seq(2014, 2018+13/52, by=1/52)
time_new = seq(2018+15/52, 2019+6/52, by=1/52)
par(mfrow = c(2,1))
# plot for MA(10)
plot(time, data,xlab='Year', ylab='Number', xlim=c(2014, 2019), ylim=c(400,1750), type='o', main='Prediction of Case Number with ARIMA(0,1,10)*(0,1,0)',cex=0.5,cex.lab=0.8,cex.axis=0.8, cex.main=1)
axis(1,at=seq(2014,2019,1/4), labels=NA)
points(time, exp(log_data-ma11$residuals), type='o', pch=2, lty=1, col='red',cex=0.5)
points(time_new, exp(ma_pred_full$pred), type='o', pch=3, lty=1, col='blue',cex=0.5)
legend.txt=c("Actual", "Fitted", "Predicted")
legend("bottomright", legend.txt, col=c("black", "red", "blue"), pch=c(1,2,3), lty=c(1,2,1), cex=0.6)

# plot for AR(4)
plot(time, data, xlab='Time', ylab='Number', xlim=c(2014, 2019), ylim=c(500,1750), type='o', main='Prediction of Case Number with ARIMA(4,1,0)*(0,1,0)',cex=0.5)
axis(1,at=seq(2014,2019,1/4), labels=NA)
points(time, exp(log_data-ar11$residuals), type='o', pch=2, lty=1, col='red',cex=0.5)
points(time_new, exp(ar_pred_full$pred), type='o', pch=3, lty=1, col='blue',cex=0.5)
legend.txt=c("Actual", "Fitted", "Predicted")
legend("bottomright", legend.txt, col=c("black", "red", "blue"), pch=c(1,2,3), lty=c(1,2,1), cex=0.5)

# Weekly Number of Injured Prediction
cr = read.csv("weekly_inj.csv")

data = ts(cr[1:222,2], start=c(2014,1), frequency = 52)
log_data = log(data)
plot(data, main="Weekly Injuried Number of Gun Violence in the US from 2014 Jan. to 2018 Mar."
     , ylab="Number", xlab="Year")
axis(1,at=seq(2014,2019,1/4), labels=NA)

train = ts(cr[1:182,2], start=c(2014,1), frequency = 52)
test = ts(cr[183:222,2], start=c(2017,26), frequency = 52)

# log transformation
log_cr = log(train)
acf(log_cr, lag.max = 150)
pacf(log_cr, lag.max = 150)

# seasonal difference
dlog_cr = diff(log_cr,52)
acf(dlog_cr, lag.max = 150)
pacf(dlog_cr, lag.max = 150)
plot(dlog_cr, main="Data after Taking Log Transformation and Seasonal Difference", ylab="Number", xlab="Year")

# regular difference
ddlog_cr = diff(dlog_cr)
par(mfrow = c(1,1))
acf(ddlog_cr, main="ACF for Transformed Data", lag.max = 150) 
pacf(ddlog_cr, main="PACF for Transformed Data", lag.max = 150) 

# check stationary
adf.test(ddlog_cr) #stationary

# build model without p,q,d orders
model_try = arima(ddlog_cr, order = c(0, 0, 0), seasonal = list(order=c(0, 0, 0), period=52))
model_try #aic = -67.53
coeftest(model_try)

model_try1 = arima(log_cr, order = c(0, 1, 0), seasonal = list(order=c(0, 1, 0), period=52))
model_try1 #aic = -69.51
par(mfrow = c(1,2))
acf(model_try1$residuals, main="ACF for Residual")
# MA(1)
pacf(model_try1$residuals, main="PACF for Residual")
# AR(5)

# MA(1)
ma = arima(log_cr, order = c(0, 1, 1), seasonal = list(order=c(0, 1, 0), period=52))
ma
# AIC = -139.16
coeftest(ma) # all significant
Box.test(ma$residuals, type = 'Ljung',lag = 60)
# p-value = 0.0328

# AR(5)
ar = arima(log_cr, order = c(5, 1, 0), seasonal = list(order=c(0, 1, 0), period=52))
ar
# AIC = -128.65
coeftest(ar) #all significant
Box.test(ar$residuals, type = 'Ljung',lag = 60)
# p-value = 0.0007609
abs(polyroot(c(1, -ar$coef[1:4])))
# 1.314146 1.244589 1.244589 1.314146

# Comparison fitted values and actual values

#MA(1) 
out_fit = ts(exp(log_cr-ma$residuals), start = c(2014,1), frequency = 52)
plot(train, ylim=c(min(train)-200, max(out_fit)+200), xlim = c(2014,2018),xlab = 'Year', ylab = 'Injured Number', main = 'Comparision between Fitted Values and Actual Values with ARIMA(0,1,10)*(0,1,0)',lwd=2)
points(out_fit, col = 'red', pch = 2,cex=0.5)
lines(out_fit, col = 'red')
legend.text=c("Actual values", "Fitted values")
legend("bottomright", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# AR(5)
ar_fit = ts(exp(log_cr-ar$residuals), start = c(2014,1), frequency = 52)
plot(train, ylim=c(min(train)-200, max(ar_fit)+200), xlim=c(2014,2018),xlab = 'Year', ylab = 'Injured Number', main = 'Comparision between Fitted Values and Actual Values with ARIMA(3,1,0)*(0,1,0)',lwd=2)
points(ar_fit, col = 'red', pch = 2,cex=0.5)
lines(ar_fit, col = 'red')
legend.text=c("Actual values", "Fitted values")
legend("bottomright", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# Prediction from 2017 (test data)
# MA(1)
ma_pred = predict(ma, 40)
ma_pred_ts = ts(exp(ma_pred$pred), start = c(2017,26), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(0,1000), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Prediction and Actual Values from 2020 to 2022 with ARIMA(0,1,1)*(0,1,0)',lwd=2)
points(ma_pred_ts, col = 'red', pch = 2,cex=0.5)
lines(ma_pred_ts, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)

# AR(5)
ar_pred = predict(ar, 40)
ar_pred_ts = ts(exp(ar_pred$pred), start = c(2017,26), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(0,1000), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Prediction and Actual Values from 2020 to 2022 with ARIMA(3,1,0)*(0,1,0)',lwd=2)
points(ar_pred_ts, col = 'red', pch = 2,cex=0.5)
lines(ar_pred_ts, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# add intervention variables
n = length(data)
n1 = 182
pulse = ts(c(rep(0, n1), 1, rep(0, n-n1-1)), start=2014, frequency=52) 
step=ts(c(rep(0, n1), rep(1, n-n1)), start=2014, frequency=52) 
# MA(1)
order=c(0,1,1)
seasonal=list(order=c(0,1,0), period=52)
# no intervention
ma00= arimax(log_data, order=order, seasonal=seasonal, method="ML")
ma00
# AIC = -166.99
# with step function
ma01= arimax(log_data, order=order, seasonal=seasonal, xtransf=data.frame(step), transfer=list(c(0,0)), method="ML")
ma01
# AIC = -167.79
# with pulse function
ma10= arimax(log_data, order=order, seasonal=seasonal, xtransf=data.frame(pulse), transfer=list(c(1,0)), method="ML")
ma10
# AIC = -164.54
# with step and pulse function
ma11= arimax(log_data, order=order, seasonal=seasonal, xtransf=data.frame(pulse, step), transfer=list(c(1,0), c(0,0)), method="ML")
ma11
# AIC = -167.66

# MA(1), step Function has the lowest AIC

# AR(5)
order1=c(5,1,0)
seasonal1=list(order=c(0,1,0), period=52)
# no intervention
ar00= arimax(log_data, order=order1, seasonal=seasonal1, method="ML")
ar00
# AIC = -158.32
# with step function
ar01= arimax(log_data, order=order1, seasonal=seasonal1, xtransf=data.frame(step), transfer=list(c(0,0)), method="ML")
ar01
# AIC = -156.84
# with pulse function
ar10= arimax(log_data, order=order1, seasonal=seasonal1, xtransf=data.frame(pulse), transfer=list(c(1,0)), method="ML")
ar10
# AIC = -156.47
# with step and pulse function
ar11= arimax(log_data, order=order1, seasonal=seasonal1, xtransf=data.frame(pulse, step), transfer=list(c(1,0), c(0,0)), method="ML")
ar11
# AIC = -157.13

# AR(5), No Intervention has the lowest AIC


# intervention variables
# MA(1)
ma_tc = filter(step, pulse, filter=ma11$coef[2],method='recursive',side=1)*ma11$coef[3]
plot(ma_tc, type='l', xlim=c(2014,2019), ylab='impact',main="Intervention Change with ARIMA(0,1,1)*(0,1,0)")

ma_pred1 = arima(log_data, order=order, seasonal=seasonal, xreg=data.frame(ma_tc))
ma_pred1 
# AIC = -169.29
ma_fit1 = ts(exp(log_data-ma11$residuals), start = c(2014,1), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(0,1000), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Fitted Values and Actual Values from 2020 to 2022 with ARIMA(0,1,1)*(0,1,0)',lwd=2)
points(ma_fit1, col = 'red', pch = 2,cex=0.5)
lines(ma_fit1, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)

# AR(5)
ar_tc = filter(step,pulse, filter=ar11$coef[4],method='recursive',side=1)*ar11$coef[5]
plot(ar_tc, type='l', xlim=c(2014,2019), ylab='impact',main="Intervention Change with ARIMA(3,1,0)*(0,1,0)")

ar_pred1 = arima(log_data, order=order, seasonal=seasonal, xreg=data.frame(ar_tc))
ar_pred1 
# AIC = -167.08
ar_fit1 = ts(exp(log_data-ar11$residuals), start = c(2014,1), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(0,1000), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Fitted Values and Actual Values from 2020 to 2022 with ARIMA(3,1,0)*(0,1,0)',lwd=2)
points(ar_fit1, col = 'red', pch = 2,cex=0.5)
lines(ar_fit1, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# prediction for future
h=44
pulse_new = ts(c(rep(0, n1), 1, rep(0, n+h-n1-1)), start=2017, frequency=52) 
step_new = ts(c(rep(0, n1), rep(1, n+h-n1)), start=2017, frequency=52) 
# MA(1)
ma_tc_new = filter(pulse_new, filter=ma11$coef[2],method='recursive',side=1)*ma11$coef[3]
ma_newdata=data.frame(tc = ma_tc_new[(n+1):(n+h)])
ma_pred_full=predict(ma_pred1, h, newxreg=ma_newdata)

# AR(5)
ar_tc_new = filter(pulse_new, filter=ar11$coef[4],method='recursive',side=1)*ar11$coef[5]
ar_newdata=data.frame(tc = ar_tc_new[(n+1):(n+h)])
ar_pred_full=predict(ar_pred1, h, newxreg=ar_newdata)

time = seq(2014, 2018+13/52, by=1/52)
time_new = seq(2018+15/52, 2019+6/52, by=1/52)
par(mfrow = c(1,1))
# plot for MA(10)
plot(time, data,xlab='Year', ylab='Number', xlim=c(2014, 2019), ylim=c(0,1000), type='o', main='Prediction of Injured Number with ARIMA(0,1,1)*(0,1,0)',cex=0.5,cex.lab=0.8,cex.axis=0.8, cex.main=1)
axis(1,at=seq(2014,2019,1/4), labels=NA)
points(time, exp(log_data-ma10$residuals), type='o', pch=2, lty=1, col='red',cex=0.5)
points(time_new, exp(ma_pred_full$pred), type='o', pch=3, lty=1, col='blue',cex=0.5)
legend.txt=c("Actual", "Fitted", "Predicted")
legend("bottomright", legend.txt, col=c("black", "red", "blue"), pch=c(1,2,3), lty=c(1,2,1), cex=0.6)

# plot for AR(5)
plot(time, data, xlab='Time', ylab='Number', xlim=c(2014, 2019), ylim=c(0,1000), type='o', main='Prediction of Injured Number with ARIMA(5,1,0)*(0,1,0)',cex=0.5)
axis(1,at=seq(2014,2019,1/4), labels=NA)
points(time, exp(log_data-ar00$residuals), type='o', pch=2, lty=1, col='red',cex=0.5)
points(time_new, exp(ar_pred_full$pred), type='o', pch=3, lty=1, col='blue',cex=0.5)
legend.txt=c("Actual", "Fitted", "Predicted")
legend("bottomright", legend.txt, col=c("black", "red", "blue"), pch=c(1,2,3), lty=c(1,2,1), cex=0.5)

# Weekly Number of Killed Prediction
cr = read.csv("weekly_kil.csv")

data = ts(cr[1:222,2], start=c(2014,1), frequency = 52)
log_data = log(data)
plot(data, main="Weekly Killed Number of Gun Violence in the US from 2014 Jan. to 2018 Mar."
     , ylab="Number", xlab="Year")
axis(1,at=seq(2014,2019,1/4), labels=NA)

train = ts(cr[1:182,2], start=c(2014,1), frequency = 52)
test = ts(cr[183:222,2], start=c(2017,26), frequency = 52)

# log transformation
log_cr = log(train)
acf(log_cr, lag.max = 150)
pacf(log_cr, lag.max = 150)

# seasonal difference
dlog_cr = diff(log_cr,52)
acf(dlog_cr, lag.max = 150)
pacf(dlog_cr, lag.max = 150)

# regular difference
ddlog_cr = diff(dlog_cr)
plot(ddlog_cr, main="Data after Taking Log Transformation and Seasonal Difference", ylab="Number", xlab="Year")

par(mfrow = c(1,1))
acf(ddlog_cr, main="ACF for Transformed Data", lag.max = 150) 
pacf(ddlog_cr, main="PACF for Transformed Data", lag.max = 150) 

# check stationary
adf.test(ddlog_cr) #stationary

# build model without p,q,d orders
model_try = arima(ddlog_cr, order = c(0, 0, 0), seasonal = list(order=c(0, 0, 0), period=52))
model_try #aic = -102.53
coeftest(model_try)

model_try1 = arima(log_cr, order = c(0, 1, 0), seasonal = list(order=c(0, 1, 0), period=52))
model_try1 #aic = -104.48
par(mfrow = c(1,2))
acf(model_try1$residuals, main="ACF for Residual")
# MA(11)
pacf(model_try1$residuals, main="PACF for Residual")
# AR(10)

# MA(11)
ma = arima(log_cr, order = c(0, 1, 11), seasonal = list(order=c(0, 1, 0), period=52))
ma
# AIC = -177.84
coeftest(ma) #ma3- ma9 not significant
Box.test(ma$residuals, type = 'Ljung',lag = 60)
# p-value = 0.3583

# AR(10)
ar = arima(log_cr, order = c(10, 1, 0), seasonal = list(order=c(0, 1, 0), period=52))
ar
# AIC = -181.36
coeftest(ar) #all significant
Box.test(ar$residuals, type = 'Ljung',lag = 60)
# p-value = 0.6595
abs(polyroot(c(1, -ar$coef[1:4])))
# 1.067439 1.035127 1.035127 1.067439

# Comparison fitted values and actual values

# MA(11)
out_fit = ts(exp(log_cr-ma$residuals), start = c(2014,1), frequency = 52)
plot(train, ylim=c(min(train)-50, max(out_fit)+50), xlim = c(2014,2018),xlab = 'Year', ylab = 'Injured Number', main = 'Comparision between Fitted Values and Actual Values with ARIMA(0,1,10)*(0,1,0)',lwd=2)
points(out_fit, col = 'red', pch = 2,cex=0.5)
lines(out_fit, col = 'red')
legend.text=c("Actual values", "Fitted values")
legend("bottomright", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# AR(10)
ar_fit = ts(exp(log_cr-ar$residuals), start = c(2014,1), frequency = 52)
plot(train, ylim=c(min(train)-50, max(ar_fit)+50), xlim=c(2014,2018),xlab = 'Year', ylab = 'Injured Number', main = 'Comparision between Fitted Values and Actual Values with ARIMA(3,1,0)*(0,1,0)',lwd=2)
points(ar_fit, col = 'red', pch = 2,cex=0.5)
lines(ar_fit, col = 'red')
legend.text=c("Actual values", "Fitted values")
legend("bottomright", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# Prediction from 2017 (test data)
# MA(11)
ma_pred = predict(ma, 40)
ma_pred_ts = ts(exp(ma_pred$pred), start = c(2017,26), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(0,500), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Prediction and Actual Values with ARIMA(0,1,11)*(0,1,0)',lwd=2)
points(ma_pred_ts, col = 'red', pch = 2,cex=0.5)
lines(ma_pred_ts, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)

# AR(4)
ar_pred = predict(ar, 40)
ar_pred_ts = ts(exp(ar_pred$pred), start = c(2017,26), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(0,500), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Prediction and Actual Values with ARIMA(10,1,0)*(0,1,0)',lwd=2)
points(ar_pred_ts, col = 'red', pch = 2,cex=0.5)
lines(ar_pred_ts, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# add intervention variables
n = length(data)
n1 = 182
pulse = ts(c(rep(0, n1), 1, rep(0, n-n1-1)), start=2014, frequency=52) 
step=ts(c(rep(0, n1), rep(1, n-n1)), start=2014, frequency=52) 
# MA(11)
order=c(0,1,11)
seasonal=list(order=c(0,1,0), period=52)
# no intervention
ma00= arimax(log_data, order=order, seasonal=seasonal, method="ML")
ma00
# AIC = -227.93
# with step function
ma01= arimax(log_data, order=order, seasonal=seasonal, xtransf=data.frame(step), transfer=list(c(0,0)), method="ML")
ma01
# AIC = -237.99
# with pulse function
ma10= arimax(log_data, order=order, seasonal=seasonal, xtransf=data.frame(pulse), transfer=list(c(1,0)), method="ML")
ma10
# AIC = -226.55
# with step and pulse function
ma11= arimax(log_data, order=order, seasonal=seasonal, xtransf=data.frame(pulse, step), transfer=list(c(1,0), c(0,0)), method="ML")
ma11
# AIC = -234.22

# MA(11), step Function has the lowest AIC

# AR(10)
order1=c(10,1,0)
seasonal1=list(order=c(0,1,0), period=52)
# no intervention
ar00= arimax(log_data, order=order1, seasonal=seasonal1, method="ML")
ar00
# AIC = -236.7
# with step function
ar01= arimax(log_data, order=order1, seasonal=seasonal1, xtransf=data.frame(step), transfer=list(c(0,0)), method="ML")
ar01
# AIC = -244.84
# with pulse function
ar10= arimax(log_data, order=order1, seasonal=seasonal1, xtransf=data.frame(pulse), transfer=list(c(1,0)), method="ML")
ar10
# AIC = -236.75
# with step and pulse function
ar11= arimax(log_data, order=order1, seasonal=seasonal1, xtransf=data.frame(pulse, step), transfer=list(c(1,0), c(0,0)), method="ML")
ar11
# AIC = -243.09

# AR(10), step has the lowest AIC


# intervention variables
# MA(11)
ma_tc = filter(step , filter=ma01$coef[2],method='recursive',side=1)*ma01$coef[3]
plot(ma_tc, type='l', xlim=c(2014,2019), ylab='impact',main="Intervention Change with ARIMA(0,1,11)*(0,1,0)")

ma_pred1 = arima(log_data, order=order, seasonal=seasonal, xreg=data.frame(ma_tc))
ma_pred1 
# AIC = -237.9
ma_fit1 = ts(exp(log_data-ma11$residuals), start = c(2014,1), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(0,500), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Fitted Values and Actual Values from 2020 to 2022 with ARIMA(0,1,1)*(0,1,0)',lwd=2)
points(ma_fit1, col = 'red', pch = 2,cex=0.5)
lines(ma_fit1, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)

# AR(10)
ar_tc = filter(step, filter=ar01$coef[4],method='recursive',side=1)*ar01$coef[5]
plot(ar_tc, type='l', xlim=c(2014,2019), ylab='impact',main="Intervention Change with ARIMA(3,1,0)*(0,1,0)")

ar_pred1 = arima(log_data, order=order, seasonal=seasonal, xreg=data.frame(ar_tc))
ar_pred1 
# AIC = -237.47
ar_fit1 = ts(exp(log_data-ar10$residuals), start = c(2014,1), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(0,500), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Fitted Values and Actual Values from 2020 to 2022 with ARIMA(3,1,0)*(0,1,0)',lwd=2)
points(ar_fit1, col = 'red', pch = 2,cex=0.5)
lines(ar_fit1, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# prediction for future
h=44
pulse_new = ts(c(rep(0, n1), 1, rep(0, n+h-n1-1)), start=2017, frequency=52) 
step_new = ts(c(rep(0, n1), rep(1, n+h-n1)), start=2017, frequency=52) 
# MA(11)
ma_tc_new = filter(step_new, filter=ma01$coef[2],method='recursive',side=1)*ma01$coef[3]
ma_newdata=data.frame(tc = ma_tc_new[(n+1):(n+h)])
ma_pred_full=predict(ma_pred1, h, newxreg=ma_newdata)

# AR(10)
ar_tc_new = filter(step_new, filter=ar01$coef[4],method='recursive',side=1)*ar01$coef[5]
ar_newdata=data.frame(tc = ar_tc_new[(n+1):(n+h)])
ar_pred_full=predict(ar_pred1, h, newxreg=ar_newdata)

time = seq(2014, 2018+13/52, by=1/52)
time_new = seq(2018+15/52, 2019+6/52, by=1/52)
par(mfrow = c(1,1))
# plot for MA(11)
plot(time, data,xlab='Year', ylab='Number', xlim=c(2014, 2019), ylim=c(0,500), type='o', main='Prediction of Killed Number with ARIMA(0,1,11)*(0,1,0)',cex=0.5,cex.lab=0.8,cex.axis=0.8, cex.main=1)
axis(1,at=seq(2014,2019,1/4), labels=NA)
points(time, exp(log_data-ma10$residuals), type='o', pch=2, lty=1, col='red',cex=0.5)
points(time_new, exp(ma_pred_full$pred), type='o', pch=3, lty=1, col='blue',cex=0.5)
legend.txt=c("Actual", "Fitted", "Predicted")
legend("bottomright", legend.txt, col=c("black", "red", "blue"), pch=c(1,2,3), lty=c(1,2,1), cex=0.6)

# plot for AR(10)
plot(time, data, xlab='Time', ylab='Number', xlim=c(2014, 2019), ylim=c(0,500), type='o', main='Prediction of Killed Number with ARIMA(10,1,0)*(0,1,0)',cex=0.5)
axis(1,at=seq(2014,2019,1/4), labels=NA)
points(time, exp(log_data-ar00$residuals), type='o', pch=2, lty=1, col='red',cex=0.5)
points(time_new, exp(ar_pred_full$pred), type='o', pch=3, lty=1, col='blue',cex=0.5)
legend.txt=c("Actual", "Fitted", "Predicted")
legend("bottomright", legend.txt, col=c("black", "red", "blue"), pch=c(1,2,3), lty=c(1,2,1), cex=0.5)

# Weekly Number of Injured in Illinois
cr = read.csv("weekly_inj_il.csv")

data = ts(cr[1:222,2], start=c(2014,1), frequency = 52)
log_data = log(data)
plot(data, main="Weekly Injuried Number of Gun Violence in Illinois from 2014 Jan. to 2018 Mar."
     , ylab="Number", xlab="Year")
axis(1,at=seq(2014,2019,1/4), labels=NA)

train = ts(cr[1:182,2], start=c(2014,1), frequency = 52)
test = ts(cr[183:222,2], start=c(2017,26), frequency = 52)

# log transformation
log_cr = log(train)
acf(log_cr, lag.max = 150)
pacf(log_cr, lag.max = 150)

# seasonal difference
dlog_cr = diff(log_cr,52)
acf(dlog_cr, lag.max = 150)
pacf(dlog_cr, lag.max = 150)
plot(dlog_cr, main="Data after Taking Log Transformation and Seasonal Difference", ylab="Number", xlab="Year")

# regular difference
ddlog_cr = diff(dlog_cr)
par(mfrow = c(1,1))
acf(ddlog_cr, main="ACF for Transformed Data", lag.max = 150) 
pacf(ddlog_cr, main="PACF for Transformed Data", lag.max = 150) 

# check stationary
adf.test(ddlog_cr) #stationary

# build model without p,q,d orders
model_try = arima(ddlog_cr, order = c(0, 0, 0), seasonal = list(order=c(0, 0, 0), period=52))
model_try #aic = 153.47
coeftest(model_try)

model_try1 = arima(log_cr, order = c(0, 1, 0), seasonal = list(order=c(0, 1, 0), period=52))
model_try1 #aic = 151.49
par(mfrow = c(1,2))
acf(model_try1$residuals, main="ACF for Residual")
# MA(1)
pacf(model_try1$residuals, main="PACF for Residual")
# AR(10)

# MA(1)
ma = arima(log_cr, order = c(0, 1, 1), seasonal = list(order=c(0, 1, 0), period=52))
ma
# AIC = 106.05
coeftest(ma) # all significant
Box.test(ma$residuals, type = 'Ljung',lag = 60)
# p-value = 8.428e-05

# AR(10)
ar = arima(log_cr, order = c(10, 1, 0), seasonal = list(order=c(0, 1, 0), period=52))
ar
# AIC = 112.51
coeftest(ar) #all significant
Box.test(ar$residuals, type = 'Ljung',lag = 60)
# p-value = 0.05675
abs(polyroot(c(1, -ar$coef[1:4])))
# 1.169435 1.216886 1.216886 1.169435

# Comparison fitted values and actual values

#MA(1) 
out_fit = ts(exp(log_cr-ma$residuals), start = c(2014,1), frequency = 52)
plot(train, ylim=c(min(train)-50, max(out_fit)+50), xlim = c(2014,2018),xlab = 'Year', ylab = 'Injured Number', main = 'Comparision between Fitted Values and Actual Values with ARIMA(0,1,10)*(0,1,0)',lwd=2)
points(out_fit, col = 'red', pch = 2,cex=0.5)
lines(out_fit, col = 'red')
legend.text=c("Actual values", "Fitted values")
legend("bottomright", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# AR(10)
ar_fit = ts(exp(log_cr-ar$residuals), start = c(2014,1), frequency = 52)
plot(train, ylim=c(min(train)-50, max(ar_fit)+50), xlim=c(2014,2018),xlab = 'Year', ylab = 'Injured Number', main = 'Comparision between Fitted Values and Actual Values with ARIMA(3,1,0)*(0,1,0)',lwd=2)
points(ar_fit, col = 'red', pch = 2,cex=0.5)
lines(ar_fit, col = 'red')
legend.text=c("Actual values", "Fitted values")
legend("bottomright", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# Prediction from 2017 (test data)
# MA(1)
ma_pred = predict(ma, 40)
ma_pred_ts = ts(exp(ma_pred$pred), start = c(2017,26), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(0,150), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Prediction and Actual Values from 2020 to 2022 with ARIMA(0,1,1)*(0,1,0)',lwd=2)
points(ma_pred_ts, col = 'red', pch = 2,cex=0.5)
lines(ma_pred_ts, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)

# AR(10)
ar_pred = predict(ar, 40)
ar_pred_ts = ts(exp(ar_pred$pred), start = c(2017,26), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(0,150), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Prediction and Actual Values from 2020 to 2022 with ARIMA(3,1,0)*(0,1,0)',lwd=2)
points(ar_pred_ts, col = 'red', pch = 2,cex=0.5)
lines(ar_pred_ts, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# add intervention variables
n = length(data)
n1 = 182
pulse = ts(c(rep(0, n1), 1, rep(0, n-n1-1)), start=2014, frequency=52) 
step=ts(c(rep(0, n1), rep(1, n-n1)), start=2014, frequency=52) 
# MA(1)
order=c(0,1,1)
seasonal=list(order=c(0,1,0), period=52)
# no intervention
ma00= arimax(log_data, order=order, seasonal=seasonal, method="ML")
ma00
# AIC = 135.96
# with step function
ma01= arimax(log_data, order=order, seasonal=seasonal, xtransf=data.frame(step), transfer=list(c(0,0)), method="ML")
ma01
# AIC = 135.96
# with pulse function
ma10= arimax(log_data, order=order, seasonal=seasonal, xtransf=data.frame(pulse), transfer=list(c(1,0)), method="ML")
ma10
# AIC = 138.82
# with step and pulse function
ma11= arimax(log_data, order=order, seasonal=seasonal, xtransf=data.frame(pulse, step), transfer=list(c(1,0), c(0,0)), method="ML")
ma11
# AIC = 139.18

# MA(1), no and step Function has the lowest AIC

# AR(10)
order1=c(10,1,0)
seasonal1=list(order=c(0,1,0), period=52)
# no intervention
ar00= arimax(log_data, order=order1, seasonal=seasonal1, method="ML")
ar00
# AIC = 135.05
# with step function
ar01= arimax(log_data, order=order1, seasonal=seasonal1, xtransf=data.frame(step), transfer=list(c(0,0)), method="ML")
ar01
# AIC = 136.02
# with pulse function
ar10= arimax(log_data, order=order1, seasonal=seasonal1, xtransf=data.frame(pulse), transfer=list(c(1,0)), method="ML")
ar10
# AIC = 138.33
# with step and pulse function
ar11= arimax(log_data, order=order1, seasonal=seasonal1, xtransf=data.frame(pulse, step), transfer=list(c(1,0), c(0,0)), method="ML")
ar11
# AIC = 139.62

# AR(10), No Intervention has the lowest AIC


# intervention variables
# MA(1)
ma_tc = filter(step,pulse , filter=ma11$coef[2],method='recursive',side=1)*ma11$coef[3]
plot(ma_tc, type='l', xlim=c(2014,2019), ylab='impact',main="Intervention Change with ARIMA(0,1,1)*(0,1,0)")

ma_pred1 = arima(log_data, order=order, seasonal=seasonal, xreg=data.frame(ma_tc))
ma_pred1 
# AIC = 135.2
ma_fit1 = ts(exp(log_data-ma11$residuals), start = c(2014,1), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(0,150), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Fitted Values and Actual Values from 2020 to 2022 with ARIMA(0,1,1)*(0,1,0)',lwd=2)
points(ma_fit1, col = 'red', pch = 2,cex=0.5)
lines(ma_fit1, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)

# AR(10)
ar_tc = filter(step,pulse, filter=ar11$coef[4],method='recursive',side=1)*ar11$coef[5]
plot(ar_tc, type='l', xlim=c(2014,2019), ylab='impact',main="Intervention Change with ARIMA(3,1,0)*(0,1,0)")

ar_pred1 = arima(log_data, order=order, seasonal=seasonal, xreg=data.frame(ar_tc))
ar_pred1 
# AIC = 136.13
ar_fit1 = ts(exp(log_data-ar11$residuals), start = c(2014,1), frequency=52)
plot(data, xlim=c(2014,2019), ylim=c(0,150), xlab = 'Year', ylab = 'Crime Number', main = 'Comparision between Fitted Values and Actual Values from 2020 to 2022 with ARIMA(3,1,0)*(0,1,0)',lwd=2)
points(ar_fit1, col = 'red', pch = 2,cex=0.5)
lines(ar_fit1, col = 'red',cex=1.5)
legend.text=c("Actual values", "Predicted values")
legend("bottomleft", legend.text, lty = rep(1,2), col = 1:2, pch = 1:2, cex=0.6)


# prediction for future
h=44
pulse_new = ts(c(rep(0, n1), 1, rep(0, n+h-n1-1)), start=2017, frequency=52) 
step_new = ts(c(rep(0, n1), rep(1, n+h-n1)), start=2017, frequency=52) 
# MA(1)
ma_tc_new = filter(pulse_new, filter=ma11$coef[2],method='recursive',side=1)*ma11$coef[3]
ma_newdata=data.frame(tc = ma_tc_new[(n+1):(n+h)])
ma_pred_full=predict(ma_pred1, h, newxreg=ma_newdata)

# AR(10)
ar_tc_new = filter(pulse_new, filter=ar11$coef[4],method='recursive',side=1)*ar11$coef[5]
ar_newdata=data.frame(tc = ar_tc_new[(n+1):(n+h)])
ar_pred_full=predict(ar_pred1, h, newxreg=ar_newdata)

time = seq(2014, 2018+13/52, by=1/52)
time_new = seq(2018+15/52, 2019+6/52, by=1/52)
par(mfrow = c(1,1))
# plot for MA(1)
plot(time, data,xlab='Year', ylab='Number', xlim=c(2014, 2019), ylim=c(0,150), type='l', main='Prediction of Injured Number in Illinois with ARIMA(0,1,1)*(0,1,0)',cex=0.5,cex.lab=0.8,cex.axis=0.8, cex.main=1)
axis(1,at=seq(2014,2019,1/4), labels=NA)
points(time, exp(log_data-ma10$residuals), type='o', pch=2, lty=1, col='red',cex=0.5)
points(time_new, exp(ma_pred_full$pred), type='o', pch=3, lty=1, col='blue',cex=0.5)
legend.txt=c("Actual", "Fitted", "Predicted")
legend("bottomright", legend.txt, col=c("black", "red", "blue"), pch=c(1,2,3), lty=c(1,2,1), cex=0.6)

# plot for AR(10)
plot(time, data, xlab='Time', ylab='Number', xlim=c(2014, 2019), ylim=c(0,150), type='l', main='Prediction of Injured Number in Illinois with ARIMA(10,1,0)*(0,1,0)',cex=0.5)
axis(1,at=seq(2014,2019,1/4), labels=NA)
points(time, exp(log_data-ar00$residuals), type='o', pch=2, lty=1, col='red',cex=0.5)
points(time_new, exp(ar_pred_full$pred), type='o', pch=3, lty=1, col='blue',cex=0.5)
legend.txt=c("Actual", "Fitted", "Predicted")
legend("bottomright", legend.txt, col=c("black", "red", "blue"), pch=c(1,2,3), lty=c(1,2,1), cex=0.5)
