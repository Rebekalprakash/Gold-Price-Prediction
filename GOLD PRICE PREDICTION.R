library(yahoofinancer)
library(tseries)
library(forecast)
library(quantmod)
data1= getSymbols('gold',src='yahoo',from='2000-01-01',to='2022-10-31',auto.assign = FALSE)
data1

Gold=ts(data1)
Gold
class(Gold)
plot(Gold)

gold_new=Gold[,c('GOLD.Close')]
differ=diff(gold_new)
differ
adf.test(differ,alternative = "stationary")

lnGold=log(gold_new)
model1 <- arima(lnGold, c(0, 1, 1))
summary(model1)

model2=arima(lnGold,c(17,1,2))
summary(model2)

#Forecast using model
pred <- predict(model1, nmodel1.ahead = 5)

#Converting natural log to decimal values
View(3.456^pred$pred)
arima <- forecast(model1, h=5)
accuracy(arima)

#Plotting the forecast
plot(arima) + abline(h=5)
