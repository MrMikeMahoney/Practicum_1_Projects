## Reading in my dataset >> Barley production from 1884 to 1939 >> Dtatset: 
#https://datamarket.com/data/set/22rj/annual-barley-yields-per-acre-in-england-wales-1884-1939#!ds=22rj&display=line

barley_test <- read.table("C:/Users/Mike/Downloads/annual-barley-yields-per-acre-in.csv",
                          nrows =56, header = TRUE, sep = ",", row.names = 1)
##### 2 >> Converting to Time Series
barley_ts <- ts(barley_test, frequency = 1,  start=c(1884))
plot.ts(barley_ts, type="l")

##### 3 >> Plotting the acf for barley_ts
par(mfrow=c(1,1)) # So I only get one plot at a time
#barley_acf <- acf(barley_test$barley_yields)
acf(barley_test$barley_yields)
pacf(barley_test$barley_yields)
##### 4 >> KPSS and ADF test
library(tseries)
library(urca)
#KPSS Test
barley_kpss <- kpss.test(barley_test$barley_yields, null="Trend");barley_kpss
barley_kpss$statistic; ## Reject Null >> barley_kpss$statistic is larger than p-value

barley_kpss_urca <- ur.kpss(barley_test$barley_yields, type="tau")#;barley_kpss_urca
summary(barley_kpss_urca) ## Reject Null at .1 and .05 sig levels

#ADF test
barley_adf <- adf.test(barley_test$barley_yields, alternative = "stationary");barley_adf # Fail to reject H0

barley_adf_urca <- ur.df(barley_test$barley_yields)
summary(barley_adf_urca) # Reject the Null Hypo

##### 5 >> Differencing the Data 
barley_diff <- diff(barley_test$barley_yields);barley_diff
class(barley_diff)
year <- 1885:1939
# Creating a new data.frame with our differenced data
barley_diff_df <- data.frame(barley_diff, row.names = year)
#barley_diff <- diff(barley_test);barley_diff

### a >> Plotting the differenced data
barley_diff_ts <- ts(barley_diff, frequency = 1, start = c(1885))
plot.ts(barley_diff_ts, type="l")
### b >> Plotting ACF for diffeenced time series
acf(barley_diff_ts)
acf(barley_diff_df$barley_diff) ## This is identical to the previous >> I just did double work

### c >> Applying KPSS and ADF to the barley_diff_df (or barley_diff_ts >> its the same thing)
barley_diff_kpss <- kpss.test(barley_diff_df$barley_diff, null="Trend");
barley_diff_kpss$statistic  ## Fail to reject the H0

barley_diff_kpss_urca <- ur.kpss(barley_diff_df$barley_diff, type="tau")#;barley_kpss_urca
summary(barley_diff_kpss_urca) # Fail to reject the Null Hypo

barley_diff_adf <- adf.test(barley_diff_df$barley_diff, alternative = "stationary");barley_diff_adf # reject the H0

barley_diff_adf_urca <- ur.df(barley_diff_df$barley_diff)
summary(barley_diff_adf_urca) # reject the H0

##### 6 >> ARCH test >> Acutally performed in GRETL
#write.csv(barley_diff_ts, "C:/Users/Mike/Documents/Practicum 1/barley_diff_ts.csv")#, sep=",")
#write.csv(barley_diff_df, "C:/Users/Mike/Documents/Practicum 1/barley_diff_df.csv")

##### 7 >> PACF (diff data)
par(mfrow=c(2,1)) 
pacf(barley_diff_ts)
acf(barley_diff_ts)

##### 8 >> ARIMA models 
library(lmtest)
## Very handy function, I found to compute the aicc >> I did not create this I found it
aicc = function(model){
  n = model$nobs
  p = length(model$coef)
  aicc = model$aic + 2*p*(p+1)/(n-p-1)
  return(aicc)
}

barley_arima_1 <- arima(barley_diff_ts, order = c(1,0,0), seasonal = list(order=c(1,0,0), period=1), include.mean=FALSE)
barley_arima_1_info <- summary(barley_arima_1)
attributes(barley_arima_1)
barley_arima_1$aic
barley_arima_1$AICc
AIC(barley_arima_1)
BIC(barley_arima_1)
AIC(barley_arima_1, k =log(length(barley_diff_ts)))
coeftest(barley_arima_1)
confint(barley_arima_1)
fitarima(barley_arima_1)
barley_arima_1$
  
barley_arima_1$aic
BIC(barley_arima_1)
aicc(barley_arima_1)

barley_arima_2 <- arima(barley_diff_ts, order = c(1,1,0), seasonal = list(order=c(1,1,0), period=1), include.mean=FALSE)
barley_arima_2_info <- summary(barley_arima_2)

barley_arima_3 <- arima(barley_diff_ts, order = c(1,1,1), seasonal = list(order=c(1,1,1), period=1), include.mean=FALSE)
barley_arima_3_info <- summary(barley_arima_3)

barley_arima_4 <- arima(barley_diff_ts, order = c(0,0,1), seasonal = list(order=c(0,0,1), period=1), include.mean=FALSE)
barley_arima_4_info <- summary(barley_arima_4)

barley_arima_5 <- arima(barley_diff_ts, order = c(1,1,2), seasonal = list(order=c(1,1,2), period=1), include.mean=FALSE)
barley_arima_5_info <- summary(barley_arima_5)

barley_arima_6 <- arima(barley_diff_ts, order = c(2,1,2), seasonal = list(order=c(2,1,2), period=1), include.mean=FALSE)
barley_arima_6_info <- summary(barley_arima_6)

barley_arima_7 <- arima(barley_diff_ts, order = c(0,0,2), seasonal = list(order=c(0,0,2), period=1), include.mean=FALSE)
barley_arima_7_info <- summary(barley_arima_7)

barley_arima_7$aic
BIC(barley_arima_7)
aicc(barley_arima_7)

arima_aic <- c(AIC(barley_arima_1), AIC(barley_arima_2), AIC(barley_arima_3), AIC(barley_arima_4), AIC(barley_arima_5), AIC(barley_arima_6), barley_arima_7$aic)
arima_aicc <- c(aicc(barley_arima_1), aicc(barley_arima_2), aicc(barley_arima_3), aicc(barley_arima_4), aicc(barley_arima_5), aicc(barley_arima_6), aicc(barley_arima_7))
arima_bic <- c(BIC(barley_arima_1), BIC(barley_arima_2), BIC(barley_arima_3), BIC(barley_arima_4), BIC(barley_arima_5), BIC(barley_arima_6), BIC(barley_arima_7))
arima_metrics <- data.frame(arima_aic, arima_aicc, arima_bic)
colnames(arima_metrics) <- c("AIC", "AICc", "BIC")
rownames(arima_metrics) <- c("ARIMA(1,0,0)","ARIMA(1,1,0)", "ARIMA(1,1,1)", "ARIMA(0,0,1)", "ARIMA(1,1,2)", "ARIMA(2,1,2)", "ARIMA(0,0,2)")

### B >> Plotting observed vs fitted data
par(mfrow=c(1,1))
# Arima 1
plot(barley_arima_1$residuals, type="l", col="red")
par(new=TRUE)
plot(barley_diff_ts, type="l", col = "black")
mtext("Arima(1,0,0) Residuals vs Fitted")

# Arima 2
plot(barley_arima_2$residuals, type="l", col="red")
par(new=TRUE)
plot(barley_diff_ts, type="l", col = "black")
mtext("Arima(1,1,0) Residuals vs Fitted")

# Arima 3
plot(barley_arima_3$residuals, type="l", col="red")
par(new=TRUE)
plot(barley_diff_ts, type="l", col = "black")
mtext("Arima(1,1,1) Residuals vs Fitted")

# Arima 4
plot(barley_arima_4$residuals, type="l", col="red")
par(new=TRUE)
plot(barley_diff_ts, type="l", col = "black")
mtext("Arima(0,0,1) Residuals vs Fitted")

# Arima 5
plot(barley_arima_5$residuals, type="l", col="red")
par(new=TRUE)
plot(barley_diff_ts, type="l", col = "black")
mtext("Arima(1,1,2) Residuals vs Fitted")

# Arima 6
plot(barley_arima_6$residuals, type="l", col="red")
par(new=TRUE)
plot(barley_diff_ts, type="l", col = "black")
mtext("Arima(2,1,2) Residuals vs Fitted")

# Arima 7
plot(barley_arima_7$residuals, type="l", col="red")
par(new=TRUE)
plot(barley_diff_ts, type="l", col = "black")
mtext("Arima(0,0,2) Residuals vs Fitted")

### D >> Forcasting with ARIMA model 4
library(forcats)
forecast_arima_4 <- forecast(barley_arima_4, h = 6)
plot(forecast_arima_4)

################### Dataset 2
## Air temperatures >> https://datamarket.com/data/set/22li/mean-monthly-air-temperature-deg-f-nottingham-castle-1920-1939#!ds=22li&display=line
temp <- read.csv("C:/Users/Mike/Downloads/mean-monthly-air-temperature-deg.csv", header=TRUE, row.names = 1)
tem <- read.csv("C:/Users/Mike/Downloads/mean-monthly-air-temperature-deg.csv", header=TRUE)
##### 9 - A >> Plot time series
temp_ts <- ts(temp, frequency = 12, start=c(1920))
ts.plot(temp_ts, type="l")

##### D >> Applying the KPSS and ADF test
temp_kpss <- kpss.test(temp_ts, null="Trend");temp_kpss # Fail to reject the H0, stat less than p-value
temp_kpss_urca <- ur.kpss(temp_ts,  type="tau")
summary(temp_kpss_urca) # Fail to reject the H0, test-stat less than any critical value


temp_adf <- adf.test(temp_ts, alternative = "stationary");temp_adf # Reject the H0 >> 
temp_adf_urca <- ur.df(temp$air.temp)
summary(temp_adf_urca) # Reject H0 >> Astronomically low P-value

##### E >> Weights for Holt Winters Smoothing
temp_hw_smooth_1 <- ets(temp_ts, model = "AAA")
temp_hw_smooth_1_error <- summary(temp_hw_smooth_1)
checkresiduals(temp_hw_smooth_1)

temp_hw_smooth_2 <- ets(temp_ts, model = "MAA")
temp_hw_smooth_2_error <- summary(temp_hw_smooth_2)
checkresiduals(temp_hw_smooth_2)

temp_hw_smooth_3 <- ets(temp_ts, model = "MNA")
temp_hw_smooth_3_error <- summary(temp_hw_smooth_3)
checkresiduals(temp_hw_smooth_3)

temp_hw_smooth_4 <- ets(temp_ts, model = "MMM")
temp_hw_smooth_4_error <- summary(temp_hw_smooth_4)
checkresiduals(temp_hw_smooth_4)

temp_hw_smooth_5 <- ets(temp_ts, model = "ANA")
temp_hw_smooth_5_error <- summary(temp_hw_smooth_5)
checkresiduals(temp_hw_smooth_5)

temp_hw_smooth_6 <- ets(temp_ts, model = "ZZZ")
temp_hw_smooth_6_error <- summary(temp_hw_smooth_6)
checkresiduals(temp_hw_smooth_6)

error_row <- NULL
temp_hw_error_df <- rbind(temp_hw_smooth_1_error, temp_hw_smooth_2_error, temp_hw_smooth_3_error, temp_hw_smooth_4_error, temp_hw_smooth_5_error, temp_hw_smooth_6_error)
temp_hw_aic <- rbind(temp_hw_smooth_1$aic, temp_hw_smooth_2$aic, temp_hw_smooth_3$aic, temp_hw_smooth_4$aic, temp_hw_smooth_5$aic, temp_hw_smooth_6$aic)
temp_hw_bic <- rbind(temp_hw_smooth_1$bic, temp_hw_smooth_2$bic, temp_hw_smooth_3$bic, temp_hw_smooth_4$bic, temp_hw_smooth_5$bic, temp_hw_smooth_6$bic)
colnames(temp_hw_aic) <- "AIC"; colnames(temp_hw_bic) <- "BIC"
temp_hw_error_df <- cbind(temp_hw_error_df, temp_hw_aic, temp_hw_bic)
rownames(temp_hw_error_df) <- c("AAA", "MAA", "MNA", "MMM", "ANA", "ZZZ")

temp_hw_smooth_1$par
temp_hw_smooth_5$par
##### F >> Forecasting with the ANA Holt winters model
temp_hw_smooth_5_forecast <- forecast(temp_hw_smooth_5, h = 12)
plot(temp_hw_smooth_5_forecast)

##### H >> UCM 
library(rucm)
temp_ucm <- ucm(temp_ts~0, temp_ts, level=TRUE, season=TRUE, season.length=12);temp_ucm

temp_ucm <- ucm(temp_ts~0, temp_ts, season.length = 12, season=TRUE, slope = TRUE)
temp_ucm$est.var.season
temp_ucm$est.var.slope
temp_ucm$est.var.level
temp_ucm$irr.var[1]
temp_ucm_stats <- data.frame(temp_ucm$est.var.season, temp_ucm$est.var.slope, temp_ucm$irr.var[1])
rownames(temp_ucm_stats) <- "temp_ucm"
colnames(temp_ucm_stats) <- c("seasonality", "trend", "irregular")
temp_ucm_stats

temp_ucm_predict <- predict(temp_ucm$model, n.ahead = 12)
ts.plot(temp_ts, temp_ucm_predict)


time_col <- 1:240
temp_df <- data.frame(time_col, temp)
colnames(temp_df) <- c("time", "temp")

tem$test <- tem$Month
tem$test <-paste(tem$test,"-01", sep="")

colnames(tem) <- c("time", "temp", "date")
#write.csv(tem, "C:/Users/Mike/Documents/Practicum 1/temp_df_sas.csv")
