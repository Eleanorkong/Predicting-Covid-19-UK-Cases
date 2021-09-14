# data
require(tseries)
require(forecast)
england <- read.csv("EnglandCases.csv")
df1 <- ts(england$newCasesByPublishDate)
df1 <- rev(df1)

# Step 1: check if we need take transformation (qualtratics or exponential)
# plot the data
plot(df1, type = "l")
# plot the mean 
Mt <- matrix(df1, 217, 2, byrow = T)
mt <- apply(Mt, 1, mean)
sdt <- apply(Mt,1, sd)
plot(mt, sdt, cex = 1.2, xlab = "Mean", ylab = "Sd")
## Suggested we do need log transformation to stablise the variance 
df1 <- log(df1 + 1)

# Step 2: check if the original series is weakly stationary (plot of data, ACF and ADF test)
acf(df1)
adf.test(df1, k = 17)
?adf.test
## ACF and ADF test suggested that it is not weakly stationary 
## This indicated that we need to do differencing to stabilize the mean

# Step 3: differencing 
ndiffs(df1, test = "adf")
ndiffs(df1, test = "kpss")

par(mfrow = c(2, 2))
par(mai = c(0.8, 0.8, 0.8, 0.7))
plot(df1, type = "l", ylab = "Original series", xlab = "Time")
plot(diff(df1), type = "l", ylab = "First differences", xlab = "Time")
plot(diff(df1, differences = 2), type = "l", ylab = "Second differences", xlab = "Time")
plot(diff(df1, differences = 3), type = "l", ylab = "Second differences", xlab = "Time")
## It seems that the first differencing is better 

# Step 4: plot acf and pacf and check unit root test again for first differencing
diff <- diff(df1)
par(mfrow = c(1, 2))
acf(diff, main = "ACF of differenced data")
pacf(diff, main = "PACF of differenced data")
adf.test(diff, k = 17)
kpss.test(diff, k = 17)
## adf not stationary, but visually it is okay, so we use auto.arima to try.
library(urca)
k <- trunc((length(diff) - 1)^(1/3))
test <- urca::ur.df(diff, type = "drift", lags = 17)
summary(test)
### Step 1-4: Model Identification (finding d)

# Step 5: get model using auto.arima function
auto.arima(df1, trace = TRUE)
## Suggested (1,1,2), aicc 246.26; and first differencing 

# Step 6: model diagnositc to check correlation, residual acf and normality
m1 <- arima(df1, order = c(1,1,2))
resid <- residuals(m1)
Box.test(resid, lag = 6, type = "Ljung-Box")
# not reject h0 thus not correlated 
acf(resid)
pacf(resid)
jarque.bera.test(resid)
qqnorm(resid)
# reject h0 thus not normally distirbuted  
checkresiduals(m1)

# second round: Step 5 get model using arima function to get smallest 
model <- matrix(NA, 5, 5)
for (i in 0:4) {
  for (j in 0:4) {
    fit <- Arima(df1, order = c(i, 1, j), include.mean = TRUE)
    model[(i+1), (j+1)] <- fit$aicc
  }
}
knitr::kable(
  cbind(0:4, model), booktabs = TRUE, col.names = c("p/q", 0, 1, 2, 3, 4)
)
# p = 4, q = 3, AICC = 241.8237


# Second Round: Step 6 model diagnositc to check correlation, residual acf and normality
m1 <- arima(df1, order = c(4, 1, 3))
resid <- residuals(m1)
Box.test(resid, lag = 6, type = "Ljung-Box")
# not reject h0 thus not correlated 
acf(resid)
pacf(resid)
jarque.bera.test(resid)
qqnorm(resid)
# reject h0 thus not normally distirbuted  
## perfect fit 

### Thus the final model is ARIMA(1,1,2)

# Step 7: Forcasting 
model_e <- arima(df1, order = c(1,1,2))
forecast(model_e, h = 20)
autoplot(forecast(model_e, h = 20))





