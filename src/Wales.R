# data
require(tseries)
require(forecast)
wales <- read.csv("WalesCases.csv")
df4 <- ts(wales$newCasesByPublishDate)
df4 <- rev(df4)
# Step 1: check if we need take transformation (qualtratics or exponential)
# plot the data
plot(df4, type = "l")
# plot the mean 
Mt <- matrix(df4, 217, 2, byrow = T)
mt <- apply(Mt, 1, mean)
sdt <- apply(Mt,1, sd)
plot(mt, sdt, cex = 1.2, xlab = "Mean", ylab = "Sd")
## Suggested we do need log transformation to stablise the variance 
df4 <- log(df4 + 1)


# Step 2: check if the original series is weakly stationary (plot of data, ACF and ADF test)
acf(df4)
adf.test(df4, k = 17)
## ACF suggested that it is not weakly stationary and ADF is larger than 0.05 as well
## This indicated that we need to check for differencing 

# Step 3: differencing 
par(mfrow = c(2, 2))
par(mai = c(0.8, 0.8, 0.8, 0.7))
plot(df4, type = "l", ylab = "Original series", xlab = "Time")
plot(diff(df4), type = "l", ylab = "First differences", xlab = "Time")
plot(diff(df4, differences = 2), type = "l", ylab = "Second differences", xlab = "Time")
plot(diff(df4, differences = 3), type = "l", ylab = "Second differences", xlab = "Time")
## It seems that the first differencing is better 
ndiffs(df4)

# Step 4: plot acf and pacf and check unit root test again for first differencing
diff <- diff(df4)
par(mfrow = c(1, 2))
acf(diff, main = "ACF of differenced data")
pacf(diff, main = "PACF of differenced data")
adf.test(diff, k = 17)
## Suggested first differencing is better 

### Step 1-4: Model Identification (finding d)

# Step 5: get model using auto.arima function
require(forecast)
auto.arima(df4, trace = TRUE)
## Suggested (4,1,5), aicc = 1017.09

# Step 6: model diagnositc to check correlation, residual acf and normality
m1 <- arima(df4, order = c(4,1,5))
checkresiduals(m1)
resid <- residuals(m1)
Box.test(resid, lag = 6, type = "Ljung-Box")
# not reject h0 thus not correlated  
acf(resid)
jarque.bera.test(resid)
qqnorm(resid)
# reject h0 thus normally distirbuted  
## Not perfect fit 

# second round: Step 5 get model using arima function to get smallest 
model <- matrix(NA, 9, 9)
for (i in 0:8) {
  for (j in 0:8) {
    fit <- Arima(df4, order = c(i, 1, j), include.mean = TRUE)
    model[(i+1), (j+1)] <- fit$aicc
  }
}
knitr::kable(
  cbind(0:8, model), booktabs = TRUE, col.names = c("p/q", 0, 1, 2, 3, 4,5,6,7,8)
)
# p = 7, q = 7, AICC = 998.999


# Second Round: Step 6 model diagnositc to check correlation, residual acf and normality
m1 <- arima(df4, order = c(7,1,7))
checkresiduals(m1)
resid <- residuals(m1)
Box.test(resid, lag = 6, type = "Ljung-Box")
# not reject h0 thus not correlated 
acf(resid)
pacf(resid)
jarque.bera.test(resid)
qqnorm(resid)
# reject h0 thus normally distirbuted  
## not perfect fit 

### Thus the final model is ARIMA(4,1,5)

# Step 7: Forcasting 
model_w <- arima(df4, order = c(4,1,5))
forecast(model_w, h = 20)
autoplot(forecast(model_w, h = 20))

