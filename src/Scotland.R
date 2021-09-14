require(tseries)
require(forecast)
# data
scotland <- read.csv("ScotlandCases.csv")
df2 <- ts(scotland$newCasesByPublishDate)
df2 <- rev(df2)
# Step 1: check if we need take transformation (qualtratics or exponential)
# plot the data
plot(df2, type = "l")
# plot the mean 
Mt <- matrix(df2, 217, 2, byrow = T)
mt <- apply(Mt, 1, mean)
sdt <- apply(Mt,1, sd)
plot(mt, sdt, cex = 1.2, xlab = "Mean", ylab = "Sd")
## Suggested we do need log transformation to stablise the variance 
df2 <- log(df2 + 1)

# Step 2: check if the original series is weakly stationary (plot of data, ACF and ADF test)
acf(df2)
adf.test(df2, k = 17)
## ACF suggested that it is not weakly stationary and ADF is larger than 0.05 as well
## This indicated that we need to check for differencing 

# Step 3: differencing 
par(mfrow = c(2, 2))
par(mai = c(0.8, 0.8, 0.8, 0.7))
plot(df2, type = "l", ylab = "Original series", xlab = "Time")
plot(diff(df2), type = "l", ylab = "First differences", xlab = "Time")
plot(diff(df2, differences = 2), type = "l", ylab = "Second differences", xlab = "Time")
plot(diff(df2, differences = 3), type = "l", ylab = "Second differences", xlab = "Time")
## It seems that the first differencing is better 

# Step 4: plot acf and pacf and check unit root test again for first differencing
diff <- diff(df2)
par(mfrow = c(1, 2))
acf(diff, main = "ACF of differenced data")
pacf(diff, main = "PACF of differenced data")
adf.test(diff, k = 17)
## adf not stationary, but visually it is okay, so we use auto.arima to try.

### Step 1-4: Model Identification (finding d)

# Step 5: get model using auto.arima function
auto.arima(df2)
## Suggested (1,1,4), aicc = 306.77

# Step 6: model diagnositc to check correlation, residual acf and normality
m1 <- arima(df2, order = c(1,1,4))
resid <- residuals(m1)
Box.test(resid, lag = 6, type = "Ljung-Box")
# not reject h0 thus not correlated 
acf(resid)
pacf(resid)
jarque.bera.test(resid)
qqnorm(resid)
# reject h0 thus not normally distirbuted  
## Not perfect fit 

# second round: Step 5 get model using arima function to get smallest 
model <- matrix(NA, 5, 5)
for (i in 0:4) {
  for (j in 0:4) {
    fit <- Arima(df2, order = c(i, 1, j), include.mean = TRUE)
    model[(i+1), (j+1)] <- fit$aicc
  }
}
knitr::kable(
  cbind(0:4, model), booktabs = TRUE, col.names = c("p/q", 0, 1, 2, 3, 4)
)
# p = 2, q = 3, AICC = 286.9777


# Second Round: Step 6 model diagnositc to check correlation, residual acf and normality
m1 <- arima(df2, order = c(2,1,3))
checkresiduals(m1)
resid <- residuals(m1)
Box.test(resid, lag = 6, type = "Ljung-Box")
# not reject h0 thus not correlated 
acf(resid)
pacf(resid)
jarque.bera.test(resid)
qqnorm(resid)
# reject h0 thus not normally distirbuted but qq plot seems okay 
## perfect fit 

### Thus the final model is ARIMA(2,1,3)

# Step 7: Forcasting 
model_s <- arima(df2, order = c(2,1,3))
forecast(model_s, h = 20)
autoplot(forecast(model_s, h = 20))

