# data
ni <- read.csv("NICases.csv")
df3 <- ts(ni$newCasesByPublishDate)
df3 <- rev(df3)

# Step 1: check if we need take transformation (qualtratics or exponential)
# plot the data
plot(df3, type = "l")
# plot the mean 
Mt <- matrix(df3, 217, 2, byrow = T)
mt <- apply(Mt, 1, mean)
sdt <- apply(Mt,1, sd)
plot(mt, sdt, cex = 1.2, xlab = "Mean", ylab = "Sd")
## Suggested we do need log transformation to stablise the variance 
df3 <- log(df3 + 1)


# Step 2: check if the original series is weakly stationary (plot of data, ACF and ADF test)
acf(df3)
adf.test(df3, k = 17)
## ACF suggested that it is not weakly stationary and ADF is larger than 0.05 as well
## This indicated that we need to check for differencing 

# Step 3: differencing 
par(mfrow = c(2, 2))
par(mai = c(0.8, 0.8, 0.8, 0.7))
plot(df3, type = "l", ylab = "Original series", xlab = "Time")
plot(diff(df3), type = "l", ylab = "First differences", xlab = "Time")
plot(diff(df3, differences = 2), type = "l", ylab = "Second differences", xlab = "Time")
plot(diff(df3, differences = 3), type = "l", ylab = "Second differences", xlab = "Time")
## It seems that the first differencing is better 

# Step 4: plot acf and pacf and check unit root test again for first differencing
diff <- diff(df3)
par(mfrow = c(1, 2))
acf(diff, main = "ACF of differenced data")
pacf(diff, main = "PACF of differenced data")
adf.test(diff, k = 17)
## Suggested first differencing is better 

### Step 1-4: Model Identification (finding d)

# Step 5: get model using auto.arima function
require(forecast)
auto.arima(df3, ic = "aicc", trace = TRUE)
## Suggested (4,1,3), aicc = 994.68

# Step 6: model diagnositc to check correlation, residual acf and normality
m1 <- arima(df3, order = c(4,1,3))
resid <- residuals(m1)
Box.test(resid, lag = 6, type = "Ljung-Box")
# reject h0 thus correlated 
acf(resid)
pacf(resid)
jarque.bera.test(resid)
# reject h0 thus not normally distirbuted  

# second round: Step 5 get model using arima function to get smallest 
model <- matrix(NA, 9, 9)
for (i in 0:8) {
  for (j in 0:8) {
    fit <- Arima(df3, order = c(i, 1, j), include.mean = TRUE)
    model[(i+1), (j+1)] <- fit$aicc
  }
}
knitr::kable(
  cbind(0:8, model), booktabs = TRUE, col.names = c("p/q", 0, 1, 2, 3, 4,5,6,7,8)
)
# p = 4, q = 3, AICC = 5753.894


# Second Round: Step 6 model diagnositc to check correlation, residual acf and normality
m1 <- arima(df3, order = c(4,1,3))
checkresiduals(m1)
resid <- residuals(m1)
Box.test(resid, lag = 6, type = "Ljung-Box") # lag of 6 used since ln(434)
# not reject h0 thus not correlated 
acf(resid)
pacf(resid)
jarque.bera.test(resid)
# reject h0 thus normally distirbuted  
## perfect fit 
require(tseries)
### Thus the final model is ARIMA(4,1,3)

# Step 7: Forcasting 
model_ni <- arima(df3, order = c(7,1,5))
forecast(model_ni, h = 20)
autoplot(forecast(model_ni, h = 20))
