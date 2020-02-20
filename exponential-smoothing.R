knitr::opts_chunk$set(echo = TRUE)

data <- read.csv("organic-traffic.csv", header = T)
head(data)

#Convert "factor" to "Date" according to cell formatting in CSV file
data[,1] <- as.Date(data[,1], format = "%m/%d/%y")
#Convert vector of "Organic Sessions" into time series object
Organic_Traffic <- ts(data[,2], start = c(2014,1), end = c(2018,6), frequency = 12)
#Avoid scientific notation for y-axis values
#options(scipen=999)
#Plot "Organic Traffic" data
plot(Organic_Traffic, main = "Organic Traffic", ylab = "Sessions", ylim = c(0, 700000))

#Decompose data into seasonal, trend and irregular components using classical decomposition
fit_decompose <- decompose(Organic_Traffic, type = "multiplicative")
#Print component data of decomposition 
fit_decompose

#Plot component data of decomposition
plot(fit_decompose)

seasonal.2015 <- fit_decompose$seasonal[13:24]
trend.2015 <- fit_decompose$trend[13:24]
random.2015 <- fit_decompose$random[13:24]
reconst.2015 <- seasonal.2015 * trend.2015 * random.2015
original.2015 <- fit_decompose$x[13:24]
errors.2015 <- original.2015 - reconst.2015
ymax = max(abs(c(min(errors.2015), max(errors.2015))))
#options(scipen=-3)
plot(errors.2015,
     ylim = c(-2*ymax, 2*ymax), type = "b", col = "black",
     main = "Errors (2015)", xlab = "Month", ylab = expression(y - hat(y)))
abline(a = 0, b = 0, col="darkgray", lty=2)

len <- length(fit_decompose$x)
deseason.all <- fit_decompose$x / fit_decompose$seasonal
plot(1:len, fit_decompose$x,
     main = "De-seasonalized time series", xlab = "Time steps", ylab = "Num. of sessions",
     lty = 1, lwd = 1.5, type="l", col="black")
lines(1:len, deseason.all,
      lty = 3, lwd=2.0, col="black")
legend("bottomright", lty = c(1,3), lwd = c(1.5, 2.0), col = c("black", "black"), bty = "n",
       legend = c("Original", "De-seasonalized"))

plot(1:12, original.2015 / trend.2015,
     main = "Random component (2015)", xlab = "Month", ylab = "Value",
     lty = 1, lwd=1.5, type="l", col="black")
lines(1:12, original.2015 / (trend.2015 * seasonal.2015),
      lty = 3, lwd=2.0, col="black")
legend(x = 1, y = 0.90,
       lty = c(1,3), lwd = c(1.5, 2.0), col = c("black", "black"), bty = "n",
       legend = c(expression("De-trended: " * frac(y[t],T[t]) == S[t] %*% R[t]),
                  expression("De-trended & de-seasonalized: " * frac(y[t],T[t] %*% S[t]) == R[t])))

#Install and load forecast package if not installed
if(!require(forecast)) {
  install.packages("forecast", dependencies = T, repos = "http://cran.us.r-project.org")
  library(forecast) }
#Fit simple exponential smoothing model to data and show summary
fit_ses <- ses(Organic_Traffic)
summary(fit_ses)

#Plot the forecasted values
plot(fit_ses)

#Fit Holt exponential smoothing model to data and show summary
fit_holt <- holt(Organic_Traffic)
summary(fit_holt)

#Plot the forecasted values
plot(fit_holt)

#Fit Holt-Winters exponential smoothing model to data and show summary
fit_hw <- hw(Organic_Traffic, h = 10, seasonal = "multiplicative")
summary(fit_hw)

#Plot the forecasted values
plot(fit_hw)

#Fit automated exponential smoothing model to data and show summary
fit_auto <- forecast(Organic_Traffic, h = 10)
summary(fit_auto)

#Plot the forecasted values
plot(forecast(fit_auto))

#Fit automated exponential smoothing model to training dataset and calculate forecast accuracy
train <- window(Organic_Traffic, end = c(2018, 3))
test <- window(Organic_Traffic, start = c(2018, 4))
fit_hw_train <- hw(train)
fit_auto_train <- forecast(train)
accuracy(forecast(fit_hw_train), test) ["Test set", "RMSE"]
accuracy(forecast(fit_auto_train), test) ["Test set", "RMSE"]

#Calcuate mean of residuals and plot histogram for residuals
fit_hw_res <- residuals(forecast(fit_hw_train))
fit_auto_res <- residuals(forecast(fit_auto_train))
mean(fit_hw_res)
mean(fit_auto_res)

hist(fit_hw_res)
mean.hw <- mean(fit_hw_res)
abline(v = mean.hw, lwd = 2.0, lty = 2, col = "red")

hist(fit_auto_res)
mean.auto <- mean(fit_auto_res)
abline(v = mean.auto, lwd = 2.0, lty = 2, col = "red")
