# Time Series Forecasting with ARIMA and ETS - AirPassengers Dataset

# Load required libraries
library(ggplot2)
library(forecast)
library(tseries)
library(Metrics)
library(zoo)  # For proper date handling

# Step 1: Load and visualize data
data("AirPassengers")
ap <- AirPassengers

# Convert to data frame for plotting
df <- data.frame(
  Date = as.Date(as.yearmon(time(ap))),
  Passengers = as.numeric(ap)
)

# Plot the time series
ggplot(df, aes(x = Date, y = Passengers)) +
  geom_line(color = "blue") +
  labs(title = "Monthly Air Passengers (1949-1960)",
       x = "Year", y = "Number of Passengers") +
  theme_minimal()

# Step 2: Confirm time series object
print(class(ap))         # "ts"
print(start(ap))         # 1949
print(end(ap))           # 1960
print(frequency(ap))     # 12 (monthly)

# Step 3: Check stationarity and transform if needed
cat("\nADF Test on Original Data:\n")
print(adf.test(ap))  # Likely non-stationary

# Log transformation and differencing
log_ap <- log(ap)
diff_log_ap <- diff(log_ap)

cat("\nADF Test on Differenced Log Data:\n")
print(adf.test(diff_log_ap))  # Should be stationary

# Plot transformed series
autoplot(diff_log_ap) +
  ggtitle("Differenced Log AirPassengers") +
  xlab("Year") + ylab("Log Differenced Passengers")

# Step 4: Fit ARIMA and ETS models
fit_arima <- auto.arima(log_ap)
fit_ets <- ets(log_ap)

cat("\nARIMA Model Summary:\n")
print(summary(fit_arima))

cat("\nETS Model Summary:\n")
print(summary(fit_ets))

# Step 5: Forecast next 24 months
forecast_arima <- forecast(fit_arima, h = 24)
forecast_ets <- forecast(fit_ets, h = 24)

# Plot forecasts in log scale
autoplot(forecast_arima) +
  ggtitle("ARIMA Forecast (Log Scale)") +
  xlab("Year") + ylab("Log(Passengers)")

autoplot(forecast_ets) +
  ggtitle("ETS Forecast (Log Scale)") +
  xlab("Year") + ylab("Log(Passengers)")

# Convert forecasts back to original scale (apply exp() only to components)
exp_forecast_arima <- forecast_arima
exp_forecast_arima$mean <- exp(forecast_arima$mean)
exp_forecast_arima$lower <- exp(forecast_arima$lower)
exp_forecast_arima$upper <- exp(forecast_arima$upper)

exp_forecast_ets <- forecast_ets
exp_forecast_ets$mean <- exp(forecast_ets$mean)
exp_forecast_ets$lower <- exp(forecast_ets$lower)
exp_forecast_ets$upper <- exp(forecast_ets$upper)

# Plot forecasts on original scale
autoplot(exp_forecast_arima) +
  ggtitle("ARIMA Forecast (Original Scale)") +
  xlab("Year") + ylab("Passengers")

autoplot(exp_forecast_ets) +
  ggtitle("ETS Forecast (Original Scale)") +
  xlab("Year") + ylab("Passengers")

# Step 6: Model evaluation
train <- window(log_ap, end = c(1958, 12))
test <- window(log_ap, start = c(1959, 1))

# Fit models on training data
fit_arima_train <- auto.arima(train)
fit_ets_train <- ets(train)

# Forecast for test period
fc_arima <- forecast(fit_arima_train, h = length(test))
fc_ets <- forecast(fit_ets_train, h = length(test))

# Evaluate using MAE and RMSE
mae_arima <- mae(test, fc_arima$mean)
rmse_arima <- rmse(test, fc_arima$mean)

mae_ets <- mae(test, fc_ets$mean)
rmse_ets <- rmse(test, fc_ets$mean)

# Print performance
cat("\nModel Performance on Test Data:\n")
cat("ARIMA - MAE:", round(mae_arima, 4), "RMSE:", round(rmse_arima, 4), "\n")
cat("ETS   - MAE:", round(mae_ets, 4), "RMSE:", round(rmse_ets, 4), "\n")
