# stock_analysis.R

# Load required libraries
if (!require("quantmod")) install.packages("quantmod", dependencies = TRUE)
if (!require("TTR")) install.packages("TTR", dependencies = TRUE)

library(quantmod)
library(TTR)

# --- Step 1: Fetch Stock Data ---

# Define the stock symbol and time range
stock_symbol <- "AAPL"
start_date <- Sys.Date() - 365
end_date <- Sys.Date()

# Fetch stock data from Yahoo Finance
getSymbols(stock_symbol, src = "yahoo", from = start_date, to = end_date)

# Access the data (AAPL is loaded as a variable name)
stock_data <- get(stock_symbol)

# --- Step 2: Calculate Moving Averages ---

# Calculate 20-day and 50-day Simple Moving Averages
stock_data$SMA20 <- SMA(Cl(stock_data), n = 20)
stock_data$SMA50 <- SMA(Cl(stock_data), n = 50)

# --- Step 3: Visualize Stock Prices with Moving Averages ---

# Create the chart
chartSeries(stock_data, 
            name = paste(stock_symbol, "Stock Price with 20 & 50-day SMA"),
            theme = chartTheme("white"),
            TA = c(addSMA(n = 20, col = "blue"), 
                   addSMA(n = 50, col = "red")))

