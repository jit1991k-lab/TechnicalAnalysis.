# STEP 1: Data Collection and Setup
packages <- c("shiny", "ggplot2", "quantmod")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(shiny)
library(ggplot2)
library(quantmod)

stock_symbol <- "AAPL"
start_date <- "2023-01-01"
end_date <- "2023-07-01"
stock_data <- getSymbols(stock_symbol, src = "yahoo", from = start_date,
                         to = end_date, auto.assign = FALSE)
print(head(stock_data))
print(tail(stock_data))
print(summary(stock_data))
closing_prices <- Ad(stock_data)
stock_df <- data.frame(Date = as.Date(index(closing_prices)),
                       Price = as.numeric(closing_prices))
print(ggplot(stock_df, aes(Date, Price)) +
        geom_line(color = "blue", linewidth = 1) +
        labs(title = "Apple (AAPL) Stock Price", x = "Date",
             y = "Adjusted Closing Price (USD)") + theme_minimal())
