# STEP 4 - Dynamic Stock Portfolio Dashboard
# Moving average crossover signals and technical indicators
# Save this entire file and click Source or Run App in RStudio.

required <- c("shiny", "ggplot2", "quantmod")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(shiny)
library(ggplot2)
library(quantmod)

# Extra history provides enough observations for 50-month moving averages.
stock_symbol <- "AAPL"
stock_data <- tryCatch(
  getSymbols(stock_symbol, src = "yahoo", from = "2015-01-01",
             to = Sys.Date() + 1, auto.assign = FALSE),
  error = function(e) stop("Yahoo Finance download failed: ", conditionMessage(e))
)

ui <- fluidPage(
  titlePanel("Step 4: Stock Dashboard - Trading Signals"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Select Date Range:",
                     start = "2023-01-01", end = "2023-07-01",
                     min = "2015-01-01", max = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Select Chart Type:",
                  choices = c("Line Chart", "Candlestick Chart", "Area Chart")),
      checkboxGroupInput("technical_indicators", "Technical Indicators:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = c("Moving Averages", "RSI", "MACD")),
      checkboxInput("show_signals", "Show Buy/Sell Annotations", TRUE),
      helpText("Buy = SMA20 crosses above SMA50; Sell = crosses below; otherwise Hold.")
    ),
    mainPanel(
      plotOutput("stock_chart", height = "420px"),
      conditionalPanel("input.technical_indicators.includes('RSI')",
                       plotOutput("rsi_chart", height = "230px")),
      conditionalPanel("input.technical_indicators.includes('MACD')",
                       plotOutput("macd_chart", height = "250px")),
      h4("Recent Trading Signals"), tableOutput("signal_table"),
      h4("Historical Stock Data"), tableOutput("stock_table")
    )
  )
)

server <- function(input, output, session) {
  indicator_data <- reactive({
    req(input$date_range, input$time_frame)
    # Always calculate technical indicators BEFORE applying the visible date range.
    prices <- stock_data
    if (input$time_frame == "Weekly") {
      prices <- to.weekly(prices, indexAt = "endof")
    } else if (input$time_frame == "Monthly") {
      prices <- to.monthly(prices, indexAt = "endof")
    }
    validate(need(NROW(prices) >= 50,
                  "At least 50 historical periods are needed for SMA50."))

    close_prices <- Cl(prices)
    sma20 <- SMA(close_prices, n = 20)
    sma50 <- SMA(close_prices, n = 50)
    rsi14 <- RSI(close_prices, n = 14)
    macd_values <- MACD(close_prices, nFast = 12, nSlow = 26,
                        nSig = 9, maType = "EMA", percent = FALSE)

    # A crossover is a change of relative MA position, not simply MA20 > MA50.
    difference <- as.numeric(sma20 - sma50)
    previous <- c(NA_real_, head(difference, -1))
    signals <- rep("Hold", length(difference))
    signals[!is.na(previous) & !is.na(difference) &
              previous <= 0 & difference > 0] <- "Buy"
    signals[!is.na(previous) & !is.na(difference) &
              previous >= 0 & difference < 0] <- "Sell"

    df <- data.frame(
      Date = as.Date(index(prices)),
      Open = as.numeric(Op(prices)),
      High = as.numeric(Hi(prices)),
      Low = as.numeric(Lo(prices)),
      Close = as.numeric(close_prices),
      SMA20 = as.numeric(sma20),
      SMA50 = as.numeric(sma50),
      RSI = as.numeric(rsi14),
      MACD = as.numeric(macd_values[, 1]),
      SignalLine = as.numeric(macd_values[, 2]),
      Signal = signals
    )
    df <- df[df$Date >= as.Date(input$date_range[1]) &
               df$Date <= as.Date(input$date_range[2]), , drop = FALSE]
    validate(need(nrow(df) > 0, "No stock observations in selected date range."))
    df
  })

  output$stock_chart <- renderPlot({
    df <- indicator_data()
    if (input$chart_type == "Line Chart") {
      p <- ggplot(df, aes(Date, Close)) +
        geom_line(color = "steelblue", linewidth = 0.9)
    } else if (input$chart_type == "Area Chart") {
      p <- ggplot(df, aes(Date, Close)) +
        geom_area(fill = "skyblue", alpha = 0.45) +
        geom_line(color = "steelblue", linewidth = 0.8)
    } else {
      df$Direction <- ifelse(df$Close >= df$Open, "Up", "Down")
      width <- switch(input$time_frame, Daily = 0.7, Weekly = 4, Monthly = 18)
      p <- ggplot(df, aes(x = Date)) +
        geom_linerange(aes(ymin = Low, ymax = High)) +
        geom_rect(aes(xmin = Date - width / 2,
                      xmax = Date + width / 2,
                      ymin = pmin(Open, Close),
                      ymax = pmax(Open, Close), fill = Direction),
                  color = "black", linewidth = 0.2) +
        scale_fill_manual(values = c(Up = "forestgreen", Down = "firebrick"))
    }
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(data = df, aes(x = Date, y = SMA20),
                  inherit.aes = FALSE, color = "orange", linewidth = 0.8,
                  na.rm = TRUE) +
        geom_line(data = df, aes(x = Date, y = SMA50),
                  inherit.aes = FALSE, color = "purple", linewidth = 0.8,
                  na.rm = TRUE)
    }
    if (isTRUE(input$show_signals)) {
      events <- df[df$Signal %in% c("Buy", "Sell"), , drop = FALSE]
      if (nrow(events) > 0) {
        p <- p +
          geom_point(data = events, aes(x = Date, y = Close, color = Signal),
                     inherit.aes = FALSE, size = 2.5) +
          geom_text(data = events,
                    aes(x = Date, y = Close, label = Signal, color = Signal),
                    inherit.aes = FALSE, vjust = -0.9, size = 3.5,
                    check_overlap = TRUE) +
          scale_color_manual(values = c(Buy = "forestgreen", Sell = "firebrick"))
      }
    }
    print(p + labs(title = "AAPL Stock Price and Trading Signals",
                   x = "Date", y = "Price (USD)") + theme_minimal())
  })

  output$rsi_chart <- renderPlot({
    req("RSI" %in% input$technical_indicators)
    df <- indicator_data()
    print(ggplot(df, aes(Date, RSI)) +
            geom_line(color = "purple") +
            geom_hline(yintercept = c(30, 70), linetype = "dashed",
                       color = c("forestgreen", "firebrick")) +
            coord_cartesian(ylim = c(0, 100)) +
            labs(title = "RSI (14)", x = "Date", y = "RSI") + theme_minimal())
  })

  output$macd_chart <- renderPlot({
    req("MACD" %in% input$technical_indicators)
    df <- indicator_data()
    df$Histogram <- df$MACD - df$SignalLine
    print(ggplot(df, aes(Date)) +
            geom_col(aes(y = Histogram), fill = "gray60") +
            geom_line(aes(y = MACD), color = "blue") +
            geom_line(aes(y = SignalLine), color = "red") +
            geom_hline(yintercept = 0, linetype = "dashed") +
            labs(title = "MACD (12, 26, 9)", x = "Date", y = "MACD") +
            theme_minimal())
  })

  output$signal_table <- renderTable({
    df <- indicator_data()
    events <- df[df$Signal != "Hold", c("Date", "Close", "SMA20", "SMA50", "Signal"), drop = FALSE]
    if (nrow(events) == 0) {
      return(data.frame(Message = "No Buy/Sell crossover in selected range"))
    }
    tail(events, 10)
  }, digits = 2)

  output$stock_table <- renderTable({
    head(indicator_data(), 10)
  }, digits = 2)
}

shinyApp(ui = ui, server = server)
