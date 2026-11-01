# STEP 3: OVERLAY TECHNICAL INDICATORS
packages <- c("shiny", "ggplot2", "quantmod")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(shiny)
library(ggplot2)
library(quantmod)

# Download enough history for indicator warm-up
stock_symbol <- "AAPL"
stock_data <- getSymbols(stock_symbol, src = "yahoo",
                         from = "2022-01-01", to = "2023-07-01",
                         auto.assign = FALSE)

# User interface
ui <- fluidPage(
  titlePanel("Dynamic Stock Portfolio Dashboard"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Select Date Range:",
                     start = "2023-01-01", end = "2023-07-01",
                     min = "2022-01-01", max = "2023-07-01"),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Select Chart Type:",
                  choices = c("Line Chart", "Candlestick Chart", "Area Chart")),
      checkboxGroupInput("technical_indicators", "Select Technical Indicators:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = "Moving Averages")
    ),
    mainPanel(
      h3("Apple (AAPL) Technical Analysis"),
      plotOutput("stock_chart", height = "400px"),
      conditionalPanel("input.technical_indicators.includes('RSI')",
                       h4("Relative Strength Index (RSI)"),
                       plotOutput("rsi_chart", height = "230px")),
      conditionalPanel("input.technical_indicators.includes('MACD')",
                       h4("MACD Indicator"),
                       plotOutput("macd_chart", height = "250px")),
      h4("Historical Stock Data"),
      tableOutput("stock_table")
    )
  )
)

# Reactive server
server <- function(input, output, session) {
  indicator_data <- reactive({
    req(input$date_range)
    data <- stock_data
    if (input$time_frame == "Weekly") {
      data <- to.weekly(data, indexAt = "endof")
    } else if (input$time_frame == "Monthly") {
      data <- to.monthly(data, indexAt = "endof")
    }
    close_prices <- Cl(data)
    sma10 <- SMA(close_prices, n = 10)
    sma20 <- SMA(close_prices, n = 20)
    rsi14 <- RSI(close_prices, n = 14)
    macd_values <- MACD(close_prices, nFast = 12, nSlow = 26,
                        nSig = 9, maType = "EMA", percent = FALSE)
    df <- data.frame(
      Date = as.Date(index(data)),
      Open = as.numeric(Op(data)),
      High = as.numeric(Hi(data)),
      Low = as.numeric(Lo(data)),
      Close = as.numeric(Cl(data)),
      SMA10 = as.numeric(sma10),
      SMA20 = as.numeric(sma20),
      RSI = as.numeric(rsi14),
      MACD = as.numeric(macd_values[, 1]),
      Signal = as.numeric(macd_values[, 2])
    )
    df <- df[df$Date >= input$date_range[1] &
             df$Date <= input$date_range[2], ]
    validate(need(nrow(df) > 0, "No data for selected dates."))
    df
  })

  output$stock_chart <- renderPlot({
    df <- indicator_data()
    if (input$chart_type == "Line Chart") {
      p <- ggplot(df, aes(x = Date, y = Close)) +
        geom_line(color = "blue", linewidth = 1)
    } else if (input$chart_type == "Area Chart") {
      p <- ggplot(df, aes(x = Date, y = Close)) +
        geom_area(fill = "skyblue", alpha = 0.5) +
        geom_line(color = "blue")
    } else {
      df$Direction <- ifelse(df$Close >= df$Open, "Up", "Down")
      candle_width <- switch(input$time_frame,
                             "Daily" = 0.7, "Weekly" = 4, "Monthly" = 18)
      p <- ggplot(df, aes(x = Date)) +
        geom_linerange(aes(ymin = Low, ymax = High)) +
        geom_rect(aes(xmin = Date - candle_width / 2,
                      xmax = Date + candle_width / 2,
                      ymin = pmin(Open, Close),
                      ymax = pmax(Open, Close), fill = Direction),
                  color = "black") +
        scale_fill_manual(values = c("Up" = "green", "Down" = "red"))
    }
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(data = df, aes(x = Date, y = SMA10),
                  color = "orange", linewidth = 1,
                  inherit.aes = FALSE, na.rm = TRUE) +
        geom_line(data = df, aes(x = Date, y = SMA20),
                  color = "purple", linewidth = 1,
                  inherit.aes = FALSE, na.rm = TRUE)
    }
    print(p + labs(title = "AAPL Stock Price with Indicators",
                   x = "Date", y = "Stock Price (USD)") + theme_minimal())
  })

  output$rsi_chart <- renderPlot({
    req("RSI" %in% input$technical_indicators)
    df <- indicator_data()
    ggplot(df, aes(x = Date, y = RSI)) +
      geom_line(color = "purple", linewidth = 1) +
      geom_hline(yintercept = 70, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 30, linetype = "dashed", color = "green") +
      coord_cartesian(ylim = c(0, 100)) +
      labs(title = "RSI (14 Periods)", x = "Date", y = "RSI") +
      theme_minimal()
  })

  output$macd_chart <- renderPlot({
    req("MACD" %in% input$technical_indicators)
    df <- indicator_data()
    df$Histogram <- df$MACD - df$Signal
    ggplot(df, aes(x = Date)) +
      geom_col(aes(y = Histogram), fill = "gray", alpha = 0.6) +
      geom_line(aes(y = MACD), color = "blue", linewidth = 1) +
      geom_line(aes(y = Signal), color = "red", linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "MACD (12, 26, 9)", x = "Date",
           y = "MACD Value") + theme_minimal()
  })

  output$stock_table <- renderTable({
    df <- head(indicator_data(), 10)
    df$Date <- format(df$Date, "%Y-%m-%d")
    df
  }, digits = 2)
}

shinyApp(ui = ui, server = server)
