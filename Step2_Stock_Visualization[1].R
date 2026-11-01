# STEP 2: Interactive Stock Visualization
packages <- c("shiny", "ggplot2", "quantmod")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(shiny)
library(ggplot2)
library(quantmod)
stock_data <- getSymbols("AAPL", src = "yahoo", from = "2023-01-01",
                         to = "2023-07-01", auto.assign = FALSE)

ui <- fluidPage(
  titlePanel("Dynamic Stock Portfolio Dashboard"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Select Date Range:",
                     start = "2023-01-01", end = "2023-07-01"),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Select Chart Type:",
                  choices = c("Line Chart", "Candlestick Chart", "Area Chart"))
    ),
    mainPanel(h3("Apple (AAPL) Stock Price Analysis"),
              plotOutput("stock_chart", height = "450px"),
              h4("Historical Stock Data"), tableOutput("stock_table"))
  )
)
server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$date_range)
    data <- stock_data[paste0(input$date_range[1], "/", input$date_range[2])]
    validate(need(NROW(data) > 0, "No stock data for selected dates."))
    if (input$time_frame == "Weekly") data <- to.weekly(data, indexAt = "endof")
    if (input$time_frame == "Monthly") data <- to.monthly(data, indexAt = "endof")
    data.frame(Date = as.Date(index(data)), Open = as.numeric(Op(data)),
               High = as.numeric(Hi(data)), Low = as.numeric(Lo(data)),
               Close = as.numeric(Cl(data)))
  })
  output$stock_chart <- renderPlot({
    df <- filtered_data()
    if (input$chart_type == "Line Chart") {
      p <- ggplot(df, aes(Date, Close)) + geom_line(color = "blue", linewidth = 1)
    } else if (input$chart_type == "Area Chart") {
      p <- ggplot(df, aes(Date, Close)) +
        geom_area(fill = "skyblue", alpha = 0.6) + geom_line(color = "blue")
    } else {
      df$Direction <- ifelse(df$Close >= df$Open, "Up", "Down")
      candle_width <- switch(input$time_frame, Daily = 0.7, Weekly = 4, Monthly = 18)
      p <- ggplot(df, aes(Date)) + geom_linerange(aes(ymin = Low, ymax = High)) +
        geom_rect(aes(xmin = Date - candle_width / 2,
                      xmax = Date + candle_width / 2,
                      ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                      fill = Direction), color = "black") +
        scale_fill_manual(values = c(Up = "green", Down = "red"))
    }
    print(p + labs(title = "AAPL Stock Price", x = "Date",
                   y = "Stock Price (USD)") + theme_minimal())
  })
  output$stock_table <- renderTable(head(filtered_data(), 10), digits = 2)
}
shinyApp(ui = ui, server = server)
