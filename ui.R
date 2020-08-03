ui <- dashboardPage(
  dashboardHeader(title = "Stock Analyser"),
  dashboardSidebar(
    selectInput("ticker_select", "Ticker", sp500$Ticker)
  ),
  dashboardBody(
    
    fluidRow(
      column(
        width = 12,
        box(
          width = "100%",
          plotOutput('price_chart')
        )
      )
    ),
    fluidRow(
      column(
        width = 8,
        box(
          width = "100%",
          plotOutput('industry_chart')
        )
      ),
      column(
        width = 4,
        box(
          width = "100%",
          plotOutput('performance_chart')
        )
      )
    )
    
  )
)
