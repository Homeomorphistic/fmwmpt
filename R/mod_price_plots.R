mod_price_plots_ui <- function(id) {
  date_range <- available_date_range()
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      style = "display: flex; gap: 10px; align-items: flex-end;",
      shinyWidgets::airDatepickerInput(ns("dates"), "Daty:",
                                       range = TRUE, autoClose = TRUE,
                                       language = "pl", addon = "none",
                                       minDate = date_range[1],
                                       maxDate = date_range[2]),
      shiny::selectInput(ns("ticker"), "Ticker:",
                         choices = c("", available_tickers()),
                         selected = "")
    ),
    shiny::plotOutput(ns("price_plot"))
  )
}

mod_price_plots_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      price_plot <- shiny::reactive({
        shiny::req(input$ticker, input$dates)
        plot_prices(ticker = input$ticker,
                    from = input$dates[1],
                    to = input$dates[2])
      })
      output$price_plot <- shiny::renderPlot(price_plot())
    }
  )
}
