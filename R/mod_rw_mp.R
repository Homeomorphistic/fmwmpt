mod_rw_mp_inputs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("ticker1"), "ticker 1",
                       choices = c("", available_tickers()),
                       selected = ""),
    shiny::selectInput(ns("ticker2"), "ticker 2",
                       choices = c("", available_tickers()),
                       selected = ""),
    shiny::sliderInput(ns("risk_free"), "stopa", min = 0, max = 0.2, value = 0.04, step = 0.01, post = "%")
    #shiny::tableOutput(ns("summary_ann"))
  )
}

mod_rw_mp_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("rw_mp"), width = "100%", height = "70vh")
  )
}

mod_rw_mp_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      summary_ann <- shiny::reactive({
        shiny::req(input$ticker1, input$ticker2)
        stocks_summary(c(input$ticker1, input$ticker2), "monthly") |>
          dplyr::mutate(avg_return = (1 + avg_return)^12 - 1,
                        volatility = volatility * sqrt(12))
      })
      correlation <- shiny::reactive({
        shiny::req(input$ticker1, input$ticker2)
        stocks_correlation(input$ticker1, input$ticker2, "monthly")
      })

      output$summary_ann <- shiny::renderTable({
        summary_ann() |>
          dplyr::mutate(dplyr::across(c(avg_return, volatility), ~scales::percent(., accuracy = 0.01)))
        })

      output$rw_mp <- shiny::renderPlot({
        shiny::req(summary_ann(), correlation())
        mu <- summary_ann()$avg_return
        sigma <- summary_ann()$volatility
        plot_market_portfolio(mu[1], mu[2], sigma[1], sigma[2], correlation(), input$risk_free)
      })
    }
  )
}
