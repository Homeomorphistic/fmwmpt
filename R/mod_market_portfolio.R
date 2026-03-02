mod_market_portfolio_sliders_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("rho"), "rho", min = -1, max = 1, value = 0, step = 0.01, post = "%"),
    shiny::sliderInput(ns("risk_free"), "stopa", min = 0, max = 0.2, value = 0.04, step = 0.01, post = "%"),
    shiny::sliderInput(ns("mu_1"), "mu_1", min = 0, max = 0.3, value = 0.07, step = 0.01, post = "%"),
    shiny::sliderInput(ns("mu_2"), "mu_2", min = 0, max = 0.3, value = 0.15, step = 0.01, post = "%"),
    shiny::sliderInput(ns("sigma_1"), "sigma_1", min = 0, max = 0.5, value = 0.10, step = 0.01, post = "%"),
    shiny::sliderInput(ns("sigma_2"), "sigma_2", min = 0, max = 0.5, value = 0.20, step = 0.01, post = "%")
  )
}

mod_market_portfolio_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("market_portfolio"), width = "100%", height = "70vh")
  )
}

mod_market_portfolio_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$market_portfolio <- shiny::renderPlot(
        plot_market_portfolio(input$mu_1, input$mu_2, input$sigma_1, input$sigma_2, input$rho, input$risk_free)
      )
    }
  )
}
