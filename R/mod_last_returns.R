mod_last_returns_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      style = "display: flex; gap: 10px; align-items: flex-end;",
      shiny::selectInput(ns("ticker"), "ticker",
                         choices = c("", available_tickers()),
                         selected = ""),
      shiny::selectInput(ns("period"), "okres",
                         choices = c("dzienny" = "daily",
                                     "miesięczny" = "monthly",
                                     "roczny" = "yearly"),
                         selected = "monthly"),
      shiny::numericInput(ns("latest"), "ostatnie", min = 1, max = 5, value = 5)
    ),
    DT::dataTableOutput(ns("latest_returns")),
    shiny::textOutput(ns("mu"))
  )
}

mod_last_returns_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      latest_returns <- shiny::reactive({
        shiny::req(input$ticker, input$period, input$latest)
        stock_last_returns(input$ticker, input$period, input$latest)
      })

      mu <- shiny::reactive({
        shiny::req(latest_returns())

        latest_returns() |>
          dplyr::pull(return) |>
          mean(na.rm = TRUE)
      })

      output$latest_returns <- DT::renderDataTable({
        latest_returns() |>
          dplyr::mutate(return = scales::percent(return, accuracy = 0.01))
      }, options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE))
      output$mu <- shiny::renderText({
        m <- scales::percent(mu(), accuracy = 0.01)
        paste("Średnia zwrotów:", m)
      })
    }
  )
}
