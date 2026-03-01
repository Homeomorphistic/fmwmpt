mod_stocks_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      style = "display: flex; gap: 10px; align-items: flex-end;",
      shiny::selectInput(
        ns("tickers"),
        "tickery",
        choices  = available_tickers(),
        selected = NULL,
        multiple = TRUE
      ),
      shiny::selectInput(ns("period"), "okres",
                         choices = c("dzienny" = "daily",
                                     "miesięczny" = "monthly",
                                     "roczny" = "yearly")
      )
    ),
    DT::dataTableOutput(ns("stocks_summary"))
  )
}

mod_stocks_summary_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      summary <- shiny::reactive({
        shiny::req(input$tickers)
        stocks_summary(input$tickers, input$period)
      })

      output$stocks_summary <- DT::renderDataTable({
        summary() |>
          dplyr::mutate(dplyr::across(c(avg_return, volatility), ~scales::percent(., accuracy = 0.01)))
      }, options = list(searching = FALSE, paging = FALSE, lengthChange = FALSE))
    }
  )
}
