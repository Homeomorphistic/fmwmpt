mod_attainable_set_sliders_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("rho"), "rho", min = -1, max = 1, value = 0, step = 0.01, post = "%")
  )
}

mod_attainable_set_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("attainable_set"), width = "100%", height = "70vh")
  )
}

mod_attainable_set_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$attainable_set <- shiny::renderPlot(
        plot_attainable_set(0.05, 0.15, 0.1, 0.2, rho = input$rho)
      )
    }
  )
}
