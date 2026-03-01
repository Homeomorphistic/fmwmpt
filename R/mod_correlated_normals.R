mod_correlated_normals_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("rho"), "rho", min = -1, max = 1, value = 0, step = 0.01, post = "%"),
    shiny::plotOutput(ns("correlated_normals"))
  )
}

mod_correlated_normals_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$correlated_normals <- shiny::renderPlot(
        plot_correlated_normals(input$rho)
      )
    }
  )
}
