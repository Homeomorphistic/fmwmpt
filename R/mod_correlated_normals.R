mod_correlated_normals_slider_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("rho"), greek_html("rho"), min = -1, max = 1, value = 0, step = 0.01, post = "%")
  )
}

mod_correlated_normals_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("correlated_normals"), width = "100%", height = "70vh")
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
