mod_normal_densities_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      style = "display: flex; gap: 10px; align-items: flex-end;",
      shiny::sliderInput(ns("mu"), "mu", min = -0.5, max = 0.5, value = 0, step = 0.01, post = "%"),
      shiny::sliderInput(ns("sigma"), "sigma", min = 0, max = 1, value = 2, step = 0.01, post = "%")
    ),
    shiny::plotOutput(ns("normal_densities"))
  )
}

mod_normal_densities_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$normal_densities <- shiny::renderPlot(
        plot_normal_densities(input$mu, input$sigma)
      )
    }
  )
}
