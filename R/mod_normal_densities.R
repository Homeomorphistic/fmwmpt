mod_normal_densities_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      style = "display: flex; gap: 10px; align-items: flex-end;",
      shiny::sliderInput(ns("mu"), greek_html("mu"), min = -1, max = 1, value = 0, step = 0.1),
      shiny::sliderInput(ns("sigma"), greek_html("sigma"), min = 0, max = 2, value = 1, step = 0.1)
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
