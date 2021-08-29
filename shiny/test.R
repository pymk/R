library(magrittr)

ui <- shiny::fluidPage(
  shiny::titlePanel(title = "Distribution"),
  shiny::sidebarLayout(
    sidebarPanel = shiny::sidebarPanel(
      shiny::sliderInput(inputId = "sample_n", label = "Number of samples", min = 0, max = 1000, value = 0, step = 200),
      shiny::selectInput(inputId = "type_oi", label = "Type", choices = c("mean", "sd", "median"), selected = "mean")
    ),
    mainPanel = shiny::mainPanel(
      shiny::plotOutput(outputId = "plot")
    ),
  ),
)

server <- function(input, output) {
  select_oi <- shiny::reactive({
    switch(input$type_oi,
      "mean" = "mean",
      "sd" = "sd",
      "median" = "median"
    )
  })

  dataset <- shiny::reactive({
    data_mean <- replicate(input$sample_n, mean(rnorm(n = input$sample_n, mean = input$sample_n / 2)))
    data_sd <- replicate(input$sample_n, sd(rnorm(n = input$sample_n, mean = input$sample_n / 2)))
    data_median <- replicate(input$sample_n, median(rnorm(n = input$sample_n, mean = input$sample_n / 2)))

    tibble::tibble(
      mean = data_mean,
      sd = data_sd,
      median = data_median
    )
  })

  output$plot <- shiny::renderPlot({
    ggplot2::ggplot(dataset(), ggplot2::aes(x = select_oi())) +
      ggplot2::geom_histogram()
  })
}

shiny::shinyApp(ui = ui, server = server)
