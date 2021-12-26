server <- function(input, output) {

  # Reactive objects -------------------------------------------------------------------------------
  # v <- shiny::reactiveValues(data = NULL)
  # button_action <- shiny::observeEvent(input$icon_clean, {
  #   v$data <- "good person!"
  # })
  # button_action <- shiny::observeEvent(input$icon_dirty, {
  #   v$data <- "slut!"
  # })

  pokemon_subset <- shiny::reactive({
    if (input$dropdown_pokemon == "all") {
      return(pokemon_subset <- pokemon_data)
    }
    if (input$dropdown_pokemon == "legendary") {
      return(pokemon_data %>% dplyr::filter(legendary))
    }
    if (input$dropdown_pokemon == "mythical") {
      return(pokemon_data %>% dplyr::filter(mythical))
    }
  })

  data_oi <- shiny::reactive({
    pokemon_subset() %>%
      dplyr::filter(
        generation %in% input$checkbox_gens,
        type_1 %in% input$checkbox_type_1,
        color %in% input$checkbox_colors,
        shape %in% input$checkbox_shapes
      )
  })

  # output$text_output <- shiny::renderText({
  #   input$text_input
  # })
  # output$button_action <- shiny::renderText({
  #   v$data
  # })
  
  output$checkbox_output <- DT::renderDataTable(
    data_oi(),
    escape = FALSE
  )
}
