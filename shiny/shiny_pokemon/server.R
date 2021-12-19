server <- function(input, output) {
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

  output$checkbox_output <- DT::renderDataTable(
    data_oi(),
    escape = FALSE
  )
}
