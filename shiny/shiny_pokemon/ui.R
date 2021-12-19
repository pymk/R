requireNamespace("DT")
requireNamespace("dplyr")
requireNamespace("glue")
requireNamespace("magrittr")
requireNamespace("purrr")
requireNamespace("readr")
requireNamespace("shiny")
requireNamespace("tibble")
requireNamespace("tidyr")

library(magrittr)

list_display <- list("All" = "all", "Legendary" = "legendary", "Mythical" = "mythical")

list_gens <- list(
  "Generation I" = "Generation I",
  "Generation II" = "Generation II",
  "Generation III" = "Generation III",
  "Generation IV" = "Generation IV",
  "Generation V" = "Generation V",
  "Generation VI" = "Generation VI",
  "Generation VII" = "Generation VII",
  "Generation VIII" = "Generation VIII"
)

list_colors <- list(
  "black" = "black",
  "blue" = "blue",
  "brown" = "brown",
  "gray" = "gray",
  "green" = "green",
  "pink" = "pink",
  "purple" = "purple",
  "red" = "red",
  "white" = "white",
  "yellow" = "yellow"
)

list_shapes <- list(
  "armor" = "armor",
  "arms" = "arms",
  "ball" = "ball",
  "blob" = "blob",
  "bug-wings" = "bug-wings",
  "fish" = "fish",
  "heads" = "heads",
  "humanoid" = "humanoid",
  "legs" = "legs",
  "quadruped" = "quadruped",
  "squiggle" = "squiggle",
  "tentacles" = "tentacles",
  "upright" = "upright",
  "wings" = "wings",
  "unknown" = "unknown"
)

list_types <- list(
  "bug" = "bug",
  "dark" = "dark",
  "dragon" = "dragon",
  "electric" = "electric",
  "fairy" = "fairy",
  "fighting" = "fighting",
  "fire" = "fire",
  "flying" = "flying",
  "ghost" = "ghost",
  "grass" = "grass",
  "ground" = "ground",
  "ice" = "ice",
  "normal" = "normal",
  "poison" = "poison",
  "psychic" = "psychic",
  "rock" = "rock",
  "steel" = "steel",
  "water" = "water"
)

ui <- shiny::fluidPage(
  # Title
  shiny::titlePanel("Pokémon Database "),
  shiny::p(shiny::img(src = "pokeball.png"), "Just a little R Shiny practice with Pokémon. Data and images from ", shiny::a("veekun", href = "https://github.com/veekun/pokedex"), "and ", shiny::a("msikma.", href = "https://github.com/msikma/pokesprite")),
  shiny::p(shiny::img(src = "masterball.png"), "Source code is available at ", shiny::a("github.com/pymk", href = "https://github.com/pymk/R/tree/master/shiny/shiny_pokemon")),
  shiny::sidebarLayout(
    # Side panel
    sidebarPanel = shiny::sidebarPanel(
      shiny::div(
        shiny::selectInput(inputId = "dropdown_pokemon", label = shiny::h2("Display"), choices = list_display, selected = "All Pokémon"),
        shiny::fluidRow(
          shiny::column(6, shiny::checkboxGroupInput(inputId = "checkbox_gens", label = shiny::h2("Generation"), choices = list_gens, selected = list_gens)),
          shiny::column(6, shiny::checkboxGroupInput(inputId = "checkbox_colors", label = shiny::h2("Color"), choices = list_colors, selected = list_colors))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::checkboxGroupInput(inputId = "checkbox_type_1", label = shiny::h2("Type (Primary)"), choices = list_types, selected = list_types)),
          shiny::column(6, shiny::checkboxGroupInput(inputId = "checkbox_shapes", label = shiny::h2("Shape"), choices = list_shapes, selected = list_shapes)),
        ),
      ),
    ),
    # Main panel
    mainPanel = shiny::mainPanel(
      shiny::h1(""),
      shiny::span(
        shiny::textOutput(outputId = "text_output", inline = TRUE),
        shiny::textOutput(outputId = "button_action", inline = TRUE),
        DT::dataTableOutput(outputId = "checkbox_output")
      )
    )
  )
)
