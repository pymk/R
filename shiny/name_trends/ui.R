requireNamespace("assertr")
requireNamespace("dplyr")
requireNamespace("ggplot2")
requireNamespace("glue")
requireNamespace("janitor")
requireNamespace("magrittr")
requireNamespace("purrr")
requireNamespace("rio")
requireNamespace("shiny")
requireNamespace("stringr")
requireNamespace("tidyr")

library(magrittr)

ui <- shiny::fluidPage(
  # Headers ----------------------------------------------------------------------------------------
  shiny::titlePanel("Name Trends"),
  shiny::p(
    "Using the national dataset from ",
    shiny::code("https://www.ssa.gov/oact/babynames/limits.html")
  ),
  shiny::sidebarLayout(
    # Sidebar --------------------------------------------------------------------------------------
    # Dropdown menu for Sex and slidebar for years
    sidebarPanel = shiny::sidebarPanel(
      # text input for name
      shiny::textInput(
        inputId = "name_oi",
        label = "Name of Interest",
        value = "Blake"
      ),
      # dropdown for sex
      shiny::selectInput(
        inputId = "sex_oi",
        label = "Sex of Interest",
        choices = c("female", "male"),
        selected = c("male", "female"),
        multiple = TRUE
      ),
      # slidebar for years
      shiny::sliderInput(
        inputId = "year_oi",
        label = "Year of Interest",
        min = 1880,
        max = 2020,
        value = c(1997, 2020),
        step = 1,
        sep = ""
      )
    ),
    # Main page ------------------------------------------------------------------------------------
    mainPanel = shiny::mainPanel(
      shiny::plotOutput(outputId = "plot")
    )
  )
)
