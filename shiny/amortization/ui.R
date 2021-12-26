library(magrittr)

requireNamespace("bslib")
requireNamespace("dplyr")
requireNamespace("DT")
requireNamespace("ggplot2")
requireNamespace("glue")
requireNamespace("lubridate")
requireNamespace("magrittr")
requireNamespace("purrr")
requireNamespace("scales")
requireNamespace("shiny")
requireNamespace("tibble")
requireNamespace("zoo")

# Theme (https://bootswatch.com/)
normal_mode <- bslib::bs_theme()
fun_mode <- bslib::bs_theme(bootswatch = "sketchy")

ui <- shiny::fluidPage(
  theme = normal_mode,
  shiny::titlePanel(title = "Amortization Table and Plot"),
  shiny::span("Calculation is based on ",
    shiny::a("Pecners' Mortgage Calculator",
      href = "https://github.com/Pecners/mortgage_calculator"
    ),
    shiny::br(),
    shiny::icon(name = "github", lib = "font-awesome"),
    shiny::a("Source code for this Shiny app @pymk",
      href = "https://github.com/pymk/R/tree/master/shiny/amortization"
    ),
    style = "color:gray; font-size:0.8em"
  ),
  shiny::hr(),
  shiny::sidebarLayout(
    sidebarPanel = shiny::sidebarPanel(
      width = 4,

      # Text input
      shiny::helpText("Principal loan amount in USD"),
      shiny::numericInput(
        inputId = "loan_amount", label = "Loan Amount", value = "1000000", step = 100000
      ),
      shiny::helpText("Loan repayment period in number of years"),
      shiny::numericInput(inputId = "loan_term", label = "Loan Term", value = "30", step = 5),
      shiny::helpText("Interest rate as a percentage of the principal loan amount"),
      shiny::numericInput(
        inputId = "interest_rate", label = "Interest Rate", value = "3", step = 0.01
      ),
      shiny::helpText("Date of first payment initiation"),
      shiny::dateInput(inputId = "start_date", label = "First Payment", format = "yyyy-mm-dd"),

      # Buttons ------------------------------------------------------------------------------------
      shiny::hr(),
      shiny::actionButton(
        inputId = "calculate_button",
        label = "Calculate",
        shiny::icon(name = "sync", lib = "font-awesome"), width = "auto"
      ),

      # Theme --------------------------------------------------------------------------------------
      shiny::hr(),
      shiny::h2("Options"),
      shiny::checkboxInput(inputId = "fun_mode", label = "Fun Mode"),
    ),
    mainPanel = shiny::mainPanel(
      shiny::div(
        shiny::plotOutput(outputId = "plot_remaining"),
        shiny::plotOutput(outputId = "plot_distribution"),
      ),
      shiny::br(),
      shiny::p(
        shiny::textOutput(outputId = "summary_text", inline = TRUE),
        shiny::strong(shiny::textOutput(outputId = "summary_amount", inline = TRUE))
      ),
      DT::dataTableOutput(outputId = "table_oi"),
    ),
  ),
)
