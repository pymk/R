server <- function(input, output, session) {

  # Theme (https://bootswatch.com/)
  normal_mode <- bslib::bs_theme()
  fun_mode <- bslib::bs_theme(bootswatch = "sketchy")

  # Theme ------------------------------------------------------------------------------------------
  shiny::observe(session$setCurrentTheme(
    if (isTRUE(input$fun_mode)) {
      fun_mode
    } else {
      normal_mode
    }
  ))

  # Data -------------------------------------------------------------------------------------------
  month_table <-
    shiny::observeEvent(input$calculate_button, {

      # Amortizations calculation from @Pecners
      # https://github.com/Pecners/mortgage_calculator
      start_date <- input$start_date
      term <- input$loan_term * 12
      original_loan_amount <- input$loan_amount
      annual_rate <- input$interest_rate / 100
      monthly_rate <- annual_rate / 12

      # Formula to calculate monthly principal and interest payment
      total_PI <- original_loan_amount *
        (monthly_rate * (1 + monthly_rate)^term) /
        (((1 + monthly_rate)^term) - 1)

      # Initialize the vectors as numeric with a length equal to the term of the loan
      interest <- principal <- balance <- date <- vector("numeric", term)
      loan_amount <- original_loan_amount

      # For loop to calculate values for each payment
      for (i in 1:term) {
        intr <- loan_amount * monthly_rate
        prnp <- total_PI - intr
        loan_amount <- loan_amount - prnp

        interest[i] <- intr
        principal[i] <- prnp
        balance[i] <- loan_amount
      }

      # Table --------------------------------------------------------------------------------------
      standard_schedule <- tibble::tibble(
        payment_id = 1:term,
        date = seq.Date(from = lubridate::as_date(start_date), by = "month", length.out = term),
        year_month = as.factor(zoo::as.yearmon(date)),
        interest,
        principal,
        balance
      )

      standard_schedule_fmt <- standard_schedule %>%
        dplyr::select(payment_id, date = year_month, principal, interest, balance) %>%
        purrr::modify_at(
          .at = c("interest", "principal", "balance"),
          .f = scales::dollar, largest_with_cents = 1e+6
        )

      summary_text <- "Monthly payment of "
      summary_amount <- scales::dollar(total_PI)

      # Plots --------------------------------------------------------------------------------------
      p <- standard_schedule %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = lubridate::as_date(date))) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::scale_x_date(date_breaks = "2 year", date_labels = "%b %Y") +
        ggplot2::scale_y_continuous(
          breaks = scales::pretty_breaks(n = 5),
          labels = scales::label_dollar(prefix = "$")
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90, vjust = 0.5, hjust = 1))


      plot_remaining <- p +
        ggplot2::geom_area(
          mapping = ggplot2::aes(y = balance),
          color = "darkolivegreen",
          fill = "darkolivegreen",
          alpha = 0.5
        ) +
        ggplot2::labs(
          title = "Remaining Mortgage Loan",
          subtitle = glue::glue("Monthly payments of {scales::dollar(total_PI)}"),
          x = "",
          y = ""
        )

      plot_distribution <- p +
        ggplot2::geom_area(
          mapping = ggplot2::aes(y = principal),
          color = "lightblue3",
          fill = "lightblue3",
          alpha = 0.7
        ) +
        ggplot2::geom_area(
          mapping = ggplot2::aes(y = interest),
          color = "salmon",
          fill = "salmon",
          alpha = 0.3
        ) +
        ggplot2::labs(
          title = "Monthly Payment Distribution Over Time",
          subtitle = "Interest (red) vs Principal (blue)",
          x = "",
          y = ""
        )

      # Output -------------------------------------------------------------------------------------
      output$plot_remaining <- shiny::renderPlot(plot_remaining)
      output$plot_distribution <- shiny::renderPlot(plot_distribution)

      output$summary_text <- shiny::renderText(summary_text)
      output$summary_amount <- shiny::renderText(summary_amount)
      output$table_oi <- DT::renderDataTable(
        standard_schedule_fmt,
        extensions = c("Buttons"),
        options = list(
          dom = "Brltip",
          pageLength = 12,
          lengthMenu = c(12, 24, 48),
          buttons = c("csv", "pdf", "print")
        )
      )
    })
}
