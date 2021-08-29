server <- function(input, output) {
  # Load data --------------------------------------------------------------------------------------
  # Get a list of files and store the paths in a list.
  # "National data" from: https://www.ssa.gov/oact/babynames/limits.html
  # name_ls <- list.files("~/Code/R/datasets/names", full.names = TRUE, pattern = "*.txt")

  dataset_url <- "https://pymkdb-public.s3.us-west-1.amazonaws.com/datasets/names.zip"
  dataset_zip <- "names.zip"
  dataset_unzip <- "names_data/"

  # Progress bar to notify user something is happening
  shiny::withProgress(
    message = "Downloading data...",
    value = 0, {
      shiny::incProgress(1 / 10)
      shiny::incProgress(2 / 10)
      download.file(dataset_url, destfile = dataset_zip, mode = "wb")
    }
  )

  # Progress bar to notify user something is happening
  shiny::withProgress(
    message = "Extracting data.",
    detail = glue::glue("Unzipping data to {dataset_unzip}"),
    value = 0, {
      shiny::incProgress(1 / 10)
      shiny::incProgress(2 / 10)
      unzip(zipfile = dataset_zip, exdir = dataset_unzip, overwrite = TRUE)
    }
  )

  if (file.exists(dataset_zip)) {
    file.remove(dataset_zip)
  }

  # List of files
  name_ls <- list.files(dataset_unzip, full.names = TRUE, pattern = "*.txt")

  # Get the year from the filenames and set it as row names to be extracted later
  names(name_ls) <- stringr::str_remove_all(name_ls, pattern = ".*/|yob|.txt")

  # Wrangle data -----------------------------------------------------------------------------------
  # Combine all the individual files into one dataset.
  compiled_dataset <- purrr::map_df(
    .x = name_ls,
    .id = "year",
    .f = rio::import,
    format = ",",
    col.names = c("first_name", "sex", "count")
  ) %>%
    dplyr::mutate(year = stringr::str_remove(year, "^.*//")) %>%
    assertr::verify(sex %in% c("F", "M")) %>%
    dplyr::mutate(sex = dplyr::if_else(sex == "F", "female", "male")) %>%
    dplyr::group_by(year, sex) %>%
    dplyr::mutate(rank = dplyr::dense_rank(dplyr::desc(count))) %>%
    dplyr::ungroup()
  # Plot data --------------------------------------------------------------------------------------
  dataset <- shiny::reactive({
    compiled_dataset %>%
      dplyr::filter(
        first_name == input$name_oi,
        year >= min(input$year_oi),
        year <= max(input$year_oi),
        sex %in% input$sex_oi
      )
  })

  output$plot <- shiny::renderPlot({
    ggplot2::ggplot(
      data = dataset(),
      mapping = ggplot2::aes(x = year, y = count, group = sex)
    ) +
      ggplot2::geom_line(
        linetype = "dashed",
        alpha = 0.5,
        mapping = ggplot2::aes(color = sex)
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(color = sex)
      ) +
      ggplot2::scale_x_discrete(
        breaks = scales::pretty_breaks(10)
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        breaks = scales::pretty_breaks(5)
      ) +
      ggplot2::labs(
        title = glue::glue("Popularity of the name '{input$name_oi}'"),
        x = "Year",
        y = "Births (n)",
        colour = "",
      ) +
      ggplot2::scale_color_manual(values = c("female" = "#FD6467", "male" = "#7294D4")) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        legend.position = "top",
        axis.text.x = ggplot2::element_text(angle = -90, vjust = 0.5, hjust = 1)
      )
  })
}
