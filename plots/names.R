requireNamespace("assertr")
requireNamespace("dplyr")
requireNamespace("glue")
requireNamespace("janitor")
requireNamespace("magrittr")
requireNamespace("purrr")
requireNamespace("rio")
requireNamespace("stringr")
requireNamespace("tidyr")

library(magrittr)

# Load data ----------------------------------------------------------------------------------------
# Get a list of files and store the paths in a list.
# Dataset from: https://www.ssa.gov/oact/babynames/limits.html
# Copied to: https://pymkdb-public.s3.us-west-1.amazonaws.com/datasets/names.zip
name_ls <- list.files("~/Code/R/datasets/names", full.names = TRUE, pattern = "*.txt")

# Extract the year from the filenames
names(name_ls) <- stringr::str_remove_all(name_ls, pattern = ".*/|yob|.txt")

# Wrangle data -------------------------------------------------------------------------------------
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

# Top n names for a given year
n_oi <- 10

top_n <- compiled_dataset %>%
  dplyr::group_by(year, sex) %>%
  dplyr::slice_min(rank, n = n_oi) %>%
  dplyr::ungroup()

top_n_pivot <- top_n %>%
  tidyr::pivot_wider(
    id_cols = c(year, rank),
    names_from = sex,
    values_from = first_name
  ) %>%
  dplyr::arrange(dplyr::desc(year), rank)

# Plot data ----------------------------------------------------------------------------------------
name_oi <- "Blake"
sex_oi <- c("male", "female")
year_limit_oi <- 1950

compiled_dataset %>%
  dplyr::filter(
    first_name == name_oi,
    sex %in% sex_oi,
    year >= year_limit_oi
  ) %>%
  ggplot2::ggplot(
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
    title = paste0("Popularity of the name '", name_oi, "' (", paste0(unique(sex_oi), collapse = ", "), ")"),
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
