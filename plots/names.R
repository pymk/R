requireNamespace("assertr")
requireNamespace("dplyr")
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
name_oi <- "Tyrion"
sex_oi <- "male"
year_limit_oi <- 1950

ggplot2::ggplot(
  data = subset(compiled_dataset, year > year_limit_oi & first_name == name_oi & sex == sex_oi),
  mapping = ggplot2::aes(x = year, y = count)
) +
  ggplot2::geom_line(linetype = "dashed", mapping = ggplot2::aes(group = 1)) +
  ggplot2::geom_point(colour = "steelblue") +
  ggplot2::scale_y_continuous(
    labels = scales::comma,
    breaks = scales::pretty_breaks(5)
  ) +
  ggplot2::labs(
    title = paste0("Popularity of the name '", name_oi, "' (", sex_oi, ")"),
    x = "Year",
    y = "Births (n)",
    colour = "Sex"
  ) +
  ggplot2::scale_color_brewer(palette = "Set2") +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::theme(
    legend.position = "Right",
    axis.text.x = ggplot2::element_text(angle = -90, vjust = 0.5, hjust = 1)
  )
