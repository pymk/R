# Super Bowl Commercials
# TidyTuesday dataset from 2021-03-02
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-03-02/readme.md

library(magrittr)

# Import data --------------------------------------------------------------------------------------
youtube_data <- tidytuesdayR::tt_load("2021-03-02")$youtube

# Data wrangling -----------------------------------------------------------------------------------
clean_data <- youtube_data %>%
  dplyr::rename(
    funny_flag = funny,
    show_product_quickly_flag = show_product_quickly,
    patriotic_flag = patriotic,
    celebrity_flag = celebrity,
    danger_flag = danger,
    animals_flag = animals,
    use_sex_flag = use_sex
  ) %>%
  dplyr::select(year, brand, dplyr::contains("_flag")) %>%
  dplyr::distinct() %>%
  # Count the number of flags for a given year for each brand
  dplyr::group_by(year, brand) %>%
  dplyr::summarise(
    dplyr::across(tidyselect:::where(is.logical), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    year = as.factor(year),
    brand = as.factor(brand),
    # reduce() combines the elements of a vector into a single value
    total_n = purrr::reduce(
      .x = dplyr::select(., dplyr::ends_with("_flag")),
      .f = `+`
    )
  )

clean_data_long <- clean_data %>%
  tidyr::pivot_longer(
    cols = -c(year, brand, total_n),
    names_to = "category",
    values_to = "count"
  )

# Plot ---------------------------------------------------------------------------------------------
# Color palette for plots
color_palette_interpolation_1 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
color_palette_interpolation_2 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Accent"))

colors_brand <- color_palette_interpolation_1(dplyr::n_distinct(clean_data$brand))
colors_category <- color_palette_interpolation_2(dplyr::n_distinct(clean_data_long$category))

# Bar plot (brands)
plot_brands <- clean_data %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(ggplot2::aes(x = year, y = total_n, fill = brand)) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(4)) +
  ggplot2::scale_fill_manual(values = colors_brand) +
  ggplot2::theme_linedraw(base_size = 14) +
  ggplot2::theme(
    legend.position = "right",
    axis.text.x = ggplot2::element_text(angle = -90, vjust = 0.5, hjust = 1)
  ) +
  ggplot2::labs(
    title = "Super Bowl Commercials by Brands",
    x = "Year",
    y = "Commercials (n)",
    fill = "Brands"
  )

plot_brands_facet <- plot_brands +
  ggplot2::guides(fill = "none") +
  ggplot2::facet_wrap(~brand)

# Bar plot (category)
plot_category <- clean_data_long %>%
  dplyr::filter(count != 0) %>%
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(
    x = year,
    y = count,
    size = count,
    color = category
  )) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(4)) +
  ggplot2::scale_color_manual(values = colors_category) +
  ggplot2::theme_linedraw(base_size = 14) +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(angle = -90, vjust = 0.5, hjust = 1)
  ) +
  ggplot2::labs(
    title = "Super Bowl Commercials by Category",
    x = "Year",
    y = "Commercials (n)",
    color = "Category"
  ) +
  ggplot2::guides(
    shape = "none", size = "none", alpha = "none",
    # Increases the legend icon size
    color = ggplot2::guide_legend(override.aes = list(size = 5))
  )

plot_category_facet <- plot_category +
  ggplot2::facet_wrap(~brand)

# Print --------------------------------------------------------------------------------------------
gridExtra::grid.arrange(plot_brands, plot_category, ncol = 2)
print(plot_brands_facet)
print(plot_category_facet)
