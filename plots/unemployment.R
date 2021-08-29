library(magrittr)

page_url <- rvest::read_html("http://www.bls.gov/web/laus/laumstrk.htm")
page_node <- rvest::html_node(page_url, "table")
page_table <- rvest::html_table(page_node)

fxn_transpose <- function(x) {
  repeated <- c("state", "rate", "rank")
  full <- floor(nrow(x) / length(repeated))
  remain <- nrow(x) - (length(repeated) * full)
  to_add <- repeated[1:remain]
  if (remain == 0) to_add <- NULL

  x$group <- c(rep(repeated, times = full), to_add)
  return(x)
}

page_table <- t(page_table[6:ncol(page_table)]) %>%
  tibble::as_tibble() %>%
  dplyr::select(raw_col = `V1`) %>%
  dplyr::filter(stringr::str_detect(raw_col, "Footnotes|percentage", negate = TRUE)) %>%
  fxn_transpose() %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(id_cols = id, names_from = group, values_from = raw_col) %>%
  dplyr::select(state, rate, rank)


# --------------------------------------------------------------------------------------------------
umemp_df <- readxl::read_excel(
  path = "~/Code/R/datasets/unemplpyment/unemp_2020.xlsx",
  sheet = 1,
  skip = 5,
  col_names = FALSE
) %>%
  janitor::clean_names() %>%
  dplyr::select(
    name = x4,
    year = x5,
    rate = x10
  ) %>%
  na.omit() %>%
  tidyr::extract(
    col = name,
    into = c("county", "state"),
    regex = "^(.*), ([A-Z]{2}).*$",
    remove = FALSE
  ) %>%
  dplyr::mutate(
    county = tolower(stringr::str_remove(county, " County| Parish"))
  )

# albers projection has lat0 = 39 and lat1 = 45 for US map
state_df <- ggplot2::map_data(map = "state", projection = "albers", parameters = c(39, 45))
county_df <- ggplot2::map_data(map = "county", projection = "albers", parameters = c(39, 45))
names(county_df) <- c("long", "lat", "group", "order", "state_name", "county")

county_df$state <- state.abb[match(county_df$state_name, tolower(state.name))]

choropleth <- county_df %>%
  dplyr::left_join(umemp_df, by = c("state", "county")) %>%
  dplyr::arrange(order) # required for geom_polygon

ggplot2::ggplot(
  data = choropleth,
  mapping = ggplot2::aes(long, lat, group = group)
) +
  ggplot2::geom_polygon(
    mapping = ggplot2::aes(fill = rate),
    colour = ggplot2::alpha("white", 1 / 2),
    size = 0.2
  ) +
  ggplot2::geom_polygon(
    data = state_df,
    colour = "white",
    fill = NA
  ) +
  ggplot2::coord_fixed() +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("US unemployment rate by county") +
  ggplot2::theme(
    axis.line = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(), axis.title = ggplot2::element_blank()
  ) +
  viridis::scale_fill_viridis(option = "H")
