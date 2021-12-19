library(magrittr)

# Functions ----------------------------------------------------------------------------------------
fxn_make_tibble <- function(x) {
  output <- data_list[x] %>%
    purrr::map_df(tibble::as_tibble)
}

fxn_get_eng <- function(x) {
  x %>%
    dplyr::filter(local_language_id == subset(languages, identifier == "en")$id)
}

# Read data ----------------------------------------------------------------------------------------
path_oi <- "data"
file_list <- list.files(path_oi, pattern = "*.csv", full.names = TRUE)
file_names <- gsub(pattern = "data/|.csv", replacement = "", x = file_list)

data_list <- vector(mode = "list", length = length(file_list))
data_list <- purrr::map(.x = file_list, .f = readr::read_csv)
names(data_list) <- file_names

pokedexes <- fxn_make_tibble("pokedexes")
pokemon_species <- fxn_make_tibble("pokemon_species")
languages <- fxn_make_tibble("languages")
generation_names <- fxn_make_tibble("generation_names") %>% fxn_get_eng()
pokemon_colors <- fxn_make_tibble("pokemon_colors")
pokemon_types <- fxn_make_tibble("pokemon_types")
types <- fxn_make_tibble("types")
pokemon_shapes <- fxn_make_tibble("pokemon_shapes")

# Wrangle data -------------------------------------------------------------------------------------
pokemon_data <- pokemon_species %>%
  dplyr::mutate(icon = glue::glue("<img src='{identifier}.png'></img>")) %>%
  dplyr::select(
    icon, id, identifier, generation_id, color_id, shape_id, is_legendary, is_mythical
  ) %>%
  dplyr::left_join(
    generation_names %>% dplyr::rename(generation = name),
    by = "generation_id"
  ) %>%
  dplyr::left_join(
    pokemon_colors %>% dplyr::rename(color = identifier),
    by = c("color_id" = "id")
  ) %>%
  dplyr::left_join(
    pokemon_shapes %>% dplyr::rename(shape = identifier),
    by = c("shape_id" = "id")
  ) %>%
  dplyr::left_join(
    pokemon_types %>%
      tidyr::pivot_wider(names_from = slot, names_glue = "type_id_{slot}", values_from = type_id),
    by = c("id" = "pokemon_id")
  ) %>%
  dplyr::left_join(
    types %>% dplyr::select(type_id = id, type = identifier) %>% dplyr::rename(type_1 = type),
    by = c("type_id_1" = "type_id")
  ) %>%
  dplyr::left_join(
    types %>% dplyr::select(type_id = id, type = identifier) %>% dplyr::rename(type_2 = type),
    by = c("type_id_2" = "type_id")
  ) %>%
  tidyr::replace_na(replace = list(shape = "unknown")) %>%
  dplyr::mutate(
    generation = as.factor(generation),
    color = as.factor(color),
    shape = as.factor(shape),
    type_1 = as.factor(type_1),
    type_2 = as.factor(type_2),
    legendary = dplyr::if_else(is_legendary == 1, TRUE, FALSE),
    mythical = dplyr::if_else(is_mythical == 1, TRUE, FALSE)
  ) %>%
  dplyr::select(-c(dplyr::contains("_id"), dplyr::contains("is_")))
