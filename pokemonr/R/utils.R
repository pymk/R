utils::globalVariables(c("parsed", "name", "value", "n_v"))

# Get the parameters from ellipsis (...) if they were provided
fxn_get_params <- function(l) {
  v <- l %>%
    tibble::enframe() %>%
    dplyr::mutate(n_v = paste0(name, "=", value)) %>%
    dplyr::pull(n_v) %>%
    paste0(collapse = "&")
  return(v)
}

# Return the error if the API call fail
fxn_check_api_return <- function(x) {
  if (httr::http_type(x) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # Return the error if API call fails
  if (httr::http_error(x)) {
    stop(
      sprintf(
        "API request failed [%s]\n%s\n<%s>",
        httr::status_code(x),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
}

# Tidy up the input list (single)
fxn_tidy_single <- function(x) {
  len <- length(x$name)

  v <- vector(mode = "list", length = len)

  for (i in seq(len)) {
    d <- NULL$results[[i]]
    d$id <- i
    d$id <- purrr::pluck(x, "id")
    d$name <- purrr::pluck(x, "species", "name")
    d$api <- purrr::pluck(x, "species", "url")
    d$sprite <- purrr::pluck(x, "sprites", "front_default")
    v[[i]] <- d
  }
  output <- dplyr::bind_rows(v)
  return(output)
}

# Tidy up the input list (bulk)
fxn_tidy_bulk <- function(x) {
  output <- purrr::map_df(
    .x = x$results,
    .f = purrr::pluck
  ) %>%
    dplyr::mutate(
      id = basename(url),
      sprite = paste0("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/", id, ".png")
    ) %>%
    dplyr::select(id, name, api = url, sprite)
  return(output)
}
