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

# Ensure the "page" and "pageSize" parameters are numeric
fxn_check_page <- function(x) {
  if (!is.numeric(x) & !is.na(x)) {
    stop("Numeric value needs to be provided for 'page' and 'pageSize'")
  }
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
