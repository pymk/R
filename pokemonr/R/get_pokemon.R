#' Get Pokemon
#'
#' @description
#' The \code{get_pokemon()} function is used to get structured data from the
#' \href{https://pokeapi.co/}{"PokeAPI"} API service.
#'
#' @details There are two ways to get the data:
#' \enumerate{
#'   \item By providing the Pokemon name (lowercase) or Pokemon ID.
#'   \item By providing \code{limit} and \code{offset} parameters.
#' }
#'
#' @param x Pokemon name or ID.
#' @param ... Any other optional \href{https://pokeapi.co/docs/v2}{PokeAPI parameter}.
#'
#' @return A dataframe for the requested Pokemon.
#' @export
#'
#' @examples
#' \dontrun{
#' mew <- get_pokemon("mew")
#' gen_1 <- get_pokemon(limit = 151, offset = 0)
#' }
#' @importFrom dplyr mutate pull bind_rows
#' @importFrom httr modify_url GET content http_type http_error status_code
#' @importFrom jsonlite fromJSON
#' @importFrom rlang arg_match is_missing
#' @importFrom tibble as_tibble enframe
#' @importFrom purrr map_df pluck map
#' @importFrom glue glue
#' @importFrom gt gt html
#' @importFrom gtExtras gt_merge_stack gt_img_rows
get_pokemon <- function(x, ...) {
  params <- list(...)

  if (rlang::is_missing(x)) {
    x <- NULL
  }

  # Create endpoint --------------------------------------------------------------------------------
  path_component <- paste("/api/v2/pokemon", x, sep = "/")
  param_components <- fxn_get_params(params)

  url <- httr::modify_url(
    url = "https://pokeapi.co/",
    path = path_component,
    query = param_components
  )

  # GET response -----------------------------------------------------------------------------------
  resp <- httr::GET(url)

  # Verify response
  fxn_check_api_return(resp)

  # Parse result -----------------------------------------------------------------------------------
  parsed <- jsonlite::fromJSON(
    txt = httr::content(resp, "text"),
    simplifyVector = FALSE
  )

  # Return result ----------------------------------------------------------------------------------
  if (is.null(x)) {
    output <- fxn_tidy_bulk(parsed)
  } else {
    output <- fxn_tidy_single(parsed)
  }

  x <- output %>%
    dplyr::mutate(
      api = glue::glue("<a href='{api}'>{name}</a>"),
      api = purrr::map(api, ~ gt::html(as.character(.x)))
    ) %>%
    dplyr::select(-name) %>%
    gt::gt() %>%
    gtExtras::gt_img_rows(sprite)

  return(x)
}
