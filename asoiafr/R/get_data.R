#' Get Data
#'
#' @description
#' The \code{get_data()} function is used to get structured data from the
#' \href{https://anapioficeandfire.com/Documentation}{"An API of Ice And Fire"} API service.
#'
#' @details
#' By default, only 10 results are returned. This value can be changed using the optional
#' \code{"pageSize"} parameter. Additionally, to specify which page to access, the optional
#' \code{"page"} parameter can be used.
#'
#' @param x API resource to call.
#' Options are \code{"books"}, \code{"characters"}, or \code{"houses"}.
#' @param return_as_tibble When \code{TRUE}, the output will be a tibble (default); otherwise,
#' a list will be returned.
#' @param ... Any other optional \href{https://anapioficeandfire.com/Documentation}{parameter}
#' that can be provided.
#'
#' @return An object of data for the resource parameter that was requested.
#' @export
#'
#' @examples
#' \dontrun{
#' all_books <- get_data("books", pageSize = 20)
#' houses_5 <- get_data("houses", page = 2, pageSize = 5)
#' characters_10 <- get_data("characters")
#' }
#' @importFrom dplyr mutate pull
#' @importFrom httr modify_url GET content http_type http_error status_code
#' @importFrom jsonlite fromJSON
#' @importFrom rlang arg_match is_missing
#' @importFrom tibble as_tibble enframe
get_data <- function(x, return_as_tibble = TRUE, ...) {
  params <- list(...)

  # Validate input ---------------------------------------------------------------------------------
  rlang::arg_match(
    arg = x,
    values = c("books", "characters", "houses")
  )

  # Set "return_as_tibble" parameter to TRUE if it is missing
  if (rlang::is_missing(return_as_tibble)) {
    return_as_tibble <- TRUE
  }

  # A boolean value is required for "return_as_tibble" parameter
  if (!is.logical(return_as_tibble)) {
    stop("A TRUE or FALSE value is required for 'return_as_tibble' parameter")
  }

  # Numeric values should be provided for "page" and "pageSize"
  if (!is.null(params$page)) {
    fxn_check_page(params$page)
  }
  if (!is.null(params$pageSize)) {
    fxn_check_page(params$pageSize)
  }

  # Create endpoint --------------------------------------------------------------------------------
  path_component <- paste0("/api/", x)
  param_components <- fxn_get_params(params)

  url <- httr::modify_url(
    url = "https://www.anapioficeandfire.com/",
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
    simplifyVector = return_as_tibble
  )

  if (return_as_tibble) {
    parsed <- tibble::as_tibble(parsed)
  }

  # Return result ----------------------------------------------------------------------------------
  return(parsed)
}
