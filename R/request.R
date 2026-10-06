#' send request to PokéAPI and return JASON
#'
#' @param path Character. API path, e.g. "pokemon/pikachu".
#' @return A list containing the parsed JSON response.
#' @noRd
request_api <- function(path, limit = NULL, offset = NULL) {
  stopifnot(
    is.character(path),
    length(path) == 1,
    nzchar(path)
  )
  
  resp <- httr2::request("https://pokeapi.co/api/v2") |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(limit = limit, offset = offset) |>
    httr2::req_user_agent("aprlab05 R package") |>
    httr2::req_cache(path = file.path(tempdir(), "aprlab05")) |>
    httr2::req_throttle(capacity = 100, fill_time_s = 60) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  
  status <- httr2::resp_status(resp)
  
  if (status == 404) {
    stop("Path '", path, "' not found. Check the spelling.")
  }

  if (status != 200) {
    stop("Request failed with status ", status, ".")
  }

  text <- httr2::resp_body_string(resp)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}