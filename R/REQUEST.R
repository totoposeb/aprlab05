#' send request to PokéAPI and return JASON
#'
#' @param path Character. API path, e.g. "pokemon/pikachu".
#' @return A list containing the parsed JSON response.
#' @noRd
poke_get <- function(path) {
  httr2::request("https://pokeapi.co/api/v2") |># fixed root path, use "root path + character"
  httr2::req_url_path_append(path) |>              #added path "pokemon/{id or name}"
  httr2::req_perform() |>
  httr2::resp_body_json()
}