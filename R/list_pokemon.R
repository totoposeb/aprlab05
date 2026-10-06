#' List Pokémon names
#'
#' Returns the names of all Pokémon, or only those of one type.
#' Without a type, it pages through the paginated \code{pokemon} endpoint
#' and collects every name. With a type, it reads the names from the
#' \code{type} endpoint in a single request.
#'
#' @param type A single type name such as \code{"electric"}, or
#'   \code{NULL} (the default) to return every Pokémon.
#'
#' @return A character vector of Pokémon names.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list_pokemon("electric")
#' head(list_pokemon())
#' }
list_pokemon <- function(type = NULL) {
  if (!is.null(type)) {
    if (!is.character(type) || length(type) != 1 || !nzchar(type)) {
      stop("`type` must be a single non-empty string, or NULL.")
    }
    
    json <- request_api(paste0("type/", type))
    
    return(vapply(
      json$pokemon,
      function(x) x$pokemon$name,
      character(1)
    ))
  }
  
  limit <- 500
  offset <- 0
  result <- character(0)
  
  repeat {
    page <- request_api("pokemon", limit = limit, offset = offset)
    
    result <- c(
      result,
      vapply(page$results, function(x) x$name, character(1))
    )
    
    if (is.null(page$`next`)) break
    offset <- offset + limit
  }
  
  result
}