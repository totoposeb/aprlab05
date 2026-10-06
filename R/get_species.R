#' Get Species Information
#'
#' Retrieve species-level information for one or more Pokémon.
#'
#' @param names A character vector of Pokémon species names.
#' @return A data frame with columns: ...
#' @export
#' @examplesIf httr2::is_online()
#' get_species("Fuecoco")

get_species <- function(names) {
  if (!is.character(names) || length(names) == 0 ||
      anyNA(names) || !all(nzchar(names))) {
    stop(
      "`names` must be a non-empty character vector ",
      "with no NA and no empty strings."
    )
  }
  
  rows <- lapply(names, function(nm) {
    json <- request_api(paste0("pokemon-species/", nm))
    as.data.frame(parse_species(json), stringsAsFactors = FALSE)
  })
  
  do.call(rbind, rows)
}