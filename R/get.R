#' 1.Get basic attributes information about one or more Pokémon
#'
#' Retrieves base stats, capture rate, growth rate, and a rarity level
#' for each Pokémon from PokéAPI. Results are cached, so repeated calls
#' for the same Pokémon return instantly.
#'
#' @param pokemon_names Character vector of Pokémon names or IDs,
#'   e.g. \code{"Pikachu"} or \code{c("Pikachu", "Fuecoco")}.
#'
#' @return A data.frame with one row per Pokémon and the following columns:
#'   \code{name}, \code{hp}, \code{attack}, \code{defense},
#'   \code{sp_attack}, \code{sp_defense}, \code{speed},
#'   \code{capture_rate}, \code{growth_rate}, \code{rarity_level}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_pokemon_info("Pikachu")
#' get_pokemon_info(c("Pikachu", "Fuecoco"))
#' }
get_pokemon_info <- function(pokemon_names) {
  results <- list()
  
  for (nm in pokemon_names) {
    # ask for info&species
    pokemon_json <- request_api(paste0("pokemon/", nm))
    species_json <- request_api(paste0("pokemon-species/", nm))
    
    # build two data.frame
    stats_df   <- parse_pokemon(pokemon_json)
    species_df <- parse_species(species_json)
    
    # merge by name
    merged <- merge(stats_df, species_df, by = "name")
    
    # save to list
    results[[nm]] <- merged
  }
  
  # return the result
  do.call(rbind, results)
}

#' 2.Get type effectiveness for an attacking type
#'
#' Returns how effective moves of one type are against each of the
#' 18 Pokémon types. Multipliers are 2, 0.5, 0, or 1.
#'
#' @param attacking_type Character. One of the 18 Pokémon types,
#'   e.g. \code{"fire"}.
#'
#' @return A data.frame with 18 rows and 3 columns:
#'   \code{attacking_type}, \code{defending_type}, \code{multiplier}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_type_effectiveness("fire")
#' get_type_effectiveness("water")
#' }
get_type_effectiveness <- function(attacking_type) {
  json <- request_api(paste0("type/", attacking_type))
  
  double_types <- purrr::map_chr(json$damage_relations$double_damage_to, "name")
  half_types   <- purrr::map_chr(json$damage_relations$half_damage_to, "name")
  no_types     <- purrr::map_chr(json$damage_relations$no_damage_to, "name")
  
  all_types <- c(
    "normal", "fire", "water", "electric", "grass", "ice",
    "fighting", "poison", "ground", "flying", "psychic", "bug",
    "rock", "ghost", "dragon", "dark", "steel", "fairy"
  )
  
  multipliers <- purrr::map_dbl(all_types, function(defender) {
    if (defender %in% double_types) {
      2
    } else if (defender %in% half_types) {
      0.5
    } else if (defender %in% no_types) {
      0
    } else {
      1
    }
  })
  
  data.frame(
    attacking_type = json$name,
    defending_type = all_types,
    multiplier     = multipliers,
    stringsAsFactors = FALSE
  )
}


