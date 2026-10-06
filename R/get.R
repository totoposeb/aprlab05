#1. 6 kinds different basic attributes
get_pokemon_info <- function(pokemon_names) {
  results <- list()
  
  for (nm in pokemon_names) {
    # ask for info&species
    pokemon_json <- poke_get(paste0("pokemon/", nm))
    species_json <- poke_get(paste0("pokemon-species/", nm))
    
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


# 2. different types

#' get_type_effectiveness("fire")
get_type_effectiveness <- function(attacking_type) {
  json <- poke_get(paste0("type/", attacking_type))
  
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


