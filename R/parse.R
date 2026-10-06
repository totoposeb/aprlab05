# 1. get the basic attributes of a pokemon
# e.g. "pokemon/pikachu"
parse_pokemon <- function(json) {
  stats_values <- vapply(
    json$stats,
    function(s) s$base_stat,
    numeric(1)
  )
  
  stats_names <- vapply(
    json$stats,
    function(s) s$stat$name,
    character(1)
  )
  
  stats_names <- gsub("-", "_", stats_names)
  names(stats_values) <- stats_names
  
  list(
    id = json$id,
    name = json$name,
    base_experience = json$base_experience,
    height = json$height,
    weight = json$weight,
    abilities = vapply(
      json$abilities,
      function(a) a$ability$name,
      character(1)
    ),
    moves = vapply(
      json$moves,
      function(m) m$move$name,
      character(1)
    ),
    types = vapply(
      json$types,
      function(t) t$type$name,
      character(1)
    ),
    stats = stats_values,
    sprite = if (length(json$sprites$front_default) == 0) {
      NA_character_
    } else {
      json$sprites$front_default
    },
    species = json$species$name
  )
}


# 2. get the species attributes
# e.g. "pokemon-species/pikachu"
parse_species <- function(json) {
  name <- json$name
  capture_rate <- json$capture_rate
  growth_rate <- json$growth_rate$name
  is_legendary <- json$is_legendary
  is_mythical <- json$is_mythical
  gender_rate <- json$gender_rate
  evolves_from_species <- if (length(json$evolves_from_species$name) == 0) {
    NA_character_
  } else {
    json$evolves_from_species$name
  }
  evolution_chain <- if (length(json$evolution_chain$url) == 0) {
    NA_character_
  } else {
    json$evolution_chain$url
  }
  generation <- json$generation$name

  rarity_level <- if (isTRUE(is_legendary)) {
    "Legendary"
  } else if (isTRUE(is_mythical)) {
    "Mythical"
  } else if (is.na(capture_rate)) {
    NA_character_
  } else if (capture_rate <= 50) {
    "Rare"
  } else if (capture_rate <= 150) {
    "Uncommon"
  } else {
    "Common"
  }
  
  list(
    name = name,
    capture_rate = capture_rate,
    growth_rate = growth_rate,
    rarity_level = rarity_level,
    gender_rate = gender_rate,
    evolves_from_species = evolves_from_species,
    evolution_chain = evolution_chain,
    generation = generation
  )
}


#3. for type

type_names <- function() {
  c(
    "normal", "fire", "water", "electric", "grass", "ice",
    "fighting", "poison", "ground", "flying", "psychic", "bug",
    "rock", "ghost", "dragon", "dark", "steel", "fairy"
  )
}

names_from <- function(x) {
  vapply(x, function(e) e$name, character(1))
}

parse_type <- function(json) {
  attack <- stats::setNames(rep(1, 18), type_names())
  defense <- stats::setNames(rep(1, 18), type_names())
  
  dr <- json$damage_relations
  
  attack[names_from(dr$double_damage_to)] <- 2
  attack[names_from(dr$half_damage_to)] <- 0.5
  attack[names_from(dr$no_damage_to)] <- 0
  
  defense[names_from(dr$double_damage_from)] <- 2
  defense[names_from(dr$half_damage_from)] <- 0.5
  defense[names_from(dr$no_damage_from)] <- 0
  
  cbind(attack = attack, defense = defense)
}