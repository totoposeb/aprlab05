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
  
  data.frame(
    name = name,
    capture_rate = capture_rate,
    growth_rate = growth_rate,
    rarity_level = rarity_level,
    stringsAsFactors = FALSE
  )
}