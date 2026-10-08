pokemon_response <- list(
  id = 909L,
  name = "fuecoco",
  base_experience = 62L,
  height = 4L,
  weight = 98L,
  abilities = list(
    list(ability = list(name = "blaze")),
    list(ability = list(name = "unaware"))
  ),
  moves = list(
    list(move = list(name = "tackle")),
    list(move = list(name = "body-slam")),
    list(move = list(name = "take-down"))
  ),
  species = list(name = "fuecoco"),
  sprites = list(
    other = list(
      home = list(
        front_default = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/other/home/909.png"
      )
    ),
    front_default = "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/909.png"
  ),
  stats = list(
    list(base_stat = 67L, stat = list(name = "hp")),
    list(base_stat = 45L, stat = list(name = "attack")),
    list(base_stat = 59L, stat = list(name = "defense")),
    list(base_stat = 63L, stat = list(name = "special-attack")),
    list(base_stat = 40L, stat = list(name = "special-defense")),
    list(base_stat = 36L, stat = list(name = "speed"))
  ),
  types = list(list(type = list(name = "fire")))
)

species_response <- list(
  name = "fuecoco",
  capture_rate = 45L,
  is_legendary = FALSE,
  is_mythical = FALSE,
  gender_rate = 1L,
  growth_rate = list(
    name = "medium-slow"
  ),
  evolution_chain = list(
    url = "https://pokeapi.co/api/v2/evolution-chain/479/"
  ),
  generation = list(name = "generation-ix")
)

type_response <- list(
  damage_relations = list(
    no_damage_to = list(),
    half_damage_to = list(
      list(name = "rock"),
      list(name = "fire"),
      list(name = "water"),
      list(name = "dragon")
    ),
    double_damage_to = list(
      list(name = "bug"),
      list(name = "steel"),
      list(name = "grass"),
      list(name = "ice")
    ),
    no_damage_from = list(),
    half_damage_from = list(
      list(name = "bug"),
      list(name = "steel"),
      list(name = "fire"),
      list(name = "grass"),
      list(name = "ice"),
      list(name = "fairy")
    ),
    double_damage_from = list(
      list(name = "ground"),
      list(name = "rock"),
      list(name = "water")
    )
  )
)

fake_api <- function(req) {
  if (grepl("/type/", req$url)) {
    httr2::response_json(body = type_response)
  } else if (grepl("/pokemon-species/", req$url)) {
    httr2::response_json(body = species_response)
  } else {
    httr2::response_json(body = pokemon_response)
  }
}