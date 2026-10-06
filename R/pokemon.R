pokemon <- setRefClass(
  "pokemon", # "pokemon" is the class name
  fields  = list(
    name = "character",
    id = "numeric",
    name = "character",
    base_experience = "numeric",
    height = "numeric",
    weight = "numeric",
    abilities = "character",
    moves = "character",
    sprite = "character",
    stats = "character",
    types = "character",
    rarity_level = "character",
    gender_rate = "numerical",
    evolves_from_species = "character",
    evolution_chain = "character",
    generation = "character"
  ), # stored data, and its type
  methods = list(
    initialize = function(name) { # runs automatically on $new()
      # Validates inputs
      stopifnot(
        "`name` must be a single string!" = is.character(name) && length(name) == 1
      )

      # Gets data
      json <- request_api(paste0("pokemon/", name))

      # Parses response
      parsed <- parse_pokemon(json)

      # Gets attributes
      id <<- parsed$id
      name <<- parsed$name
      base_experience <<- parsed$base_experience
      height <<- parsed$height
      weight <<- parsed$weight
      abilities <<- parsed$abilities
      moves <<- parsed$moves
      sprite <<- parsed$sprite
      stats <<- parsed$stats
      types <<- parsed$types
    },
    data = function() {
      data.frame(
        id = id,
        name = name,
        base_experience = base_experience,
        height = height,
        weight = weight,
        abilities = abilities,
        moves = moves,
        sprite = sprite,
        stats = stats,
        types = types
      )
    },
    show = function() {
    },
    plot = function() {
    },
    show_sprite = function() {
    },
    show_type_effectiveness = function() {
    }
  )
)