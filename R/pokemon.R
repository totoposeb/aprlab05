pokemon <- setRefClass(
  "pokemon", # "pokemon" is the class name
  fields  = list(
    name = "character"
  ), # stored data, and its type
  methods = list(
    initialize = function(name) { # runs automatically on $new()
      # Validates inputs
      stopifnot(
        "`name` must be a single string!" = is.character(name) && length(name) == 1
      )

      # Gets data
      response <- request_api(paste0("pokemon/", name))

      # Parses response
      parsed <- parse_pokemon(response)

      # Gets attributes
      id
      name
      base_experience
      height
      weight
      abilities
      moves
      sprite
      stats
      types
      rarity_level
    },
    data = function() {
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