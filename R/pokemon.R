pokemon <- setRefClass(
  "pokemon", # "pokemon" is the class name
  fields  = list(
    id = "numeric",
    name = "character",
    base_experience = "numeric",
    height = "numeric",
    weight = "numeric",
    abilities = "character",
    moves = "character",
    sprite = "character",
    stats = "numeric",
    types = "character",
    species = "character",
    rarity_level = "character",
    gender_rate = "numeric",
    evolves_from_species = "character",
    evolution_chain = "character",
    generation = "character"
  ), # stored data, and its type
  methods = list(
    initialize = function(name) { # runs automatically on $new()
      # Validates inputs
      stopifnot(
        "`name` must be a single string!" = (
          is.character(name) && length(name) == 1 && !is.na(name) && nzchar(trimws(name))
        )
      )

      # Gets data
      json <- request_api(paste0("pokemon/", tolower(trimws(name))))

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
      species <<- parsed$species
    },
    data = function() {
      data.frame(
        id = id,
        name = name,
        base_experience = base_experience,
        height = height,
        weight = weight,
        abilities = paste(abilities, collapse = ", "),
        sprite = sprite,
        types = paste(types, collapse = ", ")
      )
    },
    show = function() {
      cat("Pok\u00e9mon:\n")
      cat(
        "#", id, " - ", toupper(name), "\n",
        sep = ""
      )
      cat(
        "Types: ", paste(types, collapse = ", "), "\n",
        sep = ""
      )
    },
    plot_stats = function() {
    },
    show_sprite = function() {
    },
    show_type_effectiveness = function() {
    }
  )
)