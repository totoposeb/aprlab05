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
    image = "character",
    stats = "numeric",
    types = "character",
    species = "character",
    rarity_level = "character",
    gender_rate = "numeric",
    evolves_from_species = "character",
    evolution_chain = "character",
    generation = "character"
  ),
  methods = list(
    initialize = function(name) { # runs automatically on $new()
      # Validates inputs
      stopifnot(
        "`name` must be a single string!" = (
          is.character(name) && length(name) == 1 && !is.na(name) &&
          nzchar(trimws(name))
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
      image <<- parsed$image
      stats <<- parsed$stats
      types <<- parsed$types
      species <<- parsed$species
    },
    data = function() {
      # Returns the attributes in a data.frame
      data.frame(
        id = id,
        name = name,
        base_experience = base_experience,
        height = height,
        weight = weight,
        abilities = paste(abilities, collapse = ", "),
        types = paste(types, collapse = ", "),
        stats_total = sum(stats),
        hp = stats[["hp"]],
        attack = stats[["attack"]],
        defense = stats[["defense"]],
        special_attack = stats[["special_attack"]],
        special_defense = stats[["special_defense"]],
        speed = stats[["speed"]]
      )
    },
    show = function() {
      # Prints out basic information about the Pokémon
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
      # Creates a data.frame with the base stats info
      df <- data.frame(
        stat = factor(names(stats), levels = rev(names(stats))),
        value = unname(stats),
        label = c("HP", "Attack", "Defense", "Sp. Atk", "Sp. Def", "Speed"),
        color = c(
          "#69DC12", "#EFCC18", "#E86412", "#14C3F1", "#4A6ADF", 
          "#D51DAD"
        )
      )
      # Plots the base stats for the pokemon
      p1 <- ggplot2::ggplot(df, ggplot2::aes(y = stat)) +
        ggplot2::geom_col(ggplot2::aes(x = value, fill = color), width = 0.7) +
        ggplot2::geom_text(
          ggplot2::aes(x = -86, label = label), hjust = 0, size = 4.5
        ) + ggplot2::geom_text(
          ggplot2::aes(x = -8, label = value), hjust = 1, size = 4.5
        ) + ggplot2::scale_fill_identity() +
        ggplot2::scale_x_continuous(limits = c(-90, 255), expand = c(0, 0)) +
        ggplot2::labs(
          title = toupper(name), subtitle = "Base stats", caption = paste(
            "Total:", sum(stats)
          )
        ) + ggplot2::theme_void(base_size = 14) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", colour = "grey20"),
          plot.subtitle = ggplot2::element_text(colour = "grey20"),
          plot.caption = ggplot2::element_text(size = 14, hjust = 0),
          plot.background = ggplot2::element_rect(
            fill = "white", colour = "grey70", linewidth = 1
          ),
          plot.margin = ggplot2::margin(12, 16, 12, 16)
        )

      base::print(p1)

      # Draws the sprite in the upper right corner, as tall as the title area
      if (!is.na(sprite)) {
        grid::grid.raster(
          request_sprite(sprite),
          x = grid::unit(1, "npc") - grid::unit(16, "pt"),
          y = grid::unit(1, "npc") - grid::unit(12, "pt"),
          just = c("right", "top"),
          height = grid::unit(43, "pt"),
          interpolate = FALSE
        )
      }

      invisible(p1)
    },
    show_sprite = function() {
      # Checks if there is a valid URL
      stopifnot(
        "There is no sprite for this Pok\u00e9mon!" = !is.na(sprite)
      )

      # Draws the sprite
      draw_sprite(request_sprite(sprite))
    },
    show_image = function() {
      # Checks if there is a valid URL
      stopifnot(
        "There is no image for this Pok\u00e9mon!" = !is.na(image)
      )

      # Draws the sprite
      draw_sprite(request_sprite(image))
    },
    show_type_effectiveness = function() {

    },
    get_species_info = function() {
      # Calls the API
      json <- request_api(paste0("pokemon-species/", species))

      # Parses the pokemon-species/ response
      parsed <- parse_species(json)

      rarity_level <<- parsed$rarity_level
      gender_rate <<- parsed$gender_rate
      evolves_from_species <<- parsed$evolves_from_species
      evolution_chain <<- parsed$evolution_chain
      generation <<- parsed$generation

      return(parsed)
    }
  )
)