#' Type Matchup
#'
#' Compare the types of two Pokémon.
#'
#' @param name1 The name of the first Pokémon.
#' @param name2 The name of the second Pokémon.
#' @return A data frame with one row: \code{attack} is the multiplier when
#'   the first attacks the second, and \code{defense} when the second attacks
#'   the first.
#' @export
#' @examplesIf httr2::is_online()
#' type_matchup("pikachu", "fuecoco")
type_matchup <- function(name1, name2) {
  # Validates inputs
  stopifnot(
    "`name1` should be a single character name!" = is.character(name1) &&
      length(name1) == 1 && !is.na(name1) && nzchar(trimws(name1)),
    "`name2` should be a single character name!" = is.character(name2) &&
      length(name2) == 1 && !is.na(name2) && nzchar(trimws(name2))
  )

  # Gets both Pokémon objects
  pokemon1 <- pokemon$new(name1)
  pokemon2 <- pokemon$new(name2)

  # Gets pokemon1 type_effectiveness table and filters it for pokemon2's types
  # Each value is one of pokemon2's types hitting pokemon1
  pokemon1_type_effectiveness <- pokemon1$get_type_effectiveness()
  defense <- pokemon1_type_effectiveness[pokemon2$types, "defense"]

  # Gets pokemon2 type_effectiveness table and filters it for pokemon1's types
  # Each value is one of pokemon1's types hitting pokemon2
  pokemon2_type_effectiveness <- pokemon2$get_type_effectiveness()
  attack <- pokemon2_type_effectiveness[pokemon1$types, "defense"]

  # Keeps the best type of each attacker and returns both values in one row
  data.frame(attack = max(attack), defense = max(defense))
}