
# 1.get the basic attribute:hp,attack,defense,sp_attack,sp_denfense,spped of pokemon
# e.g. pokemon/pikachu
parse_pokemon <- function(json){ #use dataframe for columns and rows
  data.frame(
    name = json$name,
    hp = json$stat[[1]]$base_stat, # basic value
    attack = json$stat[[2]]$base_stat,
    defense = json$stat[[3]]$base_stat,
    sp_attack = json$stat[[4]]$base_stat,
    sp_defense = json$stat[[5]]$base_stat,
    speed = json$stat[[6]]$base_stat
  )
}

# 2.get the species attributes.
parse_species <- function(json) { # e.g."pokemon-species/pikachu"
  name = json$name
  capture_rate = json$capture_rate
  growth_rate = json$growth_rate$name
  is_legendary = json$is_legendary
  is_mythical = json$is_mythical

# define different rarity_level
rarity_level <- if(is_legendary){
  'Legendary' 
} else if (is_mythical){
  'Mythical'
} else if (capture_rate <= 50){
  'Rare'
} else if (capture_rate <=150){
  'Uncommon'
} else {
  'Common'
}
data.frame(
  name = json$name,
  capture_rate = capture_rate,
  growth_rate = growth_rate,
  rarity_level = rarity_level,
  stringsAsFactors = FALSE # keep the original type, avoid chararter mistake.
)
}


