
# aprlab05

<!-- badges: start -->
[![R-CMD-check](https://github.com/totoposeb/aprlab05/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/totoposeb/aprlab05/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This repository contains an R package to get data about Pokémon from [PokéAPI](https://pokeapi.co/), using a reference class (RC): `pokemon`

## Installation

You can install the development version of aprlab05 from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
options(pkg.build_vignettes = TRUE)
pak::pak("totoposeb/aprlab05")
```

## Example

This is a basic example which shows you how to get the data of a Pokémon with `pokemon`:

``` r
library(aprlab05)
p <- pokemon$new("fuecoco")
p$data()
p$plot_stats()
p$get_type_effectiveness()
```

## Shiny app

A Shiny app that uses this package is available at [SummerDaMu/aprlab05shiny](https://github.com/SummerDaMu/aprlab05shiny). You can run it with:

``` r
shiny::runGitHub("aprlab05shiny", "SummerDaMu")
```

