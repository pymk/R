
# pokemonr

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

This package allows you to get some structured data from the "PokeAPI" API service.

## Installation

You can install the development version of pokemonr from GitHub by forking [the pokemonr branch](https://github.com/pymk/R/tree/master/pokemonr) and installing with:

``` r
install.packages(".", repos = NULL, type="source")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pokemonr)

get_pokemon("mew")
```
