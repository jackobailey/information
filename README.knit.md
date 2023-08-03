---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# `information`: Information Theoretic Tools in R

<!-- badges: start -->
<!-- badges: end -->

<img src="https://raw.githubusercontent.com/jackobailey/information/master/inst/figures/information_hex.png" alt="information hexlogo" align="right" width="275" style="padding: 0 15px; float: right;"/>

The `information` package contains a suite of important information theoretic functions. This includes entropy, relative entropy, mutual information, amongst others. What's more, these functions are simple, lightweight, and rely on few dependencies. As such, they should be robust and easy to put into practice when applying information theoretic insights in R.

## Installation

For now, `information` is available only on GitHub. You can install it using `devtools` by running the following code in R:

``` r
devtools::install_github("jackobailey/information")
```

## Example

All of the functions in `information` take probability distributions as inputs. Here is a simple example showing how to compute the entropy of 100 fair coin flips in bits.


```r

# Load package

library(information)


# Set seed

set.seed(01)


# Simulate 100 fair coin flips

coin_flips <- 
  sample(
    c("H", "T"),
    size = 100,
    replace = T
    ) |> 
  table() |> 
  prop.table()


# Compute entropy

entropy(coin_flips)
#> [1] 0.9997114
```
