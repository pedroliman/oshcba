# oshcba

[![Travis-CI Build Status](https://travis-ci.org/pedroliman/oshcba.svg?branch=master)](https://travis-ci.org/pedroliman/oshcba)
[![Coverage Status](https://img.shields.io/codecov/c/github/pedroliman/oshcba/master.svg)](https://codecov.io/github/pedroliman/oshcba?branch=master)

This library is meant to evidence the value of investing in people's health at work. In other words, we do a Monte Carlo Simulation of a Cost-Benefit Analysis for a set of Organizational Health Initiatives.

## Installation

You can install oshcba from github with:


``` r
# install.packages("devtools")
devtools::install_github("pedroliman/oshcba")
```

## Example

``` r
## simulating
resultados = oshcba::simular_cba("Dados.xlsx")
```
