# Two-Way Fixed Effects weights

**Note**: this package is in progress (5/15).

------------------------------------------------------------------------

In this repository I've update a code from from [Shuo Zhang](https://github.com/shuo-zhang-ucsb/twowayfeweights) repository in [Chaisemartin & D'Haultfoeuille (2018)](https://www.aeaweb.org/articles?id=10.1257/aer.20181169).

Basically, this repo contains a faster way to estimate the Shuo Zhang's code and fix/optimize some parts using [`data.table`](https://github.com/Rdatatable/data.table).

## TWFEweights vs twowayfeweights

I was particularly interested in the performance increase of each function.

``` r
library(dplyr)
library(did2s)

data = did2s::df_hom %>%
  mutate(treat = if_else(treat == T, 1, 0),
         crtl = rbinom(46500, size=1, p=0.7)) %>% 
  rename(Y = dep_var,
         G = group,
         T = year,
         D = treat)
```

### `twowayfeweights_rename_var()` function

### `twowayfeweights_normalize_var()`

sda

``` r
bench = microbenchmark::microbenchmark(
  "twowayfeweights" = twowayfeweights_normalize_var(data, c("state", "crtl")),
  "TWFEweights" =  twowayfeweights_normalize_var2(data, c(state, crtl))
)

#Unit: milliseconds
#           expr      min        lq      mean   median       uq      max neval cld
#twowayfeweights 136.8073 154.54795 177.40709 170.3359 191.0102 460.9254   100   b
#TWFEweights  18.0254  20.57045  25.64296  24.9279  29.0076  45.8401   100  a 

ggplot2::autoplot(bench)
```

![](pl1.png){width="460"}

## Installation (in progress)

You can install the development version from GitHub:

``` r
devtools::install_github("cdcarrion/TWFEweights")
```

s
