# Baseline hazards of 'tteICE' objects

This function extracts the baseline cumulative hazards in the survival
models

## Usage

``` r
basehaz.tteICE(object)
```

## Arguments

- object:

  A fitted object returned by the function `tteICE`, `surv.tteICE`, or
  `scr.tteICE`.

## Value

A data frame of baseline cumulative hazards in the working Kaplan-Meier
or Cox models, stratified by treatment groups. The first column is time,
the following columns are baseline cumulative hazards.
