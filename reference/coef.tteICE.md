# Coefficients of 'tteICE' objects

This function extracts the coefficients in the Cox models

## Usage

``` r
# S3 method for class 'tteICE'
coef(object, ...)
```

## Arguments

- object:

  A fitted object returned by the function `tteICE`, `surv.tteICE`, or
  `scr.tteICE`.

- ...:

  Other arguments in function
  [`coef.default`](https://rdrr.io/r/stats/coef.html)

## Value

A list of coefficients of covariates in the working Cox models,
stratified by treatment groups. For the treatment policy strategy and
composite variable strategy, only one Cox model is fit (for the primary
outcome event or the composite event). In these two strategies, `coef1`
is the coefficients in the treated group, `coef0` is the coefficients in
the control group. For other strategies, Cox models are fitted for each
event (primary outcome event and intercurrent event). In these
strategies, `coef11` is the coefficients for the primary outcome event
in the treatment group, `coef10` is the coefficients for the primary
outcome event in the control group, `coef21` is the coefficients for the
intercurrent event in the treated group, `coef20` is the coefficients
for the intercurrent in the control group.
