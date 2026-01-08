# Risk prediction at specific time points

This function predicts the potential cumulative incidence function and
treatment effect at specific time points.

## Usage

``` r
riskpredict(fit, timeset = NULL)
```

## Arguments

- fit:

  A fitted object returned by the function `surv.tteICE` or
  `scr.tteICE`.

- timeset:

  Time at which to predict the risk. If `timeset=NULL`, risks will be
  predict at the quartiles of the maximum follow-up time.

## Value

A matrix. The meanings of each row are: time points, potential
cumulative incidences (under treated and under control), treatment
effects, standard errors, and P-values.

## See also

[`scr.tteICE`](https://mephas.github.io/tteICE/reference/scr.tteICE.md),
[`surv.tteICE`](https://mephas.github.io/tteICE/reference/surv.tteICE.md),
[`surv.boot`](https://mephas.github.io/tteICE/reference/surv.boot.md)

## Examples

``` r
## load data
data(bmt)
bmt = transform(bmt, d4=d2+d3)
A = as.numeric(bmt$group>1)
X = as.matrix(bmt[,c('z1','z3','z5')])
## Composite variable strategy,
## nonparametric estimation without covariates
fit1 = scr.tteICE(A, bmt$t1, bmt$d1, bmt$t2, bmt$d2, "composite")
riskpredict(fit1, timeset=c(670,2000))
#>               670        2000
#> CIF1   0.53226259  0.58641246
#> se1    0.05012612  0.04988569
#> CIF0   0.63767315  0.63767315
#> se0    0.07929563  0.07929563
#> ATE   -0.10541056 -0.05126070
#> se     0.09381057  0.09368233
#> p.val  0.26116016  0.58425802
```
