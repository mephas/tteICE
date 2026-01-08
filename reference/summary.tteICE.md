# Summary of tteICE

This function summarizes the results

## Usage

``` r
# S3 method for class 'tteICE'
summary(object, ...)
```

## Arguments

- object:

  A fitted object returned by the function `surv.tteICE` or
  `scr.tteICE`.

- ...:

  Other augments in function
  [`summary`](https://rdrr.io/r/base/summary.html)

## Value

A list that consists of summaries of a tteICE object: data type,
strategy, estimation method, maximum follow-up time, sample size,
treated sample size, controlled sample size, p-value, and predicted
risks at quartiles

## See also

[`surv.tteICE`](https://mephas.github.io/tteICE/reference/surv.tteICE.md),
[`scr.tteICE`](https://mephas.github.io/tteICE/reference/scr.tteICE.md),
[`print.tteICE`](https://mephas.github.io/tteICE/reference/print.tteICE.md)

## Examples

``` r
## load data
data(bmt)
bmt = transform(bmt, d4=d2+d3)
A = as.numeric(bmt$group>1)
## summarize the results
fit1 = surv.tteICE(A, bmt$t2, bmt$d4, 'natural')
summary(fit1)
#> $dtype
#> [1] "cmprsk"
#> 
#> $strategy
#> [1] "natural"
#> 
#> $method
#> [1] "np"
#> 
#> $maxt
#> [1] 2640
#> 
#> $n
#> [1] 137
#> 
#> $n1
#> [1] 99
#> 
#> $n0
#> [1] 38
#> 
#> $p.val
#> [1] 0.6382412
#> 
#> $est
#>               660        1320        1980        2640
#> CIF1   0.23655610  0.27785687  0.27785687 0.317272930
#> se1    0.04373288  0.04733443  0.04733443 0.062647625
#> CIF0   0.31083347  0.31083347  0.31083347 0.310833467
#> se0    0.07781834  0.07781834  0.07781834 0.077818342
#> ATE   -0.07427737 -0.03297659 -0.03297659 0.006439462
#> se     0.08631459  0.08699188  0.08699188 0.095135528
#> p.val  0.38949010  0.70463092  0.70463092 0.946034598
#> 
#> attr(,"class")
#> [1] "summary.tteICE"
fit2 = tteICE(Surv.ice(t2, d4)~A, data=bmt,
strategy="composite", cov.formula=~z1+z3+z5, method='eff')
summary(fit2)
#> $dtype
#> [1] "cmprsk"
#> 
#> $strategy
#> [1] "composite"
#> 
#> $method
#> [1] "eff"
#> 
#> $maxt
#> [1] 2640
#> 
#> $n
#> [1] 137
#> 
#> $n1
#> [1] 99
#> 
#> $n0
#> [1] 38
#> 
#> $p.val
#> [1] 0.1365913
#> 
#> $est
#>               660        1320        1980        2640
#> CIF1   0.52459066  0.58362684  0.58362684  0.63470334
#> se1    0.05140949  0.05108944  0.05108944  0.05874485
#> CIF0   0.67836383  0.70103992  0.70103992  0.70103992
#> se0    0.06623568  0.06510731  0.06510731  0.06510731
#> ATE   -0.15377316 -0.11741308 -0.11741308 -0.06633659
#> se     0.08384570  0.08275925  0.08275925  0.08769219
#> p.val  0.06665373  0.15597758  0.15597758  0.44936692
#> 
#> attr(,"class")
#> [1] "summary.tteICE"
fit3 = tteICE(Surv.ice(t1, d1, t2, d2)~A, data=bmt,
strategy="composite", cov.formula=~z1+z3+z5, method='eff')
summary(fit3)
#> $dtype
#> [1] "smcmprsk"
#> 
#> $strategy
#> [1] "composite"
#> 
#> $method
#> [1] "eff"
#> 
#> $maxt
#> [1] 2640
#> 
#> $n
#> [1] 137
#> 
#> $n1
#> [1] 99
#> 
#> $n0
#> [1] 38
#> 
#> $p.val
#> [1] 0.1365913
#> 
#> $est
#>               660        1320        1980        2640
#> CIF1   0.52459066  0.58362684  0.58362684  0.63470334
#> se1    0.05140949  0.05108944  0.05108944  0.05874485
#> CIF0   0.67836383  0.70103992  0.70103992  0.70103992
#> se0    0.06623568  0.06510731  0.06510731  0.06510731
#> ATE   -0.15377316 -0.11741308 -0.11741308 -0.06633659
#> se     0.08384570  0.08275925  0.08275925  0.08769219
#> p.val  0.06665373  0.15597758  0.15597758  0.44936692
#> 
#> attr(,"class")
#> [1] "summary.tteICE"
```
