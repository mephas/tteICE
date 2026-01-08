# Print a short summary of the estimated treatment effect

This function summarizes the results

## Usage

``` r
# S3 method for class 'tteICE'
print(x, digits = 4, ...)
```

## Arguments

- x:

  A fitted object returned by the function `surv.tteICE` or
  `scr.tteICE`.

- digits:

  The digits of the results

- ...:

  Other augments in function
  [`print.default`](https://rdrr.io/r/base/print.default.html)

## Value

Print the summary of a tteICE object

## See also

[`surv.tteICE`](https://mephas.github.io/tteICE/reference/surv.tteICE.md),
[`scr.tteICE`](https://mephas.github.io/tteICE/reference/scr.tteICE.md)

## Examples

``` r
## load data
data(bmt)
bmt = transform(bmt, d4=d2+d3)
A = as.numeric(bmt$group>1)
## print the results
for (st in c('composite','natural','removed','whileon','principal')){
 fit = surv.tteICE(A, bmt$t2, bmt$d4, st)
 print(fit)
}
#> Data type: competing risks 
#> Strategy: composite variable strategy 
#> Estimation method: nonparametric estimation 
#> Observations: 137 (including 99 treated and 38 control)
#> Maximum follow-up time: 2640 
#> P-value of the average treatment effect: 0.5907 
#> -----------------------------------------------------------------------
#> The estimated cumulative incidences and treatment effects at quartiles:
#>           660    1320    1980    2640
#> CIF1   0.5323  0.5864  0.5864  0.6299
#> se1    0.0501  0.0499  0.0499  0.0617
#> CIF0   0.6087  0.6377  0.6377  0.6377
#> se0    0.0803  0.0793  0.0793  0.0793
#> ATE   -0.0764 -0.0513 -0.0513 -0.0078
#> se     0.0946  0.0937  0.0937  0.1005
#> p.val  0.4192  0.5843  0.5843  0.9384
#> 
#> Data type: competing risks 
#> Strategy: hypothetical strategy (controlling the hazard of ICEs) 
#> Estimation method: nonparametric estimation 
#> Observations: 137 (including 99 treated and 38 control)
#> Maximum follow-up time: 2640 
#> P-value of the average treatment effect: 0.6382 
#> -----------------------------------------------------------------------
#> The estimated cumulative incidences and treatment effects at quartiles:
#>           660    1320    1980   2640
#> CIF1   0.2366  0.2779  0.2779 0.3173
#> se1    0.0437  0.0473  0.0473 0.0626
#> CIF0   0.3108  0.3108  0.3108 0.3108
#> se0    0.0778  0.0778  0.0778 0.0778
#> ATE   -0.0743 -0.0330 -0.0330 0.0064
#> se     0.0863  0.0870  0.0870 0.0951
#> p.val  0.3895  0.7046  0.7046 0.9460
#> 
#> Data type: competing risks 
#> Strategy: hypothetical strategy (removing ICEs) 
#> Estimation method: nonparametric estimation 
#> Observations: 137 (including 99 treated and 38 control)
#> Maximum follow-up time: 2640 
#> P-value of the average treatment effect: 0.6382 
#> -----------------------------------------------------------------------
#> The estimated cumulative incidences and treatment effects at quartiles:
#>           660    1320    1980   2640
#> CIF1   0.2795  0.3483  0.3483 0.4168
#> se1    0.0497  0.0559  0.0559 0.0842
#> CIF0   0.4044  0.4044  0.4044 0.4044
#> se0    0.0936  0.0936  0.0936 0.0936
#> ATE   -0.1248 -0.0561 -0.0561 0.0125
#> se     0.1060  0.1090  0.1090 0.1259
#> p.val  0.2389  0.6070  0.6070 0.9211
#> 
#> Data type: competing risks 
#> Strategy: while on treatment strategy 
#> Estimation method: nonparametric estimation 
#> Observations: 137 (including 99 treated and 38 control)
#> Maximum follow-up time: 2640 
#> P-value of the average treatment effect: 0.7138 
#> -----------------------------------------------------------------------
#> The estimated cumulative incidences and treatment effects at quartiles:
#>           660    1320    1980   2640
#> CIF1   0.2389  0.2824  0.2824 0.3235
#> se1    0.0432  0.0462  0.0462 0.0617
#> CIF0   0.3108  0.3108  0.3108 0.3108
#> se0    0.0778  0.0778  0.0778 0.0778
#> ATE   -0.0719 -0.0284 -0.0284 0.0127
#> se     0.0890  0.0905  0.0905 0.0993
#> p.val  0.4189  0.7535  0.7535 0.8982
#> 
#> Data type: competing risks 
#> Strategy: principal stratum strategy 
#> Estimation method: nonparametric estimation 
#> Observations: 137 (including 99 treated and 38 control)
#> Maximum follow-up time: 2640 
#> P-value of the average treatment effect: NA 
#> -----------------------------------------------------------------------
#> The estimated cumulative incidences and treatment effects at quartiles:
#>           660    1320    1980   2640
#> CIF1   0.3444  0.4072  0.4072 0.4664
#> se1    0.0578  0.0606  0.0606 0.0832
#> CIF0   0.4618  0.4618  0.4618 0.4618
#> se0    0.1030  0.1030  0.1030 0.1030
#> ATE   -0.1174 -0.0546 -0.0546 0.0047
#> se     0.1181  0.1195  0.1195 0.1325
#> p.val  0.3205  0.6479  0.6479 0.9718
#> 
for (st in c('composite','natural','removed','whileon','principal')){
 fit = scr.tteICE(A, bmt$t1, bmt$d1, bmt$t2, bmt$d2, st)
 print(fit, digits=2)
}
#> Data type: semicompeting risks 
#> Strategy: composite variable strategy 
#> Estimation method: nonparametric estimation 
#> Observations: 137 (including 99 treated and 38 control)
#> Maximum follow-up time: 2640 
#> P-value of the average treatment effect: 0.59 
#> -----------------------------------------------------------------------
#> The estimated cumulative incidences and treatment effects at quartiles:
#>         660  1320  1980  2640
#> CIF1   0.53  0.59  0.59  0.63
#> se1    0.05  0.05  0.05  0.06
#> CIF0   0.61  0.64  0.64  0.64
#> se0    0.08  0.08  0.08  0.08
#> ATE   -0.08 -0.05 -0.05 -0.01
#> se     0.09  0.09  0.09  0.10
#> p.val  0.42  0.58  0.58  0.94
#> 
#> Data type: semicompeting risks 
#> Strategy: hypothetical strategy (controlling the hazard of ICEs) 
#> Estimation method: nonparametric estimation 
#> Observations: 137 (including 99 treated and 38 control)
#> Maximum follow-up time: 2204 
#> P-value of the average treatment effect: 0.5 
#> -----------------------------------------------------------------------
#> The estimated cumulative incidences and treatment effects at quartiles:
#>         551  1102  1653  2204
#> CIF1   0.46  0.55  0.57  0.62
#> se1    0.06  0.06  0.06  0.07
#> CIF0   0.55  0.61  0.64  0.64
#> se0    0.08  0.08  0.08  0.08
#> ATE   -0.09 -0.06 -0.07 -0.01
#> se     0.08  0.08  0.08  0.08
#> p.val  0.27  0.46  0.34  0.86
#> 
#> Data type: semicompeting risks 
#> Strategy: hypothetical strategy (removing ICEs) 
#> Estimation method: nonparametric estimation 
#> Observations: 137 (including 99 treated and 38 control)
#> Maximum follow-up time: 2640 
#> P-value of the average treatment effect: 0.64 
#> -----------------------------------------------------------------------
#> The estimated cumulative incidences and treatment effects at quartiles:
#>         660  1320  1980 2640
#> CIF1   0.28  0.35  0.35 0.42
#> se1    0.05  0.06  0.06 0.08
#> CIF0   0.40  0.40  0.40 0.40
#> se0    0.09  0.09  0.09 0.09
#> ATE   -0.12 -0.06 -0.06 0.01
#> se     0.11  0.11  0.11 0.13
#> p.val  0.24  0.61  0.61 0.92
#> 
#> Data type: semicompeting risks 
#> Strategy: while on treatment strategy 
#> Estimation method: nonparametric estimation 
#> Observations: 137 (including 99 treated and 38 control)
#> Maximum follow-up time: 2640 
#> P-value of the average treatment effect: 0.71 
#> -----------------------------------------------------------------------
#> The estimated cumulative incidences and treatment effects at quartiles:
#>         660  1320  1980 2640
#> CIF1   0.24  0.28  0.28 0.32
#> se1    0.04  0.05  0.05 0.06
#> CIF0   0.31  0.31  0.31 0.31
#> se0    0.08  0.08  0.08 0.08
#> ATE   -0.07 -0.03 -0.03 0.01
#> se     0.09  0.09  0.09 0.10
#> p.val  0.42  0.75  0.75 0.90
#> 
#> Data type: semicompeting risks 
#> Strategy: principal stratum strategy 
#> Estimation method: nonparametric estimation 
#> Observations: 137 (including 99 treated and 38 control)
#> Maximum follow-up time: 2640 
#> P-value of the average treatment effect: NA 
#> -----------------------------------------------------------------------
#> The estimated cumulative incidences and treatment effects at quartiles:
#>         660  1320  1980 2640
#> CIF1   0.34  0.41  0.41 0.47
#> se1    0.06  0.06  0.06 0.08
#> CIF0   0.46  0.46  0.46 0.46
#> se0    0.10  0.10  0.10 0.10
#> ATE   -0.12 -0.05 -0.05 0.00
#> se     0.12  0.12  0.12 0.13
#> p.val  0.32  0.65  0.65 0.97
#> 

fit2 = tteICE(Surv.ice(t2, d4)~A, data=bmt,
strategy="composite", cov.formula=~z1+z3+z5, method='eff')
print(fit2)
#> Data type: competing risks 
#> Strategy: composite variable strategy 
#> Estimation method: semiparametrically efficient estimation 
#> Observations: 137 (including 99 treated and 38 control)
#> Maximum follow-up time: 2640 
#> P-value of the average treatment effect: 0.1366 
#> -----------------------------------------------------------------------
#> The estimated cumulative incidences and treatment effects at quartiles:
#>           660    1320    1980    2640
#> CIF1   0.5246  0.5836  0.5836  0.6347
#> se1    0.0514  0.0511  0.0511  0.0587
#> CIF0   0.6784  0.7010  0.7010  0.7010
#> se0    0.0662  0.0651  0.0651  0.0651
#> ATE   -0.1538 -0.1174 -0.1174 -0.0663
#> se     0.0838  0.0828  0.0828  0.0877
#> p.val  0.0667  0.1560  0.1560  0.4494
#> 
fit3 = tteICE(Surv.ice(t1, d1, t2, d2)~A, data=bmt,
strategy="composite", cov.formula=~z1+z3+z5, method='eff')
print(fit3)
#> Data type: semicompeting risks 
#> Strategy: composite variable strategy 
#> Estimation method: semiparametrically efficient estimation 
#> Observations: 137 (including 99 treated and 38 control)
#> Maximum follow-up time: 2640 
#> P-value of the average treatment effect: 0.1366 
#> -----------------------------------------------------------------------
#> The estimated cumulative incidences and treatment effects at quartiles:
#>           660    1320    1980    2640
#> CIF1   0.5246  0.5836  0.5836  0.6347
#> se1    0.0514  0.0511  0.0511  0.0587
#> CIF0   0.6784  0.7010  0.7010  0.7010
#> se0    0.0662  0.0651  0.0651  0.0651
#> ATE   -0.1538 -0.1174 -0.1174 -0.0663
#> se     0.0838  0.0828  0.0828  0.0877
#> p.val  0.0667  0.1560  0.1560  0.4494
#> 
```
