# Graphical results of tteICE

This function plots the estimated potential cumulative incidence
functions or treatment effect curve with pointwise confidence intervals.

## Usage

``` r
# S3 method for class 'tteICE'
plot(
  x,
  type = c("ate", "inc")[1],
  decrease = FALSE,
  conf.int = 0.95,
  xlab = "Time",
  xlim = NULL,
  ylim = NULL,
  plot.configs = list(),
  ...
)
```

## Arguments

- x:

  A fitted object returned by the function `surv.tteICE` or
  `scr.tteICE`.

- type:

  Which plot to create: `ate` indicates to plot the estimated treatment
  effect; `inc` indicates to plot the estimated cumulative incidence
  function.

- decrease:

  A logical variable indicating the type of curve to display. If
  `decrease = FALSE` (default), the function displays the cumulative
  incidence functions (CIFs) or their differences. If `decrease = TRUE`,
  the function instead displays the survival functions or their
  differences.

- conf.int:

  Confidence level for the interval. If `conf.int = NULL`, no confidence
  interval is provided.

- xlab:

  Label for x-axis.

- xlim:

  A numeric vector of length 2 giving the limits of the x-axis. If
  `xlim=NULL` (default), the range is determined automatically from the
  data.

- ylim:

  A numeric vector of length 2 giving the limits of the y-axis. If
  `ylim=NULL` (default), the range is determined automatically by the
  type of plot

- plot.configs:

  A named `list` of additional plot configurations. See details in
  functions
  [`plot_ate`](https://mephas.github.io/tteICE/reference/plot_ate.md)
  and
  [`plot_inc`](https://mephas.github.io/tteICE/reference/plot_inc.md)

- ...:

  Other augments in function
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html) or
  function [`curve`](https://rdrr.io/r/graphics/curve.html)

## Value

Plot the results from a tteICE object

## See also

[`plot_ate`](https://mephas.github.io/tteICE/reference/plot_ate.md),
[`plot_inc`](https://mephas.github.io/tteICE/reference/plot_inc.md)

## Examples

``` r
## load data
data(bmt)
bmt = transform(bmt, d4=d2+d3)
A = as.numeric(bmt$group>1)
## plot cumulative incidence functions with p-values
for (st in c('composite','natural','removed','whileon','principal')){
 fit = surv.tteICE(A, bmt$t2, bmt$d4, st)
 plot(fit, type="inc", decrease=TRUE, ylim=c(0,1),
      plot.configs=list(show.p.value=TRUE))
}





## plot treatment effects for semicompeting risk data
for (st in c('composite','natural','removed','whileon','principal')){
 fit = scr.tteICE(A, bmt$t1, bmt$d1, bmt$t2, bmt$d2, st)
 plot(fit, type="ate", ylim=c(-1,1), xlab="time",
      plot.configs=list(col="red"))
}





```
