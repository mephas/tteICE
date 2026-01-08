# Plot the estimated cumulative incidence function (CIF)

This function plots the estimated potential cumulative incidence
function, along with pointwise confidence intervals.

## Usage

``` r
plot_inc(
  fit,
  decrease = FALSE,
  conf.int = 0.95,
  xlab = "Time",
  xlim = NULL,
  ylim = c(0, 1),
  plot.configs = list(ylab = NULL, main = NULL, cex = 0.9, lty = 1, lwd = 2, ci.lty = 5,
    ci.lwd = 1.5, legend = c("Treated", "Control"), col = c("brown", "darkcyan"),
    legend.cex = 0.9, show.p.value = TRUE),
  ...
)
```

## Arguments

- fit:

  A fitted object returned by the function `surv.tteICE` or
  `scr.tteICE`.

- decrease:

  A logical variable indicating the type of curve to display. If
  `decrease = FALSE` (default), the function displays the cumulative
  incidence functions (CIFs). If `decrease = TRUE`, the function instead
  displays the survival functions.

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

  A numeric vector of length 2 giving the limits of the y-axis. Defaults
  to `ylim=c(-1, 1)`.

- plot.configs:

  A named `list` of additional plot configurations. Common entries
  include:

  - `ylab`: character, label for the y-axis (default: `ylab=NULL`, use
    the default label).

  - `main`: character, title for the plot (default: `main=NULL`, use the
    default label).

  - `lty`: line type for the curve (default: 1).

  - `lwd`: line width for the curve (default: 2).

  - `ci.lty`: line type for confidence interval curves (default: 5).

  - `ci.lwd`: line width for confidence interval curves (default: 1.5).

  - `legend.cex`: font size for the legend (default: 0.9).

- ...:

  Additional graphical arguments passed to function
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html) or
  function [`curve`](https://rdrr.io/r/graphics/curve.html)

## Value

Plot the cumulative incidence function results from a tteICE object

## See also

[`plot.default`](https://rdrr.io/r/graphics/plot.default.html),
[`points`](https://rdrr.io/r/graphics/points.html),
[`curve`](https://rdrr.io/r/graphics/curve.html),
[`plot.tteICE`](https://mephas.github.io/tteICE/reference/plot.tteICE.md)

## Examples

``` r
## load data and fit the model
data(bmt)
bmt = transform(bmt, d4=d2+d3)
A = as.numeric(bmt$group>1)
## Model with competing risk data
fit1 = surv.tteICE(A, bmt$t2, bmt$d4, 'treatment')
## plot asymptotic confidence intervals based on explicit formulas
plot_inc(fit1, ylim=c(0,1),
         plot.configs=list(legend=c('AML','ALL'), show.p.value=FALSE) )

# \donttest{
## plot bootstrap confidence intervals
fit1 = surv.tteICE(A, bmt$t2, bmt$d4, 'treatment', nboot=200)
plot_inc(fit1, ylim=c(0,1),
         plot.configs=list(legend=c('AML','ALL')) )

# }
## Model with semicompeting risk data
fit2 = scr.tteICE(A, bmt$t1, bmt$d1, bmt$t2, bmt$d2, "composite")
## plot asymptotic confidence intervals based on explicit formulas
plot_inc(fit2, ylim=c(0,1), plot.configs=list(add.null.line=FALSE))

## plot bootstrap confidence intervals
fit2 = scr.tteICE(A, bmt$t1, bmt$d1, bmt$t2, bmt$d2, "composite", nboot=200)
plot_inc(fit2, ylim=c(0,1),
         plot.configs=list(lty=2, lwd=3,main="Title"))

```
