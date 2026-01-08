# Plot the estimated treatment effect

This function plots the estimated treatment effect, defined as the
difference in potential cumulative incidences under treated and control
groups, along with pointwise confidence intervals.

## Usage

``` r
plot_ate(
  fit,
  decrease = FALSE,
  conf.int = 0.95,
  xlab = "Time",
  ylim = c(-1, 1),
  xlim = NULL,
  plot.configs = list(ylab = NULL, main = NULL, lty = 1, lwd = 2, col = "black",
    add.null.line = TRUE, null.line.lty = 2, ci.lty = 5, ci.lwd = 1.5, ci.col =
    "darkgrey"),
  ...
)
```

## Arguments

- fit:

  A fitted object returned by the function `surv.tteICE` or
  `scr.tteICE`.

- decrease:

  A logical variable indicating the type of curve difference to display.
  If `decrease = FALSE` (default), the function displays the difference
  in cumulative incidence functions (CIFs). If `decrease = TRUE`, the
  function instead displays the difference in survival functions.

- conf.int:

  Confidence level for the interval. If `conf.int = NULL`, no confidence
  interval is provided.

- xlab:

  Label for x-axis.

- ylim:

  A numeric vector of length 2 giving the limits of the y-axis. Defaults
  to `ylim=c(-1, 1)`.

- xlim:

  A numeric vector of length 2 giving the limits of the x-axis. If
  `xlim=NULL` (default), the range is determined automatically from the
  data.

- plot.configs:

  A named `list` of additional plot configurations. Common entries
  include:

  - `ylab`: character, label for the y-axis (default: `ylab=NULL`, use
    the default label).

  - `main`: character, title for the plot (default: `main=NULL`, use the
    default label).

  - `lty`: line type for effect curve (default: 1).

  - `lwd`: line width for effect curve (default: 2).

  - `col`: line color for effect curve (default: "black").

  - `add.null.line`: logical, whether to draw a horizontal line at 0
    (default: TRUE).

  - `null.line.lty`: line type for horizontal line at 0 (default: 2).

  - `ci.lty`: line type for confidence interval curves (default: 5).

  - `ci.lwd`: line width for confidence interval curves (default: 1.5).

  - `ci.col`: line color for confidence interval curves (default:
    "darkgrey").

- ...:

  Additional graphical arguments passed to function
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html) or
  function [`curve`](https://rdrr.io/r/graphics/curve.html)

## Value

Plot the average treatment effect (ATE) results from a tteICE object

## See also

[`plot.default`](https://rdrr.io/r/graphics/plot.default.html),
[`points`](https://rdrr.io/r/graphics/points.html),
[`curve`](https://rdrr.io/r/graphics/curve.html),
[`plot.tteICE`](https://mephas.github.io/tteICE/reference/plot.tteICE.md)

## Examples

``` r
## Load data and fit the model
data(bmt)
bmt = transform(bmt, d4=d2+d3)
A = as.numeric(bmt$group>1)
## Model with competing risk data
fit1 = surv.tteICE(A, bmt$t2, bmt$d4, 'composite')
## Plot asymptotic confidence intervals based on explicit formulas
plot_ate(fit1, ylim=c(-0.4,0.4))

# \donttest{
## Plot bootstrap confidence intervals
fit1 = surv.tteICE(A, bmt$t2, bmt$d4, 'composite', nboot=200)
plot_ate(fit1, ylim=c(-0.4,0.4))

# }
## Model with semicompeting risk data
fit2 = scr.tteICE(A, bmt$t1, bmt$d1, bmt$t2, bmt$d2, "composite")
## Plot asymptotic confidence intervals based on explicit formulas
plot_ate(fit2, ylim=c(-0.4,0.4),
         plot.configs=list(add.null.line=FALSE))

## Plot bootstrap confidence intervals
fit2 = scr.tteICE(A, bmt$t1, bmt$d1, bmt$t2, bmt$d2, "composite", nboot=200)
plot_ate(fit2, ylim=c(-0.4,0.4),
         plot.configs=list(add.null.line=FALSE, lty=2, main=""))

```
