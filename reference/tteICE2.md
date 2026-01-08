# Using formula to fit the CIF for time-to-event with intercurrent events

This function estimates the potential cumulative incidence function for
time-to event data under ICH E9 (R1) to address intercurrent events. The
input data should be of a competing risks structure.

## Usage

``` r
tteICE2(
  formula,
  data,
  strategy = "composite",
  method = "np",
  cov.formula = NULL,
  weights = NULL,
  subset = NULL,
  na.rm = FALSE,
  nboot = 0,
  seed = 0
)
```

## Arguments

- formula:

  An object of class "formula" (or one that can be coerced to that
  class): a symbolic description of the model to be fitted. The details
  of model specification are given under ‘Details’.

- data:

  An optional data frame, list or environment (or object coercible by
  as.data.frame to a data frame) containing the variables in the model.

- strategy:

  Strategy to address intercurrent events, `"treatment"` indicating
  treatment policy strategy, `"composite"` indicating composite variable
  strategy, `"natural"` indicating hypothetical strategy (Scenario I,
  controlling the hazard of intercurrent events), `"removed"` indicating
  hypothetical strategy (Scenario II, removing intercurrent events),
  `"whileon"` indicating while on treatment strategy, and `"principal"`
  indicating principal stratum strategy.

- method:

  Estimation method, `"np"` indicating nonparametric estimation, `"np"`
  indicating inverse treatment probability weighting, `"eff"` indicating
  semiparametrically efficient estimation based on efficient influence
  functions.

- cov.formula:

  Baseline covariates in an object of "formula". The details of model
  specification are given under ‘Details’.

- weights:

  Weight for each subject.

- subset:

  Subset, either numerical or logical.

- na.rm:

  Whether to remove missing values.

- nboot:

  Number of resamplings in the boostrapping method. If `nboot` is 0 or
  1, then asymptotic standard error based on the explicit form is
  calculated instead of bootstrapping.

- seed:

  Seed for bootstrapping.

## Value

A list including the fitted object and input variables.

## Details

- Background:

  Intercurrent events refer to the events occurring after treatment
  initiation of clinical trials that affect either the interpretation of
  or the existence of the measurements associated with the clinical
  question of interest. The International Conference on Harmonization
  (ICH) E9 (R1) addendum proposed five strategies to address
  intercurrent events, namely, treatment policy strategy, composite
  variable strategy, while on treatment strategy, hypothetical strategy,
  and principal stratum strategy. To answer a specific scientific
  question, a strategy with a particular estimand is chosen before the
  study design.

- Model:

  We adopt the potential outcomes framework that defines a causal
  estimand as the contrast between functionals of potential outcomes.
  Consider a randomized controlled trial with \\n\\ individuals randomly
  assigned to one of two treatment conditions, denoted by \\w\\, where
  \\w = 1\\ represents the active treatment (a test drug) and \\w = 0\\
  represents the control (placebo). Assume that all patients adhere to
  their treatment assignments and do not discontinue treatment.
  Associated with individual \\i = 1, ..., n\\ are two potential
  time-to-event primary outcomes \\T_i(1)\\ and \\T_i(0)\\, if any,
  which represent the time durations from treatment initiation to the
  primary outcome event under two treatment assignments respectively.
  Let \\R_i(1)\\ and \\R_i(0)\\ denote the occurrence time of potential
  intercurrent events, if any, under the two treatment assignments,
  respectively. Intercurrent events are considered as absent if no
  post-treatment intercurrent events occur until the end of study.

- Estimand:

  We adopt the potential cumulative incidences under both treatment
  assignments as the target estimands. Potential cumulative incidences
  describe the probability of time-to-event outcomes occurring at each
  time point. We define the treatment effect as the contrast of two
  potential cumulative incidences. Cumulative incidences are model-free
  and collapsible, enjoying causal interpretations.

- Formula specifications in the function:

  A typical model has the form `Surv(time, status, type = "mstate")~ A`,
  where `status=0,1,2` (1 for the primary event, 2 for the intercurrent
  event, and 0 for censoring), `A` is the treatment indicator (1 for
  treatment and 0 for control). An alternative model has the form
  `cbind(Surv(time1, status1), Surv(time2, status2))~ A`, where
  `Surv(time1, status1)` defines the time and status of primary
  (terminal) event, and `Surv(time2, status2)` defines the time and
  status of intercurrent event. Baseline covariates can be added by the
  model in the form `~ x1+x2`

## References

Deng, Y., Han, S., & Zhou, X. H. (2025). Inference for Cumulative
Incidences and Treatment Effects in Randomized Controlled Trials With
Time-to-Event Outcomes Under ICH E9 (R1). *Statistics in Medicine*.
[doi:10.1002/sim.70091](https://doi.org/10.1002/sim.70091)

## See also

[`surv.boot`](https://mephas.github.io/tteICE/reference/surv.boot.md),
[`scr.tteICE`](https://mephas.github.io/tteICE/reference/scr.tteICE.md)

## Examples

``` r
## load data
data(bmt)
bmt = transform(bmt, d4=d2+d3)
A = as.numeric(bmt$group>1)
X = as.matrix(bmt[,c('z1','z3','z5')])
bmt$A = A
library(survival)
## Composite variable strategy,
## nonparametric estimation without covariates

fit1 = tteICE2(Surv(t2, d4, type = "mstate")~A, data=bmt, 
strategy="composite", method='eff')
fit2 = tteICE2(Surv(t2, d4, type = "mstate")~A, data=bmt, 
strategy="composite", cov.formula=~z1+z3+z5, method='eff')
fit20 = tteICE2(Surv(t2, d4)~A, data=bmt, 
strategy="composite", cov.formula=~z1+z3+z5, method='eff')
#> Warning: Invalid status value, converted to NA

fit3 = tteICE2(cbind(Surv(t1, d1),Surv(t2,d2))~A, data=bmt, 
strategy="composite", cov.formula=~z1+z3+z5, method='eff')

fit3 = tteICE2(cbind(Surv(t2, d2), Surv(t1,d1))~A, data=bmt, 
strategy="composite", cov.formula=~z1+z3+z5, method='eff') ## same results??

fit4 = tteICE2(Surv(t2, d2)~A, data=bmt, 
strategy="composite", cov.formula=~z1+z3+z5, method='eff')
fit5 = tteICE2(cbind(t1, d1,t2, d2)~A, data=bmt, 
strategy="composite", cov.formula=~z1+z3+z5, method='eff')  ## also work

```
