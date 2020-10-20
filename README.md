Zachary McCaw <br>
Updated: 2020-10-19

## Overview

Given stratified event-count data for two arms, this package calculates summary statistics comparing two arms with respect to the marginal event rate. Marginal event rates are calculated as the stratum-sized weighted-average of the per-stratum event rates, then compared via the risk difference, risk ratio, and odds ratio.

## Installation


```r
devtools::install_github(repo = 'zrmacc/MargRates')
```

## Example

Consider the following 28-day unadjusted mortality data from a recent [COVID-19 clinical trial](https://www.nejm.org/doi/full/10.1056/NEJMoa2021436), stratified by respiratory support at randomization


```r
# Event counts.
y0 <- c(283, 682, 145)
n0 <- c(683, 2604, 1034)
y1 <- c(95, 298, 89)
n1 <- c(324, 1279, 501)

# Marginal Odds Ratio
library(MargRates)
set.seed(2013)
out <- CompMargRates(
  y0 = y0,
  n0 = n0,
  y1 = y1,
  n1 = n1,
  alpha = 0.05,
  reps = 2e3
)
show(out)
```

```
## Marginal Rates:
##   Arm    N      Rate
## 1   0 4321 0.2567286
## 2   1 2104 0.2292085
## 
## 
## Risk Difference:
##       Method     Stat        Est         SE       Lower        Upper          P
## 1 Asymptotic RiskDiff -0.0275201 0.01131947 -0.04970586 -0.005334347 0.01504816
## 4  Bootstrap RiskDiff -0.0275201 0.01123516 -0.04957270 -0.005407053 0.01799100
## 
## 
## Risk Ratio:
##       Method      Stat       Est         SE     Lower     Upper          P
## 2 Asymptotic RiskRatio 0.8928047 0.04252159 0.8132355 0.9801592 0.01727794
## 5  Bootstrap RiskRatio 0.8928047 0.04233066 0.8123457 0.9782870 0.01799100
## 
## 
## Odds Ratio:
##       Method      Stat       Est         SE     Lower     Upper          P
## 3 Asymptotic OddsRatio 0.8609283 0.05378581 0.7617086 0.9730723 0.01653468
## 6  Bootstrap OddsRatio 0.8609283 0.05358627 0.7608769 0.9712936 0.01799100
```
