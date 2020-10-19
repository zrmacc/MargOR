Zachary McCaw <br>
Updated: 2020-10-18

## Overview

Given stratified event-count data for two arms, this package calculates the marginal odds ratio of [Tian et al. (2009)](https://doi.org/10.1093/biostatistics/kxn034), and constructs the asymptotic and bootstrap confidence intervals. In contrast to the Mantel-Haenszel approach for combining odds ratios across strata, the present approach is to first calculate the marginal events in each arm by taking a stratum-size weighted average of the per-stratum event rates, then form the odds ratio. 

## Installation


```r
devtools::install_github(repo = 'zrmacc/MCC')
```

## Example

Consider the following 28-day unadjusted mortality data from a recent [COVID-19 clinical trial](https://www.nejm.org/doi/full/10.1056/NEJMoa2021436), stratified by respiratory support at ranomization. The marginal odds ratio, comparing dexamethasone with usual care, is 0.861. 


```r
# Event counts.
y0 <- c(283, 682, 145)
n0 <- c(683, 2604, 1034)
y1 <- c(95, 298, 89)
n1 <- c(324, 1279, 501)

# Marginal Odds Ratio
library(MargOR)
or_analysis <- MargOR(
  y0 = y0,
  n0 = n0,
  y1 = y1,
  n1 = n1,
  alpha = 0.05,
  reps = 2e3
)
show(or_analysis)
```

```
## Rates:
##   Arm    N      Rate
## 1   0 4321 0.2567286
## 2   1 2104 0.2292085
## 
## 
## CIs:
##       Method        OR         SE     Lower     Upper          P
## 1 Asymptotic 0.8609283 0.06247420 0.7617086 0.9730723 0.01653468
## 2  Bootstrap 0.8609283 0.05353668 0.7648350 0.9720767 0.01799100
```

The results of a Mantel-Haenszel test are similar:


```r
library(abind)
# Formatting data.
z0 <- n0 - y0
z1 <- n1 - y1

ctrl <- rbind(z0, y0)
trt <- rbind(z1, y1)

data <- abind(ctrl, trt, along = 0)
mantelhaen.test(data)
```

```
## 
## 	Mantel-Haenszel chi-squared test with continuity correction
## 
## data:  data
## Mantel-Haenszel X-squared = 5.7032, df = 1, p-value = 0.01693
## alternative hypothesis: true common odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.7594595 0.9721183
## sample estimates:
## common odds ratio 
##         0.8592349
```


