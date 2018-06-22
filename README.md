
<!-- README.md is generated from README.Rmd. Please edit that file -->
PointFore
=========

The goal of PointFore is to estimate specification models for the state-dependent level of an optimal quantile/expectile forecast.

Wald Tests and the test of overidentifying restrictions are implemented. Ploting of the estimated specification model is possible.

Based on "Interpretation of Point Forecasts" by Patrick Schmidt, Matthias Katzfuss, and Tilmann Gneiting, 2018.

Installation
------------

You can install PointFore from github with:

``` r
# install.packages("devtools")
devtools::install_github("Schmidtpk/PointFore")
```

Example
-------

This is a basic example which shows you how to evaluate which quantile is forecsted by the Greenbook GDP forecats:

``` r
library(PointFore)
#> 
#> Attaching package: 'PointFore'
#> The following object is masked from 'package:stats':
#> 
#>     lag

res <- estimate.functional(Y=GDP[,1],X=GDP[,2])
#> Drop  1 case(s) because of chosen instruments

summary(res)
#> $call
#> estimate.functional(Y = GDP[, 1], X = GDP[, 2])
#> 
#> $coefficients
#>           Estimate Std. Error  t value     Pr(>|t|)
#> Theta[1] 0.5924402 0.04006288 14.78776 1.757143e-49
#> 
#> $Jtest
#> 
#>  ##  J-Test: degrees of freedom is 2  ## 
#> 
#>                 J-test    P-value 
#> Test E(g)=0:    6.812312  0.033168

plot(res)
```

![](README-example-1.png)

On average the forecast is over-optimistic with a forecasted quantile of 0.59. The J-test rejects optimality for this model.

In the next step, we apply a more general model, where the forecasted quantile depends on the current forecast via a logistic model.

``` r
res <- estimate.functional(Y=GDP[,1],X=GDP[,2],
                           model=logistic,
                           theta0=c(0,0),
                           stateVariable = GDP[,2])
#> Drop  1 case(s) because of chosen instruments

summary(res)
#> $call
#> estimate.functional(model = logistic, theta0 = c(0, 0), Y = GDP[, 
#>     1], X = GDP[, 2], stateVariable = GDP[, 2])
#> 
#> $coefficients
#>            Estimate Std. Error   t value   Pr(>|t|)
#> Theta[1] -0.2255394 0.27990264 -0.805778 0.42037086
#> Theta[2]  0.1811752 0.07329482  2.471869 0.01344086
#> 
#> $Jtest
#> 
#>  ##  J-Test: degrees of freedom is 1  ## 
#> 
#>                 J-test   P-value
#> Test E(g)=0:    1.37263  0.24136

plot(res)
```

![](README-example%20with%20state-dependence-1.png)

We see that the forecast is overly optimistic in times of high growth. For this model we cannot reject optimality with a p-value of 0.24 in the J-Test of overidentifying restrictions.
