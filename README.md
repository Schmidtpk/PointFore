
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

This is a basic example which shows you how to evaluate which quantile is forecasted by the Greenbook GDP forecats:

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
#>          Estimate Std. Error  t value     Pr(>|t|)
#> Theta[1] 0.600349  0.0391736 15.32534 5.177899e-53
#> 
#> $Jtest
#> 
#>  ##  J-Test: degrees of freedom is 2  ## 
#> 
#>                 J-test    P-value 
#> Test E(g)=0:    6.356318  0.041662

#plot(res)
```

On average the forecast is over-optimistic with a forecasted quantile of 0.6. The J-test rejects optimality for this model.

In the next step, we apply a more general model, where the forecasted quantile depends on the current forecast via a logistic model.

``` r
res <- estimate.functional(Y=GDP$observation,X=GDP$forecast,
                           model=logistic,
                           theta0=c(0,0),
                           stateVariable = GDP$forecast)
#> Drop  1 case(s) because of chosen instruments

summary(res)
#> $call
#> estimate.functional(model = logistic, theta0 = c(0, 0), Y = GDP$observation, 
#>     X = GDP$forecast, stateVariable = GDP$forecast)
#> 
#> $coefficients
#>            Estimate Std. Error    t value   Pr(>|t|)
#> Theta[1] -0.1674806 0.27118217 -0.6175945 0.53684265
#> Theta[2]  0.1770935 0.07208819  2.4566229 0.01402498
#> 
#> $Jtest
#> 
#>  ##  J-Test: degrees of freedom is 1  ## 
#> 
#>                 J-test   P-value
#> Test E(g)=0:    1.12177  0.28954

#plot(res,
#     pdf=FALSE,
#     conf.levels = c(0.6,0.9))
```

We see that the forecast is overly optimistic in times of high growth. For this model we cannot reject optimality with a p-value of 0.29 in the J-Test of overidentifying restrictions.
