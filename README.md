
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

res <- estimate.functional(Y=GDP$observation,X=GDP$forecast)
#> Drop  1 case(s) because of chosen instruments
#> Choose parameter theta0 automatically.

summary(res)
#> $call
#> estimate.functional(Y = GDP$observation, X = GDP$forecast)
#> 
#> $coefficients
#>           Estimate Std. Error  t value     Pr(>|t|)
#> Theta[1] 0.5980637 0.04429534 13.50173 1.527435e-41
#> 
#> $Jtest
#> 
#>  ##  J-Test: degrees of freedom is 2  ## 
#> 
#>                 J-test    P-value 
#> Test E(g)=0:    5.507506  0.063688

plot(res)
```

![](README-example-1.png)

On average the forecast is over-optimistic with a forecasted quantile of 0.6. The J-test rejects optimality for this model.

In the next step, we apply a more general model, where the forecasted quantile depends on the current forecast via a linear probit model.

``` r
res <- estimate.functional(Y=GDP$observation,X=GDP$forecast,
                           model=probit_linear,
                           stateVariable = GDP$forecast)
#> Drop  1 case(s) because of chosen instruments
#> Choose parameter theta0 automatically.



summary(res)
#> $call
#> estimate.functional(model = probit_linear, Y = GDP$observation, 
#>     X = GDP$forecast, stateVariable = GDP$forecast)
#> 
#> $coefficients
#>            Estimate Std. Error    t value   Pr(>|t|)
#> Theta[1] -0.1125011 0.16807744 -0.6693408 0.50327812
#> Theta[2]  0.1132529 0.04437854  2.5519745 0.01071144
#> 
#> $Jtest
#> 
#>  ##  J-Test: degrees of freedom is 1  ## 
#> 
#>                 J-test   P-value
#> Test E(g)=0:    1.38747  0.23883
plot(res)
```

![](README-example%20with%20state-dependence-1.png)

We see that the forecast is overly optimistic in times of high growth. For this model we cannot reject optimality with a p-value of 0.239 in the J-Test of overidentifying restrictions.
