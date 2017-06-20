[![Travis-CI Build Status](https://travis-ci.org/gvegayon/numint.svg?branch=master)](https://travis-ci.org/gvegayon/numint)

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/gvegayon/numint?branch=master&svg=true)](https://ci.appveyor.com/project/gvegayon/numint)

[![Coverage Status](https://img.shields.io/codecov/c/github/gvegayon/numint/master.svg)](https://codecov.io/github/gvegayon/numint?branch=master)

<!-- README.md is generated from README.Rmd. Please edit that file -->
numint
======

The goal of numint is to ...

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(numint)

set.seed(1231)

ans <- numint(dbeta, a = 0, b = .5, shape1 = 2, shape2 = 20)
ans
#> MONTE CARLO INTEGRATION
#> N: 100
#> Volume: 0.5000
#> 1.4385 +- 1.4996

plot(ans)
```

![](README-example-1.png)
