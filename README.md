
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rwiki

Download wikibook in R

<!-- badges: start -->

[![Travis Build
Status](https://travis-ci.org/kongdd/rwiki.svg?branch=master)](https://travis-ci.org/kongdd/rwiki)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/kongdd/rwiki?branch=master&svg=true)](https://ci.appveyor.com/project/kongdd/rwiki)
[![codecov](https://codecov.io/gh/kongdd/rwiki/branch/master/graph/badge.svg)](https://codecov.io/gh/kongdd/rwiki)
[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![CRAN](http://www.r-pkg.org/badges/version/rwiki)](https://cran.r-project.org/package=rwiki)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/rwiki)](https://CRAN.R-project.org/package=rwiki)
<!-- badges: end -->

## Installation

You can install the released version of rwiki from
[github](https://github.com/kongdd/rwiki) with:

``` r
devtools::install_github("kongdd/rwiki")
```

## Example

This example if for my wikibook
[统计学基础](https://en.wikipedia.org/wiki/User:Kongdd/Books/%E7%BB%9F%E8%AE%A1%E5%AD%A6%E5%9F%BA%E7%A1%80).

``` r
library(rwiki)
# Use https_proxy, if you can't access wiki. If needn't, comment this line
Sys.setenv(https_proxy="http://127.0.0.1:1080") 

url  <- "https://en.wikipedia.org/wiki/User:Kongdd/Books/%E7%BB%9F%E8%AE%A1%E5%AD%A6%E5%9F%BA%E7%A1%80"
info <- wikibook(url, lang = "zh")
#> Retrieve other language: zh

outdir <- info$origin$title[1]

## basic example code
plyr::m_ply(info$origin[, .(url = url, outfile = title)],
             write_pdf, outdir = outdir)
#> [ok] file already exist: https://en.wikipedia.org/wiki/Expected_value ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Covariance ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Covariance_matrix ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Variance ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Degrees_of_freedom_(statistics) ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Lagrange_multiplier ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Normal_distribution ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Multivariate_normal_distribution ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Bernoulli_distribution ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Binomial_distribution ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Multinomial_distribution ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Chi-squared_distribution ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Student's_t-distribution ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/F-distribution ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Gamma_distribution ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Chi-squared_test ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Student's_t-test ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Ordinary_least_squares ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Proofs_involving_ordinary_least_squares ... 
#> [ok] file already exist: https://en.wikipedia.org/wiki/Total_least_squares ...

## book in other language
plyr::m_ply(info$lang[, .(url = url, outfile = title)],
             write_pdf, outdir = outdir)
#> [ok] file already exist: https://zh.wikipedia.org/wiki/期望值 ... 
#> process https://zh.wikipedia.org/wiki/%E5%8D%8F%E6%96%B9%E5%B7%AE | 1.2 协方差.pdf ... 
#> 

  |=================================================================| 100%
#> [ok] file already exist: https://zh.wikipedia.org/wiki/协方差矩阵 ... 
#> [ok] file already exist: https://zh.wikipedia.org/wiki/方差 ... 
#> [ok] file already exist: https://zh.wikipedia.org/wiki/自由度_(统计学) ... 
#> [ok] file already exist: https://zh.wikipedia.org/wiki/拉格朗日乘数 ... 
#> [ok] file already exist: https://zh.wikipedia.org/wiki/正态分布 ... 
#> [ok] file already exist: https://zh.wikipedia.org/wiki/多元正态分布 ... 
#> [ok] file already exist: https://zh.wikipedia.org/wiki/伯努利分布 ... 
#> [ok] file already exist: https://zh.wikipedia.org/wiki/二項分佈 ... 
#> [ok] file already exist: https://zh.wikipedia.org/wiki/卡方分佈 ... 
#> [ok] file already exist: https://zh.wikipedia.org/wiki/学生t-分布 ... 
#> [ok] file already exist: https://zh.wikipedia.org/wiki/F-分布 ... 
#> [ok] file already exist: https://zh.wikipedia.org/wiki/伽玛分布 ... 
#> [ok] file already exist: https://zh.wikipedia.org/wiki/卡方检验 ... 
#> [ok] file already exist: https://zh.wikipedia.org/wiki/學生t檢驗 ...
```
