# 'SciViews::R' Dialect for Data Processing and Visualization <a href='https://www.sciviews.org/SciViews'><img src='man/figures/logo.png' align='right' height='139'/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/SciViews/SciViews/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SciViews/SciViews/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/SciViews/SciViews/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SciViews/SciViews?branch=main) [![CRAN Status](https://www.r-pkg.org/badges/version/SciViews)](https://cran.r-project.org/package=SciViews) [![r-universe status](https://sciviews.r-universe.dev/badges/SciViews)](https://sciviews.r-universe.dev/SciViews) [![License](https://img.shields.io/badge/license-GPL-blue.svg)](https://www.gnu.org/licenses/gpl-2.0.html) [![Life cycle stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

{SciViews} mainly provides the `SciViews::R` dialect through the function of the same name. It loads a series of tidyverse and SciViews packages in order to supplement base R with functions to implement that dialect. See, for instance the books [Science des Données Biologiques I](http://biodatascience-course.sciviews.org/sdd-umons/) and [Science des Données Biologiques II](http://biodatascience-course.sciviews.org/sdd-umons2/) (in French) for extensive examples of the use of `SciViews::R`.

## Installation

{SciViews} is available from CRAN, but it is an old version. You should install it from the [SciViews R-Universe](https://sciviews.r-universe.dev). To install this package and its dependencies, run the following command in R:

``` r
install.packages('SciViews', repos = c('https://sciviews.r-universe.dev',
  'https://cloud.r-project.org'))
```

An older version of {SciViews} can be installed from [CRAN](http://cran.r-project.org):

``` r
install.packages("SciViews")
```

You can also install the latest development version. Make sure you have the {remotes} R package installed:

``` r
install.packages("remotes")
```

Use `install_github()` to install the {SciViews} package from GitHub (source from **main** branch will be recompiled on your machine):

``` r
remotes::install_github("SciViews/SciViews")
```

R should install all required dependencies automatically, and then it should compile and install {SciViews}.

## Further explore {SciViews}

You can get further help about this package this way: Make the {SciViews} package and all the other packages required by the `SciViews::R` dialect available in your R session:

``` r
SciViews::R()
```

Get help about this package:

``` r
library(help = "SciViews")
help("SciViews-package")
vignette("SciViews") # None is installed with install_github()
```

For further instructions, please, refer to these help pages at <https://www.sciviews.org/SciViews/>.

## Code of Conduct

Please note that the {SciViews} package is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## Note to developers

This package used to be developed on R-Forge in the past. However, the latest [R-Forge version](https://r-forge.r-project.org/projects/sciviews/) was moved to this Github repository on 2018-01-05 (SVN version 569). **Please, do not use R-Forge anymore for SciViews development, use this Github repository instead.**
