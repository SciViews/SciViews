
# SciViews

<!-- badges: start -->
[![Linux Build Status](https://travis-ci.com/SciViews/SciViews.svg )](https://travis-ci.com/SciViews/SciViews)
[![Win Build Status](https://ci.appveyor.com/api/projects/status/github/SciViews/SciViews?branch=master&svg=true)](https://ci.appveyor.com/project/phgrosjean/SciViews)
[![Coverage Status](https://img.shields.io/codecov/c/github/SciViews/SciViews/master.svg)
](https://codecov.io/github/SciViews/SciViews?branch=master)
[![CRAN Status](https://www.r-pkg.org/badges/version/SciViews)](https://cran.r-project.org/package=SciViews)
[![License](https://img.shields.io/badge/license-GPL-blue.svg)](https://www.gnu.org/licenses/gpl-2.0.html)
[![Life
cycle stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

> 'SciViews' mainly provides the `SciViews::R` dialect through the function of the same name. It loads a series of tidyverse and Sciviews packages in order to supplement base R with functions to implement that dialect. See, for instance the books [Science des Données Biologiques I](http://biodatascience-course.sciviews.org/sdd-umons/) and [Science des Données Biologiques II](http://biodatascience-course.sciviews.org/sdd-umons2/) (in French) for extensive examples of the use of `SciViews::R`.

## Installation

The latest stable version of 'SciViews' can simply be installed from [CRAN](http://cran.r-project.org):

```r
install.packages("SciViews")
```

You can also install the latest developement version. Make sure you have the 'devtools' R package installed:

```r
install.packages("devtools")
```

Use `install_github()` to install the 'SciViews' package from Github (source from **master** branch will be recompiled on your machine):

```r
devtools::install_github("SciViews/SciViews")
```

R should install all required dependencies automatically, and then it should compile and install 'SciViews'.

Latest devel version of 'SciViews' (source + Windows binaires for the latest stable version of R at the time of compilation) is also available from [appveyor](https://ci.appveyor.com/project/phgrosjean/SciViews/build/artifacts).

## Further explore 'SciViews'

You can get further help about this package this way: Make the 'SciViews' package and all the other packages required by the `SciViews::R` dialect available in your R session:

```r
SciViews::R()
```

Get help about this package:

```r
library(help = "SciViews")
help("SciViews-package")
vignette("SciViews") # None is installed with install_github()
```

For further instructions, please, refer to these help pages at https://www.sciviews.org/SciViews/.

## Code of Conduct

Please note that the 'SciViews' project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## Note to developers

This package used to be developed on R-Forge in the past. However, the latest [R-Forge version](https://r-forge.r-project.org/projects/sciviews/) was moved to this Github repository on 2018-01-05 (SVN version 569). **Please, do not use R-Forge anymore for SciViews development, use this Github repository instead.**
