# SciViews To Do list

- `SciViews::R`: no fancy characters?

- `na.omit()` -> `na_omit()` and co + `complete_cases()`.

- Plot functions issue warnings for unknown arguments

- `file_head()`

- `attrNames()` or `attr_names()` like `slotNames()`

- `unscale()` to revert the effect of `scale()`?

- `is.wholenumber()`

- other` xxx.` functions for those using `na.rm = FALSE` like `mean()`, `median()`, `sd()`, `var()`, `quantile()`, `fivenum()`, ...

- Refine `panel.xxx()` functions to avoid warning in case we provide non-par arguments to `pairs()` and design a better mechanism to define default (colors, line type and weight, etc.).

- For PCA, look at ade4 (dudi.pca => c("pca", "dudi"), ellipse, area), FactoMineR (PCA), rrcov (Pca), pcaMethods (PcaRes), psych, nFactors, vegan, rda = special case of pca => c("rda", "cca")), pcaPP, chemometrics (cross-validation), BiodiversityR (broken-stick = PCAsignificance + variable importance = ordiequilibriumCircle) and labdsv packages => how to sort all this and make something coherent with our pcomp object and methods? See also zoonek znd http://www.statmethods.net/advstats/factor.html.

- For correspondence analysis, see 'ca' package.

- Analyze this: http://obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/

- Translation of this package.
