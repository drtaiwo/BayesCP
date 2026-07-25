# ExactBayesCP developer notes

This source tree was prepared from BayesCP 0.2.1 and renamed to ExactBayesCP 0.2.2.

Before release, run in R from the package root:

```r
devtools::document()
devtools::test()
devtools::check()
```

Then build and test the source archive in a fresh R session:

```r
pkg <- devtools::build()
install.packages(pkg, repos = NULL, type = "source")
library(ExactBayesCP)
packageVersion("ExactBayesCP")
citation("ExactBayesCP")
```

The `bayescp_*` function names and `bayescp_fit` S3 class are intentionally retained for API continuity.
