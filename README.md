
<!-- README.md is generated from README.Rmd. Please edit that file -->

# predRupdate <a href="https://glenmartin31.github.io/predRupdate/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/GlenMartin31/predRupdate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GlenMartin31/predRupdate/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/GlenMartin31/predRupdate/branch/master/graph/badge.svg)](https://app.codecov.io/gh/GlenMartin31/predRupdate?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/predRupdate)](https://CRAN.R-project.org/package=predRupdate)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/grand-total/predRupdate)](https://cran.r-project.org/package=predRupdate)
<!-- badges: end -->

The goal of predRupdate is to provide a suite of functions for
validating a existing (i.e. previously developed) prediction/ prognostic
model, and for applying model updating methods to said model, according
to an available dataset.

## Installation

The package can be installed from CRAN as follows:

``` r
install.packages("predRupdate")
```

## Development version

You can install the development version of predRupdate from
[GitHub](https://github.com/) with::

``` r
# install.packages("devtools")
devtools::install_github("GlenMartin31/predRupdate")
```

## Example

One main use of this package is to externally validate an existing
(previously developed) prediction model. This can be achieved with the
following code:

``` r
# create a data.frame of the model coefficients, with columns being variables
coefs_table <- data.frame("Intercept" = -3.4,
                          "SexM" = 0.306,
                          "Smoking_Status" = 0.628,
                          "Diabetes" = 0.499,
                          "Creatinine" = 0.538)

#pass this into pred_input_info()
Existing_Logistic_Model <- pred_input_info(model_type = "logistic",
                                           model_info = coefs_table)
summary(Existing_Logistic_Model)

#validate this model against an available dataset
pred_validate(x = Existing_Logistic_Model,
              new_data = SYNPM$ValidationData,
              binary_outcome = "Y")
```

## Getting help

If you encounter a bug, please file an issue with a minimal reproducible
example on [GitHub](https://github.com/GlenMartin31/predRupdate).
