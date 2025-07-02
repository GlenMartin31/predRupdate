# predRupdate 0.2.1

# predRupdate 0.2.1

* Fixes issue with internal tests for ggplot classes, after ggplot2 transitioned to S7 (#18)

# predRupdate 0.2.0

* Added a new function, `pred_val_probs()`, that allows users to validate a vector of binary outcomes against a corresponding vector of predicted risks. Acts as a standalone way of validating predicted risks against binary outcomes outside of the usual `pred_input_info()` -> `pred_validate()` workflow for logistic models.
* Added options to `pred_validate()` to allow users to specify the confidence interval width of all performance metrics.
* Revised calculation of confidence intervals for O:E ratio, and added calculation of confidence intervals for Brier score.
* Added an option (argument 'cal_plot_n_sample') in `pred_validate()` and `pred_val_probs()` to render the calibration plot over a subset of the data, for the purposes of rendering speed. The calibration plot is always created using all data, but for rendering speed in large datasets, it can sometimes be useful to render the plot over a smaller (random) subset of observations. Final (e.g. publication-ready) plots  should always show the full plot, so a warning is created if users use this option. For similar reasons of rending speed, the rug on the x-axis of the calibration plot is now not shown by default.
* `pred_input_info()` now contains checks to ensure that variable names of model_info, and variable names of new_data passed into other functions are 'clean' (i.e., dont contain spaces, punctuation, brackets, etc.)
* Added note to vignette about specification of baseline hazard if using basehaz() from the survival package, regarding the default scaling and centering of variables.

# predRupdate 0.1.1

* Changed the naming of the calibration metrics in `summary.predvalidate` to rename "calibration-in-the-large"" for logistic models as being the "calibration intercept". 
* `predvalidate()` now calculates observed:expected ratio for validating logistic models, along with calibration intercept
* `pred_validate()` now stores the calibration plots (ggplot) as part of the output (previous versions of the package just printed the plot without outputting the plot object). This facilitates users saving or further changing the style of the plot.
* Added a vignette to show how to validate an existing CPM that includes a spline term

# predRupdate 0.1.0

* Initial release of predRupdate.

# predRupdate 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
