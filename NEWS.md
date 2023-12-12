# predRupdate 0.1.1

* Changed the naming of the calibration metrics in `summary.predvalidate` to rename "calibration-in-the-large"" for logistic models as being the "calibration intercept". 
* `predvalidate()` now calculates observed:expected ratio for validating logistic models, along with calibration intercept
* `pred_validate()` now stores the calibration plots (ggplot) as part of the output (previous versions of the package just printed the plot without outputting the plot object). This facilitates users saving or further changing the style of the plot.
* Added a vignette to show how to validate an existing CPM that includes a spline term

# predRupdate 0.1.0

* Initial release of predRupdate.

# predRupdate 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
