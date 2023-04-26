# output of pred_validate is as expected - single models

    Code
      summary(val_results)
    Output
      Calibration Measures 
      --------------------------------- 
                              Estimate Std. Err Lower 95% Confidence Interval
      Observed:Expected Ratio   0.6319   0.0113                        0.6181
      Calibration Slope         1.0757   0.0389                        0.9994
                              Upper 95% Confidence Interval
      Observed:Expected Ratio                        0.6461
      Calibration Slope                              1.1519
      
       Also examine the calibration plot, if produced. 
      
      Discrimination Measures 
      --------------------------------- 
                Estimate Std. Err Lower 95% Confidence Interval
      Harrell C   0.5869   0.0032                        0.5806
                Upper 95% Confidence Interval
      Harrell C                        0.5932
      
       Also examine the histogram of predicted risks. 

