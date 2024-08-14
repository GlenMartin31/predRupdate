# output of pred_validate is as expected - single models

    Code
      summary(val_results)
    Output
      Calibration Measures 
      --------------------------------- 
                              Estimate Lower 95% Confidence Interval
      Observed:Expected Ratio   1.9006                        1.8368
      Calibration Intercept     0.7323                        0.6921
      Calibration Slope         0.6484                        0.5576
                              Upper 95% Confidence Interval
      Observed:Expected Ratio                        1.9666
      Calibration Intercept                          0.7726
      Calibration Slope                              0.7392
      
       Also examine the calibration plot, if produced. 
      
      Discrimination Measures 
      --------------------------------- 
          Estimate Lower 95% Confidence Interval Upper 95% Confidence Interval
      AUC   0.5814                        0.5702                        0.5927
      
      
      Overall Performance Measures 
      --------------------------------- 
      Cox-Snell R-squared: -0.0481
      Nagelkerke R-squared: -0.0863
      Brier Score (CI): 0.1249 (0.1219, 0.1279)
      
       Also examine the distribution plot of predicted risks. 

