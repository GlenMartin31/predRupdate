# output of pred_validate is as expected - single models

    Code
      summary(val_results)
    Output
      Calibration Measures 
      --------------------------------- 
                               Estimate Std. Err Lower 95% Confidence Interval
      Calibration-in-the-large   0.7323   0.0206                        0.6921
      Calibration Slope          0.6484   0.0463                        0.5576
                               Upper 95% Confidence Interval
      Calibration-in-the-large                        0.7726
      Calibration Slope                               0.7392
      
       Also examine the calibration plot, if produced. 
      
      Discrimination Measures 
      --------------------------------- 
          Estimate Std. Err Lower 95% Confidence Interval
      AUC   0.5814   0.0057                        0.5702
          Upper 95% Confidence Interval
      AUC                        0.5927
      
      
      Overall Performance Measures 
      --------------------------------- 
      Cox-Snell R-squared: -0.0481
      Nagelkerke R-squared: -0.0863
      Brier Score: 0.1249
      
       Also examine the histogram of predicted risks. 

