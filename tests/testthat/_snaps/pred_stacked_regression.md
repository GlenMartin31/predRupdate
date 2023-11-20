# pred_stacked_regression has expected output

    Code
      summary(SR)
    Output
      Existing models aggregated using stacked regression
      The model stacked regression weights are as follows: 
      (Intercept)         LP1         LP2         LP3 
       0.02781941  0.46448799  0.15626108  0.16282116 
      
      Updated Model Coefficients 
      ================================= 
        Intercept         Age      SexM Smoking_Status  Diabetes Creatinine
      1 -2.675134 0.005345728 0.1589948      0.5233706 0.2543348  0.4554044
      
      Model Functional Form 
      ================================= 
      Age + SexM + Smoking_Status + Diabetes + Creatinine

