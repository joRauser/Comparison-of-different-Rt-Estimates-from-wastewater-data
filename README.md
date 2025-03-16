# Comparison-of-different-Rt-Estimates-from-wastewater-data

This repository contains the code, raw-data, and analysis for the project "Comparison of Different $R_t$ Estimates from Wastewater Data." The study focuses on evaluating the effectiveness of wastewater data in estimating the reproduction number ($R_t$) and comparing it with other data sources, such as hospitalization data and cohort studies.

# Outline of the code-folder
DataPrep - All Data Preprosession including cleaning, alignment and interpolation of the data when necessary.  
Rt-EpiEstim - Function and Application of the EpiEstim-method to calculate the Rt from different data-sources, including direct substitution with wastewater data.  
Rt-ExpPercChangeRate - Function and Application of the Exponentiated Percent Change Rate estimator of the Rt for wastewater data.   
Plot_Rt - Functions used, to plot the different estimates.   
Metrics - Functions of Metrics used to evaluate the Rt-Estimators. These are the MSE, MAE, the EM1 and also a function to quickly calculate all 3 of them.  
Timeshift_detection - Function to detect the value and sign of the timeshift.   
Timeshift_applocation - Application of the timeshift on the cleaned data and step-by step re-calculating of the Rt. 
