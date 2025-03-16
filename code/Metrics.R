library(Metrics)

mse <- function(goldSt, predictor) {
  mean((goldSt - predictor)^2)
}

# get percentage of values which are both higher 1 and lower 1:
tendency_check <- function(rt_1, rt_2){
  merged_data <- merge(rt_1, rt_2, by = "date", suffixes = c("_1", "_2")) %>% drop_na()
  
  # check, if both values of Rt are below or above 1
  both_above_or_below_one <- with(merged_data, (mean_rt_1 > 1 & mean_rt_2 > 1) | (mean_rt_1 < 1 & mean_rt_2 < 1))
  
  # Calculating the percentage of matching points
  percentage <- mean(both_above_or_below_one) * 100
  
  return(percentage)
}

evalMetrics <- function(rt1, rt2){
  combined_dataset <- merge(rt1, rt2, by = "date") %>%
    drop_na()
  mse <- mse(combined_dataset$mean_rt.x, combined_dataset$mean_rt.y)
  mae <- mae(combined_dataset$mean_rt.x, combined_dataset$mean_rt.y)
  em1 <- tendency_check(rt1, rt2)
  cat("MSE: ", mse , "MAE: ", mae, "EM1: ", em1, "%")
}


### WITHOUT TIMESHIFT-ADJUSTMENT
evalMetrics(rt_hosp_conv, rt_wastewater_toPMMoV)
evalMetrics(rt_hosp_conv, rt_expo_1)
evalMetrics(rt_hosp_conv, rt_expo_4)
evalMetrics(rt_hosp_conv, rt_cohortPos)

### WITH TIMESHIFT-ADJUSTMENT
evalMetrics(rt_hosp_conv, rt_wastewater_toPMMoV_SHIFTED)
evalMetrics(rt_hosp_conv, rt_expo_1_SHIFTED)
evalMetrics(rt_hosp_conv, rt_expo_4_SHIFTED)
evalMetrics(rt_hosp_conv, rt_cohortPos_SHIFTED)



# Difference of rt_hospitalizations from weekly to daily measures
hosp_daily_weekly_conv <- merge(rt_hosp_conv, rt_hosp_conv_weekly, by = "date")%>%
  drop_na()
mse(hosp_daily_weekly_conv$mean_rt.x, hosp_daily_weekly_conv$mean_rt.y)

hosp_daily_weekly <- merge(rt_hospitalizations, rt_hosp_weekly, by = "date")%>%
  drop_na()
mse(hosp_daily_weekly$mean_rt.x, hosp_daily_weekly$mean_rt.y)


# Difference between Deconvoluted and Interpolated data:
hosp_conv_ip_daily <- merge(rt_hosp_conv, rt_hospitalizations, by = "date")%>%
  drop_na()
mse(hosp_conv_ip_daily$mean_rt.x, hosp_conv_ip_daily$mean_rt.y)

hosp_conv_ip_weekly <- merge(rt_hosp_conv_weekly, rt_hosp_weekly, by = "date")%>%
  drop_na()
mse(hosp_conv_ip_weekly$mean_rt.x, hosp_conv_ip_weekly$mean_rt.y)
