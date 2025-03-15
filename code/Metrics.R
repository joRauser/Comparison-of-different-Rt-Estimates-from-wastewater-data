library(Metrics)
# Compare to Gold-Standard:


mse <- function(goldSt, predictor) {
  mean((goldSt - predictor)^2)
}

# get percentage of values which are both higher 1 and lower 1:
tendency_check <- function(rt_1, rt_2){
  # Zusammenführen der Datensätze auf Basis der Spalte "Date"
  merged_data <- merge(rt_1, rt_2, by = "date", suffixes = c("_1", "_2"))
  
  # Überprüfung, ob beide Rt-Werte über 1 oder unter 1 liegen
  both_above_or_below_one <- with(merged_data, (mean_rt_1 > 1 & mean_rt_2 > 1) | (mean_rt_1 < 1 & mean_rt_2 < 1))
  
  # Berechnung des Prozentsatzes der übereinstimmenden Punkte
  percentage <- mean(both_above_or_below_one) * 100
  
  # Ausgabe des Ergebnisses
  return(percentage)
}



### BEFORE TIMESHIFT-ADJUSTMENT
rt_hospitalizations <- rt_hospitalizations[-1,] # bring rt's to same length
mae(rt_hospitalizations[-1,]$mean_rt, rt_cohortPos$mean_rt)
# .1926251
mse(rt_hospitalizations[-1,]$mean_rt, rt_cohortPos$mean_rt)
# .07734484

mae(rt_hospitalizations$mean_rt, rt_wastewater_toPMMoV$mean_rt)
# .2381259
mse(rt_hospitalizations$mean_rt, rt_wastewater_toPMMoV$mean_rt)
# .1043689

tendency_check(rt_hospitalizations, rt_cohortPos)
# 86,11%
tendency_check(rt_hospitalizations, rt_wastewater_toPMMoV)
# 67,57%


combined_dataset <- merge(rt_hosp_conv, rt_expo, by = "date") %>%
  drop_na()
mse(combined_dataset$mean_rt.x, combined_dataset$mean_rt.y)
mae(combined_dataset$mean_rt.x, combined_dataset$mean_rt.y)



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
