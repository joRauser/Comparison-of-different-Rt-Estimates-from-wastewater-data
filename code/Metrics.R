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