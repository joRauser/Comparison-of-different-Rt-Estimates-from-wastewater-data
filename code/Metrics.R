library(Metrics)
# Compare to Gold-Standard:


mse <- function(goldSt, predictor) {
  mean((goldSt - predictor)^2)
}


rt_hospitalizations <- rt_hospitalizations[-1,] # bring rt's to same length
mae(rt_hospitalizations$mean_rt, rt_cohortPos$mean_rt)
# .1698287
mse(rt_hospitalizations$mean_rt, rt_cohortPos$mean_rt)
# .05875327



# get percentage of values which are both higher 1 and lower 1:
tendency_check <- function(rt_1, rt_2){
  # Zusammenführen der Datensätze auf Basis der Spalte "Date"
  merged_data <- merge(rt_1, rt_1, by = "date", suffixes = c("_1", "_2"))
  
  # Überprüfung, ob beide Rt-Werte über 1 oder unter 1 liegen
  both_above_or_below_one <- with(merged_data, (mean_rt_1 > 1 & mean_rt_2 > 1) | (mean_rt_1 < 1 & mean_rt_2 < 1))
  
  # Berechnung des Prozentsatzes der übereinstimmenden Punkte
  percentage <- mean(both_above_or_below_one) * 100
  
  # Ausgabe des Ergebnisses
  return(percentage)
}

tendency_check(rt_hospitalizations, rt_cohortPos)
