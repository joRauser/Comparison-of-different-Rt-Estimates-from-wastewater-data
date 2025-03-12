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





