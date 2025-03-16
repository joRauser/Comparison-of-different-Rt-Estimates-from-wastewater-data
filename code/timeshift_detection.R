mse <- function(goldSt, predictor) {
  mean((goldSt - predictor)^2)
}

# calculate mse for a given time-shifts in data 
neg_log_likelihood <- function(shift, dat1, dat2) {
  shift <- round(shift) # shift only in full days
  if (shift < 0) {
    # neg shift
    shifted_mse <- mse(dat1, dat2[(-shift + 1):length(dat2)])
  } else if (shift > 0) {
    # pos shift
    shifted_mse <- mse(dat1, dat2[1:(length(dat2) - shift)])
  } else {
    # no shift
    shifted_mse <- mse(dat1, dat2)
  }
  return(shifted_mse)
}

# Optimization to get most likely time-shift (data-driven)
find_time_shift_mle <- function(dat1, dat2, max_shift = 15) {
  result <- optim(
    par = 0, 
    fn = neg_log_likelihood,
    dat1 = dat1,
    dat2 = dat2,
    method = "Brent",   # honestly not sure if there are better methods suitable here
    lower = -max_shift,
    upper = max_shift
  )
  return(list(best_shift = round(result$par), best_mse = result$value))
}


# Application of "timeshift-detector"

### Interpolated and daily
result <- find_time_shift_mle((merge(rt_hospitalizations, rt_wastewater_toPMMoV, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hospitalizations, rt_wastewater_toPMMoV, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

result <- find_time_shift_mle((merge(rt_hospitalizations, rt_expo_1, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hospitalizations, rt_expo_1, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

result <- find_time_shift_mle((merge(rt_hospitalizations, rt_expo_4, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hospitalizations, rt_expo_4, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

result <- find_time_shift_mle((merge(rt_hospitalizations, rt_cohortPos, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hospitalizations, rt_cohortPos, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)



### Interpolated and weekly
result <- find_time_shift_mle((merge(rt_hosp_weekly, rt_wastewater_toPMMoV, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hosp_weekly, rt_wastewater_toPMMoV, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

result <- find_time_shift_mle((merge(rt_hosp_weekly, rt_expo_1, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hosp_weekly, rt_expo_1, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

result <- find_time_shift_mle((merge(rt_hosp_weekly, rt_expo_4, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hosp_weekly, rt_expo_4, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

result <- find_time_shift_mle((merge(rt_hosp_weekly, rt_cohortPos, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hosp_weekly, rt_cohortPos, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)



### Deconvoluted and daily
result <- find_time_shift_mle((merge(rt_hosp_conv, rt_wastewater_toPMMoV, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hosp_conv, rt_wastewater_toPMMoV, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

result <- find_time_shift_mle((merge(rt_hosp_conv, rt_expo_1, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hosp_conv, rt_expo_1, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

result <- find_time_shift_mle((merge(rt_hosp_conv, rt_expo_4, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hosp_conv, rt_expo_4, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

result <- find_time_shift_mle((merge(rt_hosp_conv, rt_cohortPos, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hosp_conv, rt_cohortPos, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)




### Deconvoluted and weekly
result <- find_time_shift_mle((merge(rt_hosp_conv_weekly, rt_wastewater_toPMMoV, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hosp_conv_weekly, rt_wastewater_toPMMoV, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

result <- find_time_shift_mle((merge(rt_hosp_conv_weekly, rt_expo_1, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hosp_conv_weekly, rt_expo_1, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

result <- find_time_shift_mle((merge(rt_hosp_conv_weekly, rt_expo_4, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hosp_conv_weekly, rt_expo_4, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

result <- find_time_shift_mle((merge(rt_hosp_conv_weekly, rt_cohortPos, by = "date")%>%drop_na())$mean_rt.x, 
                              (merge(rt_hosp_conv_weekly, rt_cohortPos, by = "date")%>%drop_na())$mean_rt.y)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)
