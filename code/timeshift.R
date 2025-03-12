mse <- function(goldSt, predictor) {
  mean((goldSt - predictor)^2)
}

# calculate mse for a given time-shifts in data 
neg_log_likelihood <- function(shift, dat1, dat2) {
  shift <- round(shift) # shift only in full days
  if (shift < 0) {
    # neg shift: Kürze am Anfang von dat1 und Ende von dat2
    shifted_mse <- mse(dat1, dat2[(-shift + 1):length(dat2)])
  } else if (shift > 0) {
    # pos shift: Kürze am Ende von dat1 und Anfang von dat2
    shifted_mse <- mse(dat1, dat2[1:(length(dat2) - shift)])
  } else {
    # Keine Verschiebung
    shifted_mse <- mse(dat1, dat2)
  }
  return(shifted_mse)
}

# Optimization to get most likely time-shift (data-driven)
find_time_shift_mle <- function(dat1, dat2, max_shift = 10) {
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

# Shift Dataset:
# Shift the timestamps of the cleaned data first, align it and use rt_function again. Then prove by calculating the mse
shiftData <- function(data, shift){
  shifteddata <- data
  shifteddata$Date <- shifteddata$Date + shift
  return(shifteddata)
}

# Give Results with: 
result <- find_time_shift_mle(rt_hospitalizations$mean_rt, data2$mean_rt)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)


# Cohort:
result <- find_time_shift_mle(rt_hospitalizations[-1,]$mean_rt, rt_cohortPos$mean_rt)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

cohort_shifted <- shiftData(cohortPosTest, result$best_shift)


# Wastewater
result <- find_time_shift_mle(rt_hospitalizations$mean_rt, rt_wastewater_toPMMoV$mean_rt)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

wastewater_shifted <- shiftData(wastewater_cleaned%>%
                                  select(Date, genCopiesToPMMoV), result$best_shift)


# Estimate Rt on Timeshift:
cohort_shifted <- alignTimespan(cohort_shifted)
wastewater_shifted <- alignTimespan(wastewater_shifted)

rt_cohort_shifted <- rt_unknown_si(cohort_shifted, "Yes")
rt_wastewater_shifted <- rt_unknown_si(wastewater_shifted, "Yes")

# Compare Values:
result <- find_time_shift_mle(rt_hospitalizations$mean_rt[-1], rt_cohort_shifted$mean_rt)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

cohort_shifted <- shiftData(cohort_shifted, result$best_shift)


result <- find_time_shift_mle(rt_hospitalizations$mean_rt, rt_wastewater_shifted$mean_rt)
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

wastewater_shifted <- shiftData(wastewater_shifted, result$best_shift)

