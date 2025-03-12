mse <- function(goldSt, predictor) {
  mean((goldSt - predictor)^2)
}

# calculate mse for a given time-shifts in data 
neg_log_likelihood <- function(shift, dat1, dat2) {
  shift <- round(shift) # shift only in full days
  if (shift < 0) {
    # neg shift: Kürze am Anfang von dat1 und Ende von dat2
    shifted_mse <- mse(dat1[1:(length(dat1) + shift)], dat2[(-shift + 1):length(dat2)])
  } else if (shift > 0) {
    # pos shift: Kürze am Ende von dat1 und Anfang von dat2
    shifted_mse <- mse(dat1[(shift + 1):length(dat1)], dat2[1:(length(dat2) - shift)])
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

# Anwendung der Funktion
result <- find_time_shift_mle(rt_hospitalizations$mean_rt, rt_cohortPos$mean_rt)

# Ergebnisse ausgeben
cat("Best timeshift:", result$best_shift, "days, with MSE of", result$best_mse)

