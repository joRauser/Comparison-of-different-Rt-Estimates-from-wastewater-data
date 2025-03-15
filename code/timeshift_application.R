
# Wastewater- shift Applied
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
