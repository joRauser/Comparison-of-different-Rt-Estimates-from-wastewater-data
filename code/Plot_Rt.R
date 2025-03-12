library(ggplot2)
library(dplyr)
library(patchwork)


# Set datasets for Plotting here
datasets <- list(
  "Hospitalizations" = hospitalizations_aligned,
  "Wastewater" = wastewater_toPMMoV,
  "CohortStudy" = cohortPosTest
)
datasets <- list(
  "Hospitalizations_Weekly" = hospitalizations_aligned,
  "Hospitalizations_Daily" = hosp_conv_aligned
)


# Parameter für die rt_unknown_si -> Parameter dann für alle Estimates gleich! 
params <- list(
  weekly = "Yes",
)



# Ergebnisse für alle Datensätze speichern
results <- lapply(names(datasets), function(name) {
  rt_data <- rt_unknown_si(
    dataframe = datasets[[name]],
    weekly = params$weekly,
  )
  rt_data <- rt_data %>%
    mutate(Source = name) 
  return(rt_data)
})

# Combine results
combined_results <- bind_rows(results)

# create plot
plot_combined <- ggplot(combined_results, aes(x = date, color = Source)) +
  # plot the means
  geom_line(aes(y = mean_rt), linewidth = 1) +
  # plot confidence intervals
  geom_ribbon(aes(ymin = ll_95_rt, ymax = ul_95_rt, fill = Source), alpha = 0.5, color = NA) +
  
  theme_bw() +
  scale_color_brewer(palette = "Accent") + 
  scale_fill_brewer(palette = "Accent") + 
  
  labs(
    title = "Comparison of Rt-Estimates",
    subtitle = "Means and Confidence Intervals for different data used",
    x = "Date",
    y = "Rt",
    color = "Datenquelle", 
    fill = "Datenquelle") +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5)
  )

# Show Plot
print(plot_combined)
