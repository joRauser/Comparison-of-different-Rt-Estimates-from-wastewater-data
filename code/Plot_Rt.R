library(tidyverse)
library(patchwork)

print_plot <- function(datasets){
  
  results <- lapply(names(datasets), function(name) {
    rt_data <- datasets[[name]] %>%
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
}


EpiEstim_Comparison <- print_plot(list(
  "Hospitalizations" = rt_hospitalizations,
  "Wastewater" = rt_wastewater_toPMMoV,
  "CohortStudy" = rt_cohortPos
))
hospitalizations_comparison <- print_plot(list(
  "Hospitalizations_Weekly" = rt_hospitalizations,
  "Hospitalizations_Daily" = rt_hosp_conv
))

wwModel_comparison <- print_plot(list(
  "WW_EpiEstim" = rt_wastewater_toPMMoV, 
  "WW_EPCR" = rt_expo
))

Model_comparison <- print_plot(list(
  "WW_EpiEstim" = rt_wastewater_toPMMoV, 
  "WW_EPCR" = rt_expo,
  "Hospitalizations" = rt_hosp_conv
))

dailyValuesEPCR <- print_plot(list(
  "Hosp_Daily" = rt_hosp_conv, 
  "WW_EPCR_Daily" = rt_expo
))