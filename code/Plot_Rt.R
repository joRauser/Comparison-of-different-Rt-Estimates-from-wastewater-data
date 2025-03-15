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
      #title = "Comparison of Rt-Estimates",
      #subtitle = "Means and Confidence Intervals for different data used",
      x = "",
      y = "Rt",
      color = "Data source", 
      fill = "Data source") +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 16, face = "bold", hjust = .5),
      plot.subtitle = element_text(size = 12, hjust = .5)
    )
  
  # Show Plot
  print(plot_combined)
}

# Figure 2: EpiEstim-calculation for different datasets 
EpiEstim_Comparison <- print_plot(list(
  "Hospitalizations" = rt_hospitalizations,
  "Wastewater" = rt_wastewater_toPMMoV,
  "CohortStudy" = rt_cohortPos
))

# Figure 3: Rt from different wastewater methods
wwComp <- print_plot(list(
  "WW_EPCR_1" = rt_expo_1,
  "WW_EPCR_4" = rt_expo_4,
  "WW_EpiEstim" = rt_wastewater_toPMMoV
))
EPCR <- print_plot(list(
  "WW_EPCR_1" = rt_expo_1,
  "Hospitalizations" = rt_hosp_conv
))
wastewater_Comp <- EPCR / wwComp
wastewater_Comp


hospitalizations_daily_compared <- print_plot(list(
  "Hospitalizations_WM_Daily" = rt_hospitalizations,
  "Hospitalizations_DEC_Daily" = rt_hosp_conv
))
hospitlizations_weekly_compared <- print_plot(list(
  "Hospitalizations_WM_Weekly" = rt_hosp_weekly,
  "Hospitalizations_DEC_Weekly" = rt_hosp_conv_weekly
))
hospitalization_dailyVSweekly <- hospitalizations_daily_compared / hospitlizations_weekly_compared
hospitalization_dailyVSweekly


