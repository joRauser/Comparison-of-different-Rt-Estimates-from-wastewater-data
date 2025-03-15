# Shift the timestamps of the cleaned data first (due to data availability), align it and use rt_function again. Then prove by calculating the mse
shiftData <- function(data, shift, method, change_window){
  # Apply all steps for Data-Prepping on the data, after applying the timeshift
  shifteddata <- data
  shifteddata$Date <- shifteddata$Date + shift
  if(method == "EpiEstim"){
    shifteddata <- alignTimespan(shifteddata)
    rt_shifteddata <- rt_unknown_si(shifteddata, "No")
  }else if(method == "EPCR"){
    shifteddata <- expoData(shifteddata)
    rt_shifteddata <- rt_change_rate_function(shifteddata, change_window, "No")
  }
 
  return(rt_shifteddata)
}
# Estimate shifted Rt's
rt_wastewater_toPMMoV_SHIFTED <- shiftData(wastewater_cleaned%>%
                                             select(Date, genCopiesToPMMoV),
                                           -4, "EpiEstim")
rt_expo_1_SHIFTED <- shiftData(dataset_Expo%>%
                                 select(Date, genCopiesToPMMoV),
                               -4, "EPCR", 1)
rt_cohortPos_SHIFTED <- shiftData(cohortPosTest, 6, "EpiEstim")

# Plot the Timeshifted data

# Figure 5: Rt-calculation for timeshifted datasets 
afterTimeshift <- print_plot(list(
  "Hospitalizations" = rt_hosp_conv,
  "Wastewater EpiEstim" = rt_wastewater_toPMMoV_SHIFTED,
  "EPCR_1" = rt_expo_1_SHIFTED,
  "CohortStudy" = rt_cohortPos_SHIFTED
))
afterTimeshift
