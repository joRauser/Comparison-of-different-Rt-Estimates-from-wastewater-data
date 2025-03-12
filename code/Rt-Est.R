# Test all the necessary Models with the data
library(EpiEstim)
library(zoo)

### RT FUNCTION WITH UNKNOWN SI
rt_unknown_si <- function(dataframe, weekly){
  
  if(weekly == "Yes"){
    
    # create weekly time windows from df
    Time <- nrow(dataframe)
    t_start <- seq(2, Time-6, by = 7) # starting at 2 as conditional on the past observations
    t_end <- t_start + 6 # adding 6 to get 7-day windows as bounds included in window
    
    # change column-names
    colnames(dataframe) <- c("dates", "I")
    
    # config function for serial interval -> Reasonable by Brockhaus et al
    serial_interval <- make_config(
      incid = dataframe$I,
      method = c("uncertain_si"),
     # mean_si = 4,
      mean_si = 5,
     # std_si = 1,
      std_si = 1,
    #  std_mean_si = 1,
      std_mean_si = 3,
     # min_mean_si = 1,
     # max_mean_si = 7,
      min_mean_si = 3,
      max_mean_si = 7,
      std_std_si = 1,
     # min_std_si = .1,
     # max_std_si = 1.9,
       min_std_si = 1,
       max_std_si = 5,
      t_start = t_start,
      t_end = t_end
    )
    
    # calculate Rt
    r0_test<- estimate_R(
      incid = dataframe$I,
      method = c("uncertain_si"),
      config = serial_interval
    )
    
    # Table of dates used
    r_number_dates <- r0_test$dates
    
    # Table of R_0 estimates
    r_number_estim <- r0_test$R %>% 
      # Add date for end of the (weekly) estimation
      mutate(dates = r_number_dates[t_end]) %>% 
      # Extract mean and SD of R estimate
      select(dates, `Mean(R)`, `Std(R)`, `Quantile.0.05(R)`, `Quantile.0.95(R)`)
    
    # add rownames to the dataframe
    rownames <- row.names(dataframe)
    dataframe <- cbind(dataframe, rownames)
    dataframe$rownames <- as.integer(dataframe$rownames)
    r_number_estim <- r_number_estim %>% rename(rownames = dates)
    
    # extract data for cleaner ggplots
    plot_df <- full_join(dataframe, r_number_estim, by = "rownames")
    
    # rename fields
    plot_df$se_rt <- plot_df$`Std(R)` / sqrt(nrow(plot_df))
    plot_df <- plot_df %>%
      rename(date = dates,
             mean_rt = `Mean(R)`,
             ll_95_rt = `Quantile.0.05(R)`,
             ul_95_rt = `Quantile.0.95(R)`
      )%>%
      select(date,  mean_rt, se_rt, ll_95_rt, ul_95_rt)
    plot_df$week <- floor_date(plot_df$date, unit = "week")
    plot_df <- plot_df %>% filter(!is.na(mean_rt))
    return(plot_df)
    
  } else if(weekly == "No"){
    
    # change column-names 
    colnames(dataframe) <- c("dates", "I")
    
    # config function for serial interval
    serial_interval <- make_config(
      incid = dataframe$I,
      method = c("uncertain_si"),
      mean_si = 5,
      std_si = 1,
      std_mean_si = 3,
      min_mean_si = 3,
      max_mean_si = 7,
      std_std_si = 1,
      min_std_si = 1,
      max_std_si = 5
    )
    
    # calculate Rt
    r0_test<- estimate_R(
      incid = dataframe$I,
      method = c("uncertain_si"),
      config = serial_interval
    )
    
    # Table of dates used
    r_number_dates <- r0_test$dates
    
    # Table of R_0 estimates
    r_number_estim <- r0_test$R %>% 
      # Add date for end of the (weekly) estimation
      mutate(dates = r_number_dates[t_end]) %>% 
      # Extract mean and SD of R estimate
      select(dates, `Mean(R)`, `Std(R)`, `Quantile.0.05(R)`, `Quantile.0.95(R)`)
    
    # add rownames to the dataframe
    rownames <- row.names(dataframe)
    dataframe <- cbind(dataframe, rownames)
    dataframe$rownames <- as.integer(dataframe$rownames)
    r_number_estim <- r_number_estim %>% rename(rownames = dates)
    
    # extract data for cleaner ggplots
    plot_df <- full_join(dataframe, r_number_estim, by = "rownames")
    
    # rename fields
    plot_df$se_rt <- plot_df$`Std(R)` / sqrt(nrow(plot_df))
    plot_df <- plot_df %>%
      rename(date = dates,
             mean_rt = `Mean(R)`,
             ll_95_rt = `Quantile.0.05(R)`,
             ul_95_rt = `Quantile.0.95(R)`
      )%>%
      select(date,  mean_rt, se_rt, ll_95_rt, ul_95_rt)
    return(plot_df)
  }
}

### Gold Standard:
rt_hospitalizations <- rt_unknown_si(hospitalizations_aligned, "Yes")
rt_hosp_conv <- rt_unknown_si(hosp_conv_aligned, "No")


### CohortStudy
rt_cohortstudy <- rt_unknown_si(cohort_aligned, "Yes")
rt_cohortInf <- rt_unknown_si(cohortInfRate_aligned, "Yes")
rt_cohortPos <- rt_unknown_si(cohortPosTest_aligned, "Yes")


### Wastewater:
## Direct substitution:
# Incidence = Virus to PMMoV:
rt_wastewater_toPMMoV <- rt_unknown_si(wastewater_toPMMoV, "Yes", 4, 3, 1, .9)


