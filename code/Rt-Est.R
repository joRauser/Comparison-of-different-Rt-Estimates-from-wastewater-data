# Test all the necessary Models with the data
library(EpiEstim)
library(zoo)
###### From D.HILL-Code
## Comparison value - Rt from case data using EpiEstim method

### RT FUNCTION WITH UNKNOWN SI
rt_function_unknown_si <- function(dataframe, weekly, mean_SI, meanSpan_SI, std_SI, stdSpan_SI){
  
  if(weekly == "Yes"){
    
    # create weekly time windows from df
    Time <- nrow(dataframe)
    t_start <- seq(2, Time-6, by = 7) # starting at 2 as conditional on the past observations
    t_end <- t_start + 6 # adding 6 to get 7-day windows as bounds included in window
    
    # change name of case data column
 #   colnames(dataframe)[2] <- "case_data"
  #  colnames(dataframe)[1] <- "Date"
    
    # fill in missing dates
  #  dataframe <- dataframe %>%
  #    complete(Date = seq.Date(min(Date), max(Date), by = "day")
  #    ) %>%
  #    mutate(I = na.approx(case_data) # interpolate missing values
  #    ) %>% 
  #    dplyr::select(Date, I)
    
    # column names change
    colnames(dataframe) <- c("dates", "I")
    
    # config function for serial interval
    serial_interval <- make_config(
      incid = dataframe$I,
      method = c("uncertain_si"),
     # mean_si = 4,
      mean_si = mean_SI,
     # std_si = 1,
      std_si = std_SI,
      std_mean_si = 1,
     # min_mean_si = 1,
     # max_mean_si = 7,
      min_mean_si = mean_SI - meanSpan_SI,
      max_mean_si = mean_SI + meanSpan_SI,
      std_std_si = 1,
     # min_std_si = .1,
     # max_std_si = 1.9,
       min_std_si = std_SI - stdSpan_SI,
       max_std_si = std_SI + stdSpan_SI,
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
    
    p <- ggplot(plot_df, aes(x = date, y = mean_rt)) +
      geom_line(color = "blue") +
      geom_ribbon(aes(ymin = ll_95_rt, ymax = ul_95_rt), alpha = 0.2, color = "lightblue") +
      theme_minimal() +
      labs(title = "Reproduktionszahl Rt", x = "Datum", y = "Rt")
    print(p)
    return(plot_df)
    
  } else if(weekly == "No"){
    
    # change name of case data column
    colnames(dataframe)[2] <- "case_data"
    colnames(dataframe)[1] <- "Date"
    
    # fill in missing dates
  #  dataframe <- dataframe %>%
  #    complete(Date = seq.Date(min(Date), max(Date), by = "day")
  #    ) %>%
  #    mutate(I = na.approx(case_data) # interpolate missing values
  #    ) %>% 
  #    dplyr::select(Date, I)
    
    # column names change
    colnames(dataframe) <- c("dates", "I")
    
    # config function for serial interval
    serial_interval <- make_config(
      incid = dataframe$I,
      method = c("uncertain_si"),
      mean_si = 4,
    #  mean_si = mean_SI, #kann später auch bei min_mean etc mit mean_SI - 2 ersetzt werden
      std_si = 1,
      std_mean_si = 1,
      min_mean_si = 1,
    #  max_mean_si = 10,
      max_mean_si = 7,
      std_std_si = 1,
     # min_std_si = 1,
     # max_std_si = 5
      min_std_si = .1,
      max_std_si = 1.9
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
    
    p <- ggplot(plot_df, aes(x = date, y = mean_rt)) +
      geom_line(color = "blue") +
      geom_ribbon(aes(ymin = ll_95_rt, ymax = ul_95_rt), alpha = 0.2, color = "lightblue") +
      theme_minimal() +
      labs(title = "Reproduktionszahl Rt", x = "Datum", y = "Rt")
    
    print(p)
    
    return(plot_df)
  }
  
}

### Gold Standard:
rt_hospitalizations <- rt_function_unknown_si(hospitalizations_aligned, "Yes", 4, 3, 1, .9)
#rt_hospitalizations <- rt_function_unknown_si(hospitalizations_aligned, "No", 4)

### CohortStudy
# Is okayish, but very interesting because here, just the valid tests are observed
rt_cohortstudy <- rt_function_unknown_si(cohort_aligned, "Yes", 4, 3, 1, .9)
# Not good at all :D (only the peaks are recognizalbe with a lot of imagination)
# Also the KI's are very large 
rt_cohortInf <- rt_function_unknown_si(cohortInfRate_aligned, "Yes", 4, 3, 1, .9)
# Pretty good
rt_cohortPos <- rt_function_unknown_si(cohortPosTest_aligned, "Yes", 4, 3, 1, .9)

# -> As expected, the number of positiveTests works best, to estimate Rt


### Wastewater:
## Direct substitution:
# Incidence = Virus to PMMoV:
rt_wastewater_toPMMoV <- rt_function_unknown_si(wastewater_toPMMoV, "Yes", 4, 3, 1, .9)

ggplot()+
  geom_line(data = dat_curve, aes(x = date, y = mean_rt))

