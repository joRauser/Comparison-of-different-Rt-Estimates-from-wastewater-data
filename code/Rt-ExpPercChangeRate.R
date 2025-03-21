# rolling difference in wastewater levels
rt_change_rate_function <- function(dataframe, change_window, weekly){
  colnames(dataframe) <- c("date", "predictor")
  
  if(weekly == "Yes"){
    
    # change values to weekly means
    dataframe <- dataframe %>%
      group_by(date = floor_date(date, unit = "weeks")) %>%
      summarize(predictor = mean(predictor, na.rm = TRUE)
      ) %>%
      ungroup()
    
    dat_method_e <- dataframe %>%
      arrange(date)%>%
      mutate(
        rt_weekly_per_change = (predictor - lead(predictor, change_window))/lead(predictor, change_window), 
        rt_weekly_per_change = replace_na(rt_weekly_per_change, 0)
      ) %>%
      # exponentiate the change rate
      mutate(rt_weekly_per_change = exp(rt_weekly_per_change)
      ) %>%
      rename(mean_rt = rt_weekly_per_change) %>%
      select(-predictor)
    
    # rolling 4 week average
    dat_method_e$row_id <- seq(1:nrow(dat_method_e))
    dat_method_e$row_id <- seq(1:nrow(dat_method_e))
    dat_method_e <- dat_method_e %>%
      arrange(row_id) %>%
      mutate(
        # 7 day rolling centered mean
        mean_rt = zoo::rollapply(mean_rt, width = 7, function(x){
          if(sum(is.na(x)) >5){
            NA
          } else{mean(x, na.rm = TRUE)}
        }, fill = NA, align = "center"),
        # sd
        sd_rt = zoo::rollapply(mean_rt, width = 7, function(x){
          if(sum(is.na(x)) >5){
            NA
          } else{sd(x, na.rm = TRUE)}
        }, fill = NA, align = "center"),
        se_rt = sd_rt/sqrt(4),
        ll_95_rt= mean_rt - 1.96 * se_rt,
        ul_95_rt = mean_rt + 1.96 * se_rt
      ) %>%
      select(-row_id, -sd_rt)
    
    # invert the values
    dat_method_e <- dat_method_e %>%
      mutate(mean_rt = 1/mean_rt,
             ll_95_rt = 1/ll_95_rt,
             ul_95_rt = 1/ul_95_rt)
    
    return(dat_method_e)
    
  } else if(weekly == "No"){
    
    dat_method_e <- dataframe %>%
      arrange(date)%>%
      mutate(
        rt_weekly_per_change = (predictor - lead(predictor, change_window))/lead(predictor, change_window), 
        rt_weekly_per_change = replace_na(rt_weekly_per_change, 0)
      ) %>%
      # exponentiate the change rate
      mutate(rt_weekly_per_change = exp(rt_weekly_per_change)
      ) %>%
      rename(mean_rt = rt_weekly_per_change) %>%
      select(-predictor)
    
    
    # rolling 7 day average
    dat_method_e <- dat_method_e %>%
      arrange(date) %>%
      mutate(
        # 7 day rolling centered mean
        mean_rt = zoo::rollapply(mean_rt, width = 7, function(x){
          if(sum(is.na(x)) >5){
            NA
          } else{mean(x, na.rm = TRUE)}
        }, fill = NA, align = "center"),
        # sd
        sd_rt = zoo::rollapply(mean_rt, width = 7, function(x){
          if(sum(is.na(x)) >5){
            NA
          } else{sd(x, na.rm = TRUE)}
        }, fill = NA, align = "center"),
        se_rt = sd_rt/sqrt(4),
        ll_95_rt= mean_rt - 1.96 * se_rt,
        ul_95_rt = mean_rt + 1.96 * se_rt
      ) %>%
      select(-sd_rt)
    
    return(dat_method_e)
    # invert the values
    dat_method_e <- dat_method_e %>%
      mutate(mean_rt = 1/mean_rt,
             ll_95_rt = 1/ll_95_rt,
             ul_95_rt = 1/ul_95_rt)
  }
}
# Prep data for EPCR
expoData <- function(data){
  data <- data %>% 
    filter(Date >= as.Date("2023-01-15") & Date <= as.Date("2023-10-01")) %>% 
    select(Date, genCopiesToPMMoV) %>%
    drop_na()
  data$genCopiesToPMMoV <- log(data$genCopiesToPMMoV)
  return(data)
}

dataset_expo <- expoData(wastewater_cleaned)


rt_expo_1 <- rt_change_rate_function(dataset_expo, 1, "No")
rt_expo_1$date <- as.Date(rt_expo$date, format = "%Y-%m-%d")

rt_expo_4 <- rt_change_rate_function(dataset_expo, 4, "No")
rt_expo_4$date <- as.Date(rt_expo$date, format = "%Y-%m-%d")
