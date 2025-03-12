library(tidyverse)
library(zoo)
setwd("/Users/jonasrauser/Desktop/R/Comparison-of-different-Rt-Estimates-from-wastewater-data/rawData")


### Load Data

cohort_raw <- read.csv("cohortData_RLP.csv", header = T, sep = ";")%>%
  rename(Date = Date.of.SARS.CoV.2.test)

wastewater_raw <- read.csv("completeData_RLP.csv", header = T, sep = ";")%>%
  rename(Date = Datum.der.Entnahme)%>%
  filter(Probe.Valide == " ja") #  ! whitespace before "ja"

# 7-day/weekly hospitalization incidence:
hospitalizations_raw <- read.csv("Hospitalisierungen.csv", header = T, sep = ",")%>%
  filter(Bundesland == "Rheinland-Pfalz")%>%
  rename(Date = Datum)

# daily hospitalization incidence
hospitalzations_deconvoluted <- read.csv("hospitalization_deconvoluted.csv", head = T) %>%
  filter(location == "DE-RP") %>%
  rename(Date = date)

### Clean Data

## Wastewater-data
wastewater_cleaned <- wastewater_raw %>%
  # Select relevant features
  select(Date, Genkopien.im.Durchschnitt..Gq.ml., Genkopien.N1.ml, 
         Genkopien.N2.ml, Anteil.Genkopien.Durchschnitt.zu.PMMoV.x100.000, Probenvolumen..ml.) %>%
  # Set all values as numeric
  mutate(across(
    c(`Genkopien.im.Durchschnitt..Gq.ml.`, `Genkopien.N1.ml`, `Genkopien.N2.ml`, `Anteil.Genkopien.Durchschnitt.zu.PMMoV.x100.000`),
    as.numeric)) %>%
  # summarize per Date
  group_by(Date) %>%
  summarise(genCopiesAvg = sum(Genkopien.im.Durchschnitt..Gq.ml., na.rm = TRUE),
            genCopiesN1 = sum(Genkopien.N1.ml, na.rm = TRUE),
            genCopiesN2 = sum(Genkopien.N2.ml, na.rm = TRUE),
            genCopiesToPMMoV = sum(Anteil.Genkopien.Durchschnitt.zu.PMMoV.x100.000, na.rm = TRUE))%>%
  mutate(genCopiesN1_N2 = genCopiesN1 + genCopiesN2) %>%
  select(-genCopiesN1, -genCopiesN2)
wastewater_cleaned$Date <- as.Date(wastewater_cleaned$Date, format = "%Y-%m-%d")


## Hospitalization-data
hospitalizations_cleaned <- hospitalizations_raw %>%
  select(-Altersgruppe, -Bundesland_Id) %>% 
  # summarize per Date
  group_by(Date) %>% 
  summarise(Fallzahlen = sum(X7T_Hospitalisierung_Faelle, na.rm = TRUE))
hospitalizations_cleaned$Date <- as.Date(hospitalizations_cleaned$Date, format = "%Y-%m-%d")


hospitalizations_deconvoluted_cleaned <- hospitalzations_deconvoluted %>%
  select(-location, -age_group) %>%
  group_by(Date) %>%
  summarise(Hospitalizations = sum(value, na.rm = TRUE))
hospitalizations_deconvoluted_cleaned$Date <- as.Date(hospitalizations_deconvoluted_cleaned$Date, format = "%Y-%m-%d")

## SentiSurv-data
cohort_cleaned <- cohort_raw %>%
  select(-Town,-Study.phase) %>%
  group_by(Date) %>% 
  summarise(Valid.test = sum(Valid.test, na.rm = TRUE), 
            Positive.test = sum(Positive.test, na.rm = TRUE),
            Newly.positive.test = sum(Newly.positive.test, na.rm = TRUE)) %>%
  mutate(infectiousRate = (Positive.test + Newly.positive.test)/Valid.test)
cohort_cleaned$Date <- as.Date(cohort_cleaned$Date, format = "%Y-%m-%d")


# Try different features of cohortStudy, to infere for Rt:
cohortInfRate <- cohort_cleaned %>%
  select(Date, infectiousRate)

cohortPosTest <- cohort_cleaned %>%
  mutate(positiveTests = Positive.test + Newly.positive.test) %>%
  select(Date, positiveTests)


##### Function to set time-span on all datasets (is the maximal timespan, where data is available on every dataset)
alignTimespan <- function(df){
  colnames(df)[2] <- "case_data"
  df <- df %>%
   # filter(Date >= as.Date("2023-01-01") & Date <= as.Date("2023-10-01")) %>%
    # -> Cutted out the dates from 2023-01-01 to 2023-01-15 bcs of to high variance of holidays
    filter(Date >= as.Date("2023-01-15") & Date <= as.Date("2023-10-01")) %>%
    # fill in missing dates
    complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
    # interpolate missing values
    mutate(I = na.approx(case_data)) %>% 
    dplyr::select(Date, I)
  return(df)
}

hospitalizations_aligned <- alignTimespan(hospitalizations_cleaned)
hosp_conv_aligned <- alignTimespan(hospitalizations_deconvoluted_cleaned)


wastewater_aligned <- alignTimespan(wastewater_cleaned)
wastewater_toPMMoV <- alignTimespan(wastewater_cleaned %>%
                                      select(Date, genCopiesToPMMoV))

cohort_aligned <- alignTimespan(cohort_cleaned)
cohortInfRate_aligned <- alignTimespan(cohortInfRate)
cohortPosTest_aligned <- alignTimespan(cohortPosTest)
