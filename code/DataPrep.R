library(tidyverse)
setwd("/Users/jonasrauser/Desktop/R/Comparison-of-different-Rt-Estimates-from-wastewater-data/rawData")


### Load Data

cohort_raw <- read.csv("cohortData_RLP.csv", header = T, sep = ";")%>%
  rename(Date = Date.of.SARS.CoV.2.test)

wastewater_raw <- read.csv("completeData_RLP.csv", header = T, sep = ";")%>%
  rename(Date = Datum.der.Entnahme)%>%
  filter(Probe.Valide == " ja") #  ! Leerzeichen for "ja" ! 

hospitalizations_raw <- read.csv("Hospitalisierungen.csv", header = T, sep = ",")%>%
  filter(Bundesland == "Rheinland-Pfalz")%>%
  rename(Date = Datum)



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
            genCopiesToPMMoV = sum(Anteil.Genkopien.Durchschnitt.zu.PMMoV.x100.000, na.rm = TRUE))

wastewater_cleaned$Date <- as.Date(wastewater_cleaned$Date, format = "%Y-%m-%d")


## Hospitalization-data
hospitalizations_cleaned <- hospitalizations_raw %>%
  select(-Altersgruppe, -Bundesland_Id) %>% 
  # summarize per Date
  group_by(Date) %>% 
  summarise(Fallzahlen = sum(X7T_Hospitalisierung_Faelle, na.rm = TRUE))

hospitalizations_cleaned$Date <- as.Date(hospitalizations_cleaned$Date, format = "%Y-%m-%d")


## SentiSurv-data
cohort_cleaned <- cohort_raw %>%
  select(-Town,-Study.phase) %>%
  group_by(Date) %>% 
  summarise(Valid.test = sum(Valid.test, na.rm = TRUE), 
            Positive.test = sum(Positive.test, na.rm = TRUE),
            Newly.positive.test = sum(Newly.positive.test, na.rm = TRUE)) %>%
  mutate(infectiousRate = (Positive.test + Newly.positive.test)/Valid.test) %>%
  select(Date, infectiousRate)

cohort_cleaned$Date <- as.Date(cohort_cleaned$Date, format = "%Y-%m-%d")


##### Function to set time-span on all datasets (is the maximal timespan, where data is available on every dataset)

alignTimespan <- function(df){
  df %>% filter(Date >= as.Date("2023-01-01") & Date <= as.Date("2023-10-01"))
}

alignTimespan(wastewater_cleaned)
alignTimespan(hospitalizations_cleaned)
alignTimespan(cohort_cleaned)
