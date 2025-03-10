library(tidyverse)
setwd("/Users/jonasrauser/Desktop/R/Comparison-of-different-Rt-Estimates-from-wastewater-data/rawData")


### Load Data
cohort_raw <- read.csv("cohortData_RLP.csv", header = T, sep = ";")
wastewater_raw <- read.csv("completeData_RLP.csv", header = T, sep = ";")
hospitalizations_raw <- read.csv("Hospitalisierungen.csv", header = T, sep = ",")%>%
  filter(Bundesland == "Rheinland-Pfalz")

### Clean Data
# Remove all irrelevant features
wastewater_cleaned <- wastewater_raw %>%
  select(-"X", -"Kläranlagen.ID", -"Probenbegleitschein.vollständig", -"Niederschläge.am.Vortag.der.Probenentnahme..mm.", 
         -"Niederschläge.am.Probenentnahmetag..mm.", -"Lufttemperatur.am.Probenentnahmetag..Grad.Celsius.", 
         -"Chemischer.Sauerstoffbedarf.CSB..mg.l.", -"Total.Organic.Carbon.TOC..mg.l.")


# Remove Extra Rows from differentation in Age-group ("Altergruppe") and sum values up on the same date 
# => Set on only one row per Date
hospitalizations_cleaned <- hospitalizations_raw %>%
  select(-Altersgruppe, -Bundesland_Id) %>% 
  group_by(Datum) %>% 
  summarise(Fallzahlen = sum(X7T_Hospitalisierung_Faelle, na.rm = TRUE))


# Similar thing for cohort-Study-data :) 
cohort_clenaed <- cohort_raw %>%
  select(-Town,-Study.phase) %>%
  group_by(Date.of.SARS.CoV.2.test) %>% 
  summarise(Valid.test = sum(Valid.test, na.rm = TRUE), 
            Positive.test = sum(Positive.test, na.rm = TRUE),
            Newly.positive.test = sum(Newly.positive.test, na.rm = TRUE))
