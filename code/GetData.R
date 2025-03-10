# Test all the necessary Models with the data

rm(list = ls())
setwd("/Users/jonasrauser/Desktop/R/Comparison-of-different-Rt-Estimates-from-wastewater-data/rawData")
library(tidyverse)

# Load Data
cohortData_raw <- read.csv("cohortData_RLP.csv", header = T, sep = ";")
completeData_raw <- read.csv("completeData_RLP.csv", header = T, sep = ";")
hospitalisierungen_raw <- read.csv("Hospitalisierungen.csv", header = T, sep = ",")%>%
  filter(Bundesland == "Rheinland-Pfalz")


# remove all unnecessary data-features
completeData_relVar <- completeData_raw %>%
  select(-"X", -"Kläranlagen.ID", -"Probenbegleitschein.vollständig", -"Niederschläge.am.Vortag.der.Probenentnahme..mm.", 
         -"Niederschläge.am.Probenentnahmetag..mm.", -"Lufttemperatur.am.Probenentnahmetag..Grad.Celsius.", 
         -"Chemischer.Sauerstoffbedarf.CSB..mg.l.", -"Total.Organic.Carbon.TOC..mg.l.")

# get a brief overview of Data
head(cohortData_raw)
head(completeData_relVar)