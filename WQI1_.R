#Pennsylvania Water Quality Index Project - 2019
#Loads data as CSV
library(tidyverse)
library(readr)
library(chron)
library(ggplot2)
library(tibble)
library(magrittr)
library(lubridate)

PAWater <- read_csv('YOURPATHGOESHERE/Sep00_Dec19_PAWater.csv') 

# No need to alter code below - First run the entire script, then run the app (WQI2.R)
#---------------------------------------------------------------------------------------------------------
#Separate variables for WQI  Study
PAwqi <- PAWater[,c(1:4,21,13,16,17,19,15,14,18,12)]
rm(PAWater)
#Cleaning Data (Column Names, Data Types/Formats, Missing Values, etc.)

#ColNames
names(PAwqi)[1] <- "Site"
names(PAwqi)[5] <- "WQI"
names(PAwqi)[6] <- "pH"
names(PAwqi)[7] <- "NO3N"
names(PAwqi)[8] <- "P"
names(PAwqi)[9] <- "Turbidity"
names(PAwqi)[10] <- "DO"
names(PAwqi)[11] <- "TDS"
names(PAwqi)[12] <- "Alkalinity"
names(PAwqi)[13] <- "Temp"
#DataTypes
PAwqi$Site <- as.numeric(as.character(PAwqi$Site))
PAwqi$Date <- as_date(PAwqi$Date)
PAwqi$Time <- strptime(PAwqi$Time,'%H:%M:%S')
PAwqi$Time <- format(PAwqi$Time, format = '%H:%M:%S')
PAwqi$WQI <- as.double(as.character(PAwqi$WQI))
PAwqi$pH <- as.double(as.character(PAwqi$pH))
PAwqi$NO3N <- as.double(as.character(PAwqi$NO3N))
PAwqi$P <- as.double(as.character(PAwqi$P))
PAwqi$Turbidity <- as.double(as.character(PAwqi$Turbidity))
PAwqi$DO <- as.double(as.character(PAwqi$DO))
PAwqi$TDS <- as.double(as.character(PAwqi$TDS))
PAwqi$Alkalinity <- as.double(as.character(PAwqi$Alkalinity))
PAwqi$Temp <- as.double(as.character(PAwqi$Temp))

#All Quantitative Variable NAs to 0
PAwqi2 <- PAwqi[,c(5:13)]
PAwqi2[is.na(PAwqi2)] <- 0
PAwqi[,c(5:13)] <- PAwqi2
remove(PAwqi2)

#Recalculating WQI and creating WQI Error Variable
PAwqi$WQI = 10 + (abs((PAwqi$pH - 7.5)*8) + (PAwqi$NO3N * 10) + (PAwqi$P * 15) + (PAwqi$Turbidity * 0.2) - (PAwqi$DO * 0.7))

#Removing observations with NA site values and outside abnormal levels
unassigned <- PAwqi[is.na(PAwqi$Site) | is.na(PAwqi$School),]
PAwqi <- PAwqi[!is.na(PAwqi$Site),]
rm(unassigned)


PAwqi <- subset(PAwqi, WQI >= 0 & WQI <= 100)
PAwqi <- subset(PAwqi, pH >= 0 & pH <= 14)
PAwqi <- subset(PAwqi, pH >= 0 & pH <= 14)
PAwqi <- subset(PAwqi, PAwqi$P >= 0 & PAwqi$P <= 1)
PAwqi <- subset(PAwqi, PAwqi$Alkalinity >= 0 & PAwqi$Alkalinity <= 300)
PAwqi <- subset(PAwqi, PAwqi$Turbidity >= 0 & PAwqi$Turbidity <= 100)
PAwqi <- subset(PAwqi, PAwqi$DO >= 0 & PAwqi$DO <= 15)
PAwqi <- subset(PAwqi, PAwqi$Temp <= 34)
PAwqi <- PAwqi %>% arrange(Site)

#Next Step - Nested Data Frames by Site
by_site <- PAwqi %>%
  group_by(Site) %>%
  nest()

by_site <- by_site %>% arrange(Site)

#Applying Selected MLR Model to each site
id_site <- function(df){
  lm(WQI ~ DO + pH + P + Turbidity + NO3N + Alkalinity +
       TDS + Temp, data = df)
}

#DF of data and applied models
models <- by_site %>%
  mutate(
    model = data %>% map(id_site)
  )

#Extracting statistics about each model (R2, Intercept/Slope, Residuals)
models <- models %>%
  mutate(
    glance  = model %>% map(broom::glance),
    rsq     = glance %>% map_dbl("r.squared"),
    tidy    = model %>% map(broom::tidy),
    augment = model %>% map(broom::augment)
  )

models$rsq <- as.character(as.numeric(models$rsq))
models <- models %>% arrange(Site)

# by_site - observations organized by site
# PAwqi - main DF with all observations
# models - includes all models using MLR by site


