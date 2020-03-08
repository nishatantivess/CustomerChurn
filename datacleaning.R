#loading data and packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(corrplot)
TelcoChurn <- read.csv('./Team 43 Projects/TelcoChurn raw.csv')

# senior citizen is coded as an integer and not a flag - lets fix that
TelcoChurn <- TelcoChurn %>%
  mutate(SeniorCitizen = ifelse(SeniorCitizen == 1, "Yes", "No"))

# lets do the same for gender as well - new column for gender_male - Yes means male and no means female
TelcoChurn <- TelcoChurn %>%
  mutate(genderMale = ifelse(gender == 'Male', "Yes", "No")) 

#Total charges seem to be the only column with missing values - lets look into this. 
TelcoChurnMissing <- TelcoChurn[!complete.cases(TelcoChurn),]

# It seems total charges are missing for customers who are new. Lets replace their total charges with their monthly charges
TelcoChurn$TotalCharges[is.na(TelcoChurn$TotalCharges)] <- TelcoChurn$MonthlyCharges[is.na(TelcoChurn$TotalCharges)]

#Checking for primary key 
length(TelcoChurn$customerID) == length(unique(TelcoChurn$customerID)) 

#data cleaning for modeling
TelcoChurn_ADS <- read.csv("raw_data.csv")

#fixing missing values
TelcoChurn_ADS$TotalCharges[is.na(TelcoChurn_ADS$TotalCharges)] <- TelcoChurn_ADS$MonthlyCharges[is.na(TelcoChurn_ADS$TotalCharges)]

#factoring all cat variables 
TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(Partner = ifelse(Partner == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(Dependents = ifelse(Dependents == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(PhoneService = ifelse(PhoneService == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(MultipleLines = ifelse(MultipleLines == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(OnlineSecurity = ifelse(OnlineSecurity == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(DeviceProtection = ifelse(DeviceProtection == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(TechSupport = ifelse(TechSupport == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(StreamingTV = ifelse(StreamingTV == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(StreamingMovies = ifelse(StreamingMovies == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(PaperlessBilling = ifelse(PaperlessBilling == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(OnlineBackup = ifelse(OnlineBackup == "Yes", 1, 0))


