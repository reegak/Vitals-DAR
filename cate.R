library(readxl)  # To read xlsx
library(readr)   # Fast csv write
library(dplyr) #common tidy functions
library(tidyverse)
library(corrplot)
library(MASS) #stepAIC
library(car) #vif
library(pROC)
library(corrplot)
#converting xlsx to csv
setwd("C:/Users/Kelso Quan/Documents/DAR")
df <- read_excel("DATA-FILEsp2020.xlsx")
write_csv(df, path="patient.csv")
data <- read.csv("patient.csv", header = F)
sum(is.na(data))
###tidy###
colnames(data) <-  c("id","age", "ht", "sex", "survive", "shock_type", "sbp",
                     "map", "hr", "dbp", "mcvp", "bsi", "ci", "at", "mct", "uo", 
                     "pvi", "rci", "hg", "hct", "record")
data[data$survive == 1, "survive"] <- "Survived"
data[data$survive == 3, "survive"] <- "Died"
data[data$sex == 2, "sex"] <- "Female"
data[data$sex == 1, "sex"] <- "Male"
data[data$shock_type == 2, "shock_type"] <- "Non-Shock"
data[data$shock_type == 3, "shock_type"] <- "Hypovolemic"
data[data$shock_type == 4, "shock_type"] <- "Cardiogenic"
data[data$shock_type == 5, "shock_type"] <- "Bacterial"
data[data$shock_type == 6, "shock_type"] <- "Neurogenic"
data[data$shock_type == 7, "shock_type"] <- "Other"

attach(data)
fit <- glm(as.factor(survive)~ age + ht + as.factor(sex) + as.factor(shock_type) + sbp + map +
             hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
             rci + hg + hct + record, data = data)
