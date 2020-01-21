x <- seq(1:12)
y <- matrix(x, nrow = 4, ncol = 3)
y

colnames(y) <- c("v1", "v2", "v3")
y

library("readxl")  # To read xlsx
library("readr")   # Fast csv write

df <- read_excel("DATA-FILEsp2020.xlsx")
write_csv(df, path="patient.csv")


