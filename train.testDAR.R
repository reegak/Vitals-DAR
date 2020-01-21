library(readxl)  # To read xlsx
library(readr)   # Fast csv write
library(dplyr) #common tidy functions
library(tidyverse)
library(corrplot)
library(MASS) #stepAIC
library(car) #vif
library(cluster)
library(effsize)
#converting xlsx to csv
setwd("C:/Users/Kelso Quan/Documents/DAR")
df <- read_excel("DATA-FILEsp2020.xlsx")
write_csv(df, path="patient.csv")
data <- read.csv("patient.csv", header = F)


###tidy###
dim(data)
head(data)
data[1,]
data[,1]
tail(data)
data
colnames(data) <-  c("id","age", "ht", "sex", "survive", "shock_type", "sbp",
                     "map", "hr", "dbp", "mcvp", "bsi", "ci", "at", "mct", "uo", 
                     "pvi", "rci", "hg", "hct", "record")

data[data$survive == 1, "survive"] <- 0 #0 survived
data[data$survive == 3, "survive"] <- 1 #1 died
sum(is.na(data))
data[data$survive == 1, "survive"] <- 0 #0 survived
data[data$survive == 3, "survive"] <- 1 #1 died
data[data$survive == 0, "survive"] <- "Survived"
data[data$survive == 1, "survive"] <- "Died"
data[data$sex == 2, "sex"] <- "Female"
data[data$sex == 1, "sex"] <- "Male"
data[data$shock_type == 2, "shock_type"] <- "Non-Shock"
data[data$shock_type == 3, "shock_type"] <- "Hypovolemic"
data[data$shock_type == 4, "shock_type"] <- "Cardiogenic"
data[data$shock_type == 5, "shock_type"] <- "Bacterial"
data[data$shock_type == 6, "shock_type"] <- "Neurogenic"
data[data$shock_type == 7, "shock_type"] <- "Other"

attach(data)


###EDA###
#binary response
table(survive)
#binary response vs categorical covariates
table(survive, sex)
table(shock_type)
table(survive,shock_type)
round(prop.table(table(survive,shock_type)),digits = 2)
table(survive,record)
#binary response vs continuous covariates
boxplot(age~survive)
boxplot(ht~survive)
boxplot(sbp~survive)
boxplot(map~survive)
boxplot(hr~survive)
boxplot(dbp~survive)
boxplot(mcvp~survive)
boxplot(bsi~survive)
boxplot(ci~survive)
boxplot(at~survive)
boxplot(mct~survive)
boxplot(uo~survive)
boxplot(pvi~survive)
boxplot(rci~survive)
boxplot(hg~survive)
boxplot(hct~survive)
#standardize mean difference: evaluation of relationships btw covariates and response
cohen.d(age,survive)
cohen.d(ht, survive)
cohen.d(sex,survive)#doesn't mean much
chisq.test(sex,survive)
cohen.d(shock_type, survive)#doesn't mean much
chisq.test(shock_type,survive)
cohen.d(sbp,survive)
cohen.d(hr,survive)
cohen.d(dbp, survive)
cohen.d(mcvp, survive)
cohen.d(bsi,survive)
cohen.d(ci,survive)
cohen.d(at,survive)
cohen.d(mct, survive)
cohen.d(uo,survive)
cohen.d(pvi,survive)
cohen.d(rci,survive)
cohen.d(hg, survive)
cohen.d(hct,survive)
cohen.d(record,survive) #doesn't mean much
chisq.test(record,survive)

#subset on record
pre <- subset(data ,record == 1)
post <- subset(data, record ==2)

cohen.d(pre$sbp,survive)
cohen.d(post$sbp,survive)
cohen.d(hr,survive)
cohen.d(pre$hr,survive)
cohen.d(hr,survive)
cohen.d(post$dbp, survive)
cohen.d(mcvp, survive)
cohen.d(pre$mcvp, survive)
cohen.d(post$mcvp, survive)
cohen.d(bsi,survive)
cohen.d(pre$bsi,survive)
cohen.d(post$bsi,survive)
cohen.d(ci,survive)
cohen.d(pre$ci,survive)
cohen.d(post$ci,survive)
cohen.d(at,survive)
cohen.d(mct, survive)
cohen.d(uo,survive)
cohen.d(pvi,survive)
cohen.d(rci,survive)
cohen.d(hg, survive)
cohen.d(hct,survive)



ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = age))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = ht))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = sbp))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = map))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = hr))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = dbp))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = mcvp))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = bsi))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = ci))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = at))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = mct))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = uo))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = pvi))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = rci))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = hg))
ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = hct))

##correlation##
corr.plot <- data.frame(age,ht,sex,survive, shock_type, sbp, map,
                        hr, dbp, mcvp, bsi, ci, at, mct, uo, pvi,
                        rci, hg, hct, record)
data.cor <- cor(corr.plot)
corrplot(data.cor)

corr.pre <- data.frame(pre$age,pre$ht,pre$sex,pre$survive,pre$shock_type, pre$sbp,
                       pre$map,pre$hr, pre$dbp, pre$mcvp, pre$bsi, pre$ci, pre$at,
                       pre$mct, pre$uo, pre$pvi,pre$rci, pre$hg, pre$hct)
data.pre <- cor(corr.pre)
corrplot(data.pre)
corr.post <- data.frame(post$age,post$ht,post$sex,post$survive,post$shock_type, post$sbp,
                        post$map,post$hr, post$dbp, post$mcvp, post$bsi, post$ci, post$at,
                        post$mct, post$uo, post$pvi,post$rci, post$hg, post$hct)
data.post <- cor(corr.post)
corrplot(data.post)




###model selection###
a <- age + ht + sex + shock_type + sbp + map +
  hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
  rci + hg + hct + record
fit <- glm(survive~ age + ht + sex + shock_type + sbp + map +
      hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
      rci + hg + hct + record, data = data)
summary(fit)
vif(fit)
fit.pre <- glm(survive~ age + ht + sex + shock_type + sbp + map +
             hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
             rci + hg + hct, data = pre)
summary(fit.pre)
vif(fit.pre)
fit.post<- glm(survive~ age + ht + sex + shock_type + sbp + map +
                 hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
                 rci + hg + hct, data = post)
summary(fit.post)
vif(fit.pre)


