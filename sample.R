library(readxl)  # To read xlsx
library(readr)   # Fast csv write
library(MASS) #stepAIC
library(car) #vif
library(effsize)
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
data[data$survive == 1, "survive"] <- 0 #0 survived
data[data$survive == 3, "survive"] <- 1 #1 died
as.factor(data$survive)
data$shock_type <- ifelse(data$shock_type==2,"Non-Shock",
                          ifelse(data$shock_type==3,"HypoVolemic",
                                 ifelse(data$shock_type==4,"Cardiogenic",
                                        ifelse(data$shock_type==5,"Bacterial",
                                               ifelse(data$shock_type==6,"Neurogenic",
                                                      "Other")))))
as.factor(data$sex)
attach(data)
pre <- subset(data ,record == 1)
pre
model <- glm(survive ~ age + ht + sex + shock_type + sbp + map+
               hr + dbp + mcvp+ bsi+ ci+ at+ mct + uo + pvi +
               rci + hg + hct , family = binomial(link = "logit"), data = pre)
summary(model)
model2 <- glm(survive ~  ht + sex + shock_type + sbp + map+
                hr + dbp + mcvp+ bsi+ ci+ at+ mct + uo + pvi +
                rci + hg + hct , family = binomial(link = "logit"), data = pre)
summary(model2)
model3 <- glm(survive ~  ht + sex + shock_type + sbp + map+
                hr + dbp + mcvp+ bsi+ ci+ at+ mct + uo + pvi +
                rci + hg + hct , family = binomial(link = "logit"), data = pre)
summary(model3)
model4 <- glm(survive ~  ht + sex + shock_type + sbp + map+
                 dbp + mcvp+ bsi+ ci+ at+ mct + uo + pvi +
                rci + hg + hct , family = binomial(link = "logit"), data = pre)
summary(model4)
model5 <-  glm(survive ~  ht + sex + shock_type + sbp + map+
                 dbp + mcvp+ bsi+ ci+ at+ mct + uo + pvi +
                 rci +  hct , family = binomial(link = "logit"), data = pre)
summary(model5)
model6 <- glm(survive ~  ht + sex + shock_type + sbp + map+
                dbp + mcvp+ bsi+ ci+  mct + uo + pvi +
                rci +  hct , family = binomial(link = "logit"), data = pre)
summary(model6)
model7 <- glm(survive ~  ht + sex + shock_type + sbp + map+
                dbp + mcvp+ bsi+ ci+  mct + uo + pvi +
                rci +  hct , family = binomial(link = "logit"), data = pre)
summary(model7)
model8 <- glm(survive ~  ht + sex + shock_type + sbp + map+
                 mcvp+ bsi+ ci+  mct + uo + pvi +
                rci +  hct , family = binomial(link = "logit"), data = pre)
summary(model8)
model9<- glm(survive ~  ht + sex + shock_type +  map+
               mcvp+ bsi+ ci+  mct + uo + pvi +
               rci +  hct , family = binomial(link = "logit"), data = pre)
summary(model9)
model10<- glm(survive ~  ht +  shock_type +  map+
               mcvp+ bsi+ ci+  mct + uo + pvi +
               rci +  hct , family = binomial(link = "logit"), data = pre)
summary(model10)
model11<- glm(survive ~  ht +  shock_type +  map+
                mcvp+ bsi+  mct + uo + pvi +
                rci +  hct , family = binomial(link = "logit"), data = pre)
summary(model11)
model12<- glm(survive ~  ht +  shock_type +  map+
                mcvp+ bsi+    uo + pvi +
                rci +  hct , family = binomial(link = "logit"), data = pre)
summary(model12)
model13<- glm(survive ~  ht +  shock_type +  map+
                mcvp+ bsi+    uo + pvi +
                rci  , family = binomial(link = "logit"), data = pre)
summary(model13)
model14<- glm(survive ~    shock_type +  map+
                mcvp+ bsi+    uo + 
                rci  , family = binomial(link = "logit"), data = pre)
summary(model14)
model15<- glm(survive ~    shock_type +  map+
                mcvp+ bsi+     
                rci  , family = binomial(link = "logit"), data = pre)
summary(model15)
model16<- glm(survive ~    shock_type +  map+
                mcvp+ bsi    
                  , family = binomial(link = "logit"), data = pre)
summary(model16)

stepAIC(model)
model.step <- glm(formula = survive ~ shock_type + map + mcvp + bsi + uo + 
      rci, family = binomial(link = "logit"), data = pre)
summary(model.step)

summary(model16)
exp(coef(model16))

exp(confint(model16))
threshold <- 42/112 
predict.final <- ifelse(predict(model16,type = "response")>threshold,1,0)
actual.final <- model16$y
confused <- table(predict.final,actual.final)
confused

