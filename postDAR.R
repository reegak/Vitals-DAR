library(readxl)  # To read xlsx
library(readr)   # Fast csv write
library(dplyr) #common tidy functions
library(tidyverse)
library(corrplot)
library(MASS) #stepAIC
library(car) #vif
library(cluster)
library(effsize)
library(carat)#sensitivity and specificity 
#converting xlsx to csv
setwd("C:/Users/Kelso Quan/Documents/DAR")
df <- read_excel("DATA-FILEsp2020.xlsx")
write_csv(df, path="patient.csv")
data <- read.csv("patient.csv", header = F)

###tidy###
colnames(data) <-  c("id","age", "ht", "sex", "survive", "shock_type", "sbp",
"map", "hr", "dbp", "mcvp", "bsi", "ci", "at", "mct", "uo", 
"pvi", "rci", "hg", "hct", "record")

data[data$survive == 1, "survive"] <- "Survived" #0 survived
data[data$survive == 3, "survive"] <- "Died" #1 died
data[data$sex == 2, "sex"] <- "Female"
data[data$sex == 1, "sex"] <- "Male"
data[data$shock_type == 2, "shock_type"] <- "Non-Shock"
data[data$shock_type == 3, "shock_type"] <- "Hypovolemic"
data[data$shock_type == 4, "shock_type"] <- "Cardiogenic"
data[data$shock_type == 5, "shock_type"] <- "Bacterial"
data[data$shock_type == 6, "shock_type"] <- "Neurogenic"
data[data$shock_type == 7, "shock_type"] <- "Other"
sum(is.na(data))
attach(data)
pre <- subset(data ,record == 1)
post <- subset(data, record ==2)
attach(post)
dim(post)
cont <- data.frame(age, ht, sbp,map,hr,dbp,mcvp,bsi,ci,at,mct,uo,pvi,rci,hg,hct)
corrplot(cor(cont))
xtable(corrplot(cor(cont)))
chisq.test(survive,shock_type)
chisq.test(survive,sex)
chisq.test(sex,shock_type)
table(survive)
table(survive, shock_type)
table(survive, sex)

table(sex)

ggplot()+
  geom_boxplot(mapping = aes(group = shock_type, y = at))
               

fit.post <- glm(survive~ age + ht + sex + shock_type + sbp + map +
             hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
             rci + hg + hct,data = post)
aov(fit.post)
summary(fit.post)
stepAIC(fit.post)

fit.post.step <- glm(formula = survive ~ map + mcvp + mct + uo + pvi + hg, data = post)
aov(fit.post.step)
summary(fit.post.step)
vif(fit.post.step)
corrplot(cor(data.frame(map,mcvp,mct,uo,pvi,hg)))

fit.pre <-  glm(survive~ age + ht + sex + shock_type + sbp + map +
                  hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
                  rci + hg + hct,data = pre)
summary(fit.pre)
aov(fit.pre)
vif(fit.pre)
stepAIC(fit.pre)

fit.step.pre <- glm(formula = survive ~ shock_type + map + mcvp + bsi + uo + 
                      pvi, data = pre)
summary(fit.step.pre)
aov(fit.step.pre)
vif(fit.step.pre)
corrplot(cor(data.frame(map,mcvp,bsi,uo,pvi)))

#subset by sex and record
female <- subset(pre, sex == 2)
male <- subset(pre, sex == 1)


female.pre <- glm(survive~ age + ht + shock_type + sbp + map +
                  hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
                  rci + hg + hct, data = female)
stepAIC(female.pre)
summary(female.pre)
vif(female.pre)
female.pre.step <- glm(formula = survive ~ age + shock_type + sbp + hr + bsi + uo + 
                             hg + hct, data = female)
summary(female.pre.step)
aov(female.pre.step)
male.pre <- glm(survive~ age + ht + shock_type + sbp + map +
                  hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
                  rci + hg + hct, data = male)
summary(male.pre)
vif(male.pre)
stepAIC(male.pre)
male.pre.step <- glm(formula = survive ~ age + shock_type + map + hr + mcvp + 
                       pvi + hct, data = male)
summary(male.pre.step)
vif(male.pre.step)
aov(male.pre.step)
female.post <- subset(post, sex ==2)
male.post <- subset(post,sex == 1)
female.po <- glm(survive~ age + ht + shock_type + sbp + map +
                   hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
                   rci + hg + hct, data = female.post)
vif(female.po)
summary(female.po)
stepAIC(female.po)

female.po.step <- glm(formula = survive ~ map + hr + mcvp + ci + at + uo + rci, 
                      data = female.post)
summary(female.po.step)
vif(female.po.step)
aov(female.po.step)
male.po <- glm(survive~ age + ht + shock_type + sbp + map +
                 hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
                 rci + hg + hct, data = male.post)
summary(male.po)
vif(male.po)
stepAIC(male.po)

male.po.step <- glm(formula = survive ~ age + map + mcvp + at + mct + pvi + hg, 
                    data = male.post)
summary(male.po.step)
vif(male.po.step)
aov(male.po.step)
table(survive)
table(sex,survive)
threshold <- 69
predict.post <- ifelse(predict(fit.post.step, type ="response")>threshold, 1,0)
actual.post <- fit.post.step$y
confused.post <- table(predict.fit,actual.fit)
confused.post
predict.pre <- ifelse(predict(fit.step.pre, type ="response")>threshold, 1,0)
actual.pre <- fit.step.pre$y
confused.pre <- table(predict.pre,actual.pre)
confused.pre
predict.female.pre <- ifelse(predict(female.pre.step, type ="response")>threshold, 1,0)
actual.female.pre <- female.pre.step$y
confused.female.pre <- table(predict.female.pre,actual.female.pre)
confused.female.pre
predict.male.pre <- ifelse(predict(male.pre.step, type ="response")>threshold, 1,0)
actual.male.pre <- male.pre.step$y
confused.male.pre <- table(predict.male.pre,actual.male.pre)
confused.male.pre
predict.male.po <- ifelse(predict(male.po.step, type ="response")>threshold, 1,0)
actual.male.po <- male.po.step$y
confused.male.po <- table(predict.male.po,actual.male.po)
confused.male.po
predict.female.po <- ifelse(predict(female.po.step, type ="response")>threshold, 1,0)
actual.female.po <- female.po.step$y
confused.female.po <- table(predict.female.po,actual.female.po)
confused.female.po


fit.step.pre <- glm(formula = survive ~ shock_type + map + mcvp + bsi + uo + 
                      pvi, data = pre)
summary(fit.step.pre)
#shock_typeCardiogenic 
exp(0.1112211 )
exp(0.1112211 +1.96* 0.1470485   )
exp(0.1112211 -1.96* 0.1470485   )
#shock_typeHypovolemic 
exp(0.1996907)
exp(0.1996907 +1.96* 0.1505414   )
exp(0.1996907 -1.96* 0.1505414   )
#shock_typeNeurogenic  
exp(0.0676513)
exp(0.0676513 +1.96* 0.1544429 )
exp(0.0676513 -1.96* 0.1544429 )
#shock_typeNon-Shock  
exp(-0.1234019)
exp(-0.1234019  +1.96*0.1358790)
exp(-0.1234019  -1.96*0.1358790)
#shock_typeOther
exp(0.3339611)
exp(0.3339611 + 1.96*0.1721288)
exp(0.3339611 - 1.96*0.1721288)
#map    
exp(-0.0047308)
exp(-0.0047308 + 1.96*0.0020086)
exp(-0.0047308 - 1.96*0.0020086)
 #mcvp   
exp(0.0021150 )
  exp(0.0021150  +1.96*0.0007208)
  exp(0.0021150  -1.96*0.0007208)
  #bsi   
  exp(-0.0056289)
  exp(-0.0056289  +1.96*0.0022697)
  exp(-0.0056289  -1.96*0.0022697)
  #uo         
  exp(-0.0004959)
  exp(-0.0004959 +1.96* 0.0003780)
  exp(-0.0004959 -1.96* 0.0003780)
#pvi
  exp(-0.0003258)
  exp(-0.0003258  +1.96*0.0002965)
  exp(-0.0003258  -1.96*0.0002965)
  