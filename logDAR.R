
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

#View(data)
nrow(data)
ncol(data)
dim(data)
head(data)
data[1,]
data[,1]
tail(data)
data

colnames(data) <-  c("id","age", "ht", "sex", "survive", "shock_type", "sbp",
                     "map", "hr", "dbp", "mcvp", "bsi", "ci", "at", "mct", "uo", 
                     "pvi", "rci", "hg", "hct", "record")
###tidy###
data[data$survive == 1, "survive"] <- 0
data[data$survive == 3, "survive"] <- 1
View(data)
sum(is.na(data))
attach(data)


###EDA###

corr.plot <- data.frame(age,ht,sex,survive, shock_type, sbp, map,
                        hr, dbp, mcvp, bsi, ci, at, mct, uo, pvi,
                        rci, hg, hct, record)
data.cor <- cor(corr.plot)
corrplot(data.cor)
View(data.cor)
pre <- subset(data ,record == 1)
post <- subset(data, record ==2)
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

##histogram/bar graph##
# hist(age)
# hist(ht)
# ggplot()+
#   geom_histogram(mapping = aes(x = age))
# 
# ggplot()+
#   geom_bar(mapping = aes(x = sex))
# hist(survive)
# ggplot()+
#   geom_bar(mapping = aes(x = survive))
# hist(shock_type)
# ggplot()+
#   geom_bar(mapping = aes(x = shock_type))
# hist(sbp)
# hist(pre$sbp)
# hist(post$sbp)
# hist(map)
# hist(pre$map)
# hist(post$map)
# hist(hr)
# hist(pre$hr)
# hist(post$hr)
# hist(dbp)
# hist(pre$dbp)
# hist(post$dbp)
# hist(mcvp)
# hist(pre$mcvp)
# hist(post$mcvp)
# hist(log(mcvp))
# hist(bsi)
# hist(pre$bsi)
# hist(post$bsi)
# hist(ci)
# hist(pre$ci)
# hist(post$ci)
# hist(log(ci))
# hist(at)
# hist(pre$at)
# hist(post$at)
# hist(log(at)) #normalize
# hist(mct)
# hist(pre$mct)
# hist(post$mct)
# hist(log(mct))
# hist(uo)
# hist(pre$uo)
# hist(post$uo)
# hist(log(uo))
# hist(pvi)
# hist(post$pvi)
# hist(pre$pvi)
# hist(log(pvi))#normalize
# hist(log(pre$pvi))
# hist(log(post$pvi))
# hist(rci)
# hist(pre$rci)
# hist(post$rci)
# hist(log(rci))
# hist(hg)
# hist(pre$hg)
# hist(post$hg)
# hist(hct)
# hist(pre$hct)
# hist(post$hct)
# hist(record) 

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

table(survive)
prop.table(table(survive))

table(survive, shock_type)
table(shock_type)
round(prop.table(table(shock_type)), digits = 2)
t.test(survive)
t.test(survive == 1, survive == 0)


####logistics regression w/o interactions####
x = age + ht + sex + shock_type + sbp + map +
   hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
   rci + hg + hct + record

fit <- glm(survive ~ age + ht + sex + shock_type + sbp + map+
   hr + dbp + mcvp+ bsi+ ci+ at+ mct + uo + pvi +
   rci + hg + hct + record, family = binomial(link = "logit"), data = data)
plot(fit)
plot(survive~x)
summary(fit)
vif(fit)
stepAIC(fit)
fit2 <- glm(survive ~ age + ht + sex + shock_type + sbp + map+
             hr + dbp + mcvp+ bsi+ ci+ at+ mct + uo + pvi +
             rci + hg + record, family = binomial(link = "logit"), data = data)
summary(fit2)
fit3 <- glm(survive ~ age + ht + sex + shock_type + map+
             hr + dbp + mcvp+ bsi+ ci+ at+ mct + uo + pvi +
             rci + hg + record, family = binomial(link = "logit"), data = data)
summary(fit3)
fit4 <- glm(survive ~ age + ht + sex + shock_type + map+
              dbp + mcvp+ bsi+ ci+ at+ mct + uo + pvi +
              rci + hg + record, family = binomial(link = "logit"), data = data)
summary(fit4)
fit5 <- glm(survive ~ age + ht + sex + shock_type + map+
              dbp + mcvp+ bsi+ at+ mct + uo + pvi +
              rci + hg + record, family = binomial(link = "logit"), data = data)
summary(fit5)
fit6 <- glm(survive ~ age + ht + sex + shock_type + map+
              dbp + mcvp+ bsi+ mct + uo + pvi +
              rci + hg + record, family = binomial(link = "logit"), data = data)
summary(fit6)
fit7 <- glm(survive ~ age + sex + shock_type + map+
              dbp + mcvp+ bsi+ mct + uo + pvi +
              rci + hg + record, family = binomial(link = "logit"), data = data)
summary(fit7)
vif(fit7)
fit8 <- glm(survive ~  sex + shock_type + map+
              dbp + mcvp+ bsi+ mct + uo + pvi +
              rci + hg + record, family = binomial(link = "logit"), data = data)
summary(fit8) ###
vif(fit8)
fit9 <- glm(survive ~  sex + shock_type + map+
               mcvp+ bsi+ mct + uo + pvi +
              rci + hg + record, family = binomial(link = "logit"), data = data)
summary(fit9)
vif(fit9)
fit10 <- glm(survive ~  sex + shock_type + map+
              mcvp+ bsi+ mct + uo + pvi +
              rci + hg , family = binomial(link = "logit"), data = data)
summary(fit10)
vif(fit10)
fit11 <- glm(survive ~ shock_type + map+
               mcvp+ bsi+ mct + uo + pvi +
               rci + hg , family = binomial(link = "logit"), data = data)
#plot(y = survive, x = shock_type + map+
       mcvp+ bsi+ mct + uo + pvi +
       rci + hg )
a = shock_type + map+
  mcvp+ bsi+ mct + uo + pvi +
  rci + hg

summary(fit11)
vif(fit11)
datavars <- data.frame(shock_type,map,mcvp,bsi,mct,uo,pvi,rci,hg)
cdv <- cor(datavars)
corrplot(cdv)
roc(survive,a, plot = T, legacy.axes = T, percent = T, print.auc = T)
roc(survive, a, plot = T)
b <- data.frame(map,mcvp,bsi,mct,uo,pvi,rci,hg)
corrplot(cor(b))
chisq.test(y = survive, x = shock_type)
chisq.test(y = survive, x = ht)

###model w/ interactions###
#fit.int <- glm(survive~.*., family = binomial(link = logit), data = data)
##model pre/post
fit.pre <- glm(survive~ age + ht + sex + shock_type + sbp + map +
                 hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
                 rci + hg + hct, data = pre)
summary(fit.pre)
vif(fit.pre)
fit.pre1 <- glm(survive~ age + ht + sex + shock_type + sbp + map +
                 hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
                 rci + hct, data = pre)
summary(fit.pre1)
vif(fit.pre1)
fit.pre2 <- glm(survive~  ht + sex + shock_type + sbp + map +
                  hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
                  rci + hct, data = pre)
summary(fit.pre2)
vif(fit.pre2)
fit.pre3 <- glm(survive~  ht + sex + shock_type + sbp + map +
                  hr + dbp + mcvp + bsi + ci +  mct + uo + pvi +
                  rci + hct, data = pre)
summary(fit.pre3)
vif(fit.pre3)
fit.pre4 <- glm(survive~  ht + sex + shock_type + sbp + map +
                   dbp + mcvp + bsi + ci +  mct + uo + pvi +
                  rci + hct, data = pre)
summary(fit.pre4)
vif(fit.pre4)
fit.pre5 <- glm(survive~  ht + sex + shock_type + sbp + map +
                  mcvp + bsi + ci +  mct + uo + pvi +
                  rci + hct, data = pre)
summary(fit.pre5)
vif(fit.pre5)
fit.pre6 <- glm(survive~  ht + sex + shock_type + map +
                  mcvp + bsi + ci +  mct + uo + pvi +
                  rci + hct, data = pre)
summary(fit.pre6)
vif(fit.pre6)

fit.post<- glm(survive~ age + ht + sex + shock_type + sbp + map +
                 hr + dbp + mcvp + bsi + ci + at + mct + uo + pvi +
                 rci + hg + hct, data = post)
summary(fit.post)
vif(fit.pre)

###train/test###
half <- nrow(data)/2
train <- data.frame(data[1:112,])
test <- data.frame(data[113:224,])

fit.train <- glm(survive ~ shock_type + map+
               mcvp+ bsi+ mct + uo + pvi +
               rci + hg , family = binomial(link = "logit"), data = train)
summary(fit.train)
vif(fit.train)
fit.test <- glm(survive ~ shock_type + map+
               mcvp+ bsi+ mct + uo + pvi +
               rci + hg , family = binomial(link = "logit"), data = test)
summary(fit.test)
vif(fit.test)

fit.t1 <- glm(survive ~ age + ht + sex + shock_type + sbp + map+
             hr + dbp + mcvp+ bsi+ ci+ at+ mct + uo + pvi +
             rci + hg + hct + record, family = binomial(link = "logit"), data = train)
summary(fit.t1)
vif(fit.t1)
fit.t2 <- glm(survive ~ age + ht + sex + shock_type + sbp + map+
                hr + dbp + mcvp+ bsi+ ci+ at+  uo + pvi +
                rci + hg + hct + record, family = binomial(link = "logit"), data = train)
summary(fit.t2)
vif(fit.t2)
fit.t3 <- glm(survive ~ age +  sex + shock_type + sbp + map+
                hr + dbp + mcvp+ bsi+ ci+ at+ uo + pvi +
                rci + hg + hct + record, family = binomial(link = "logit"), data = train)
summary(fit.t3)
vif(fit.t3)
fit.t4 <- glm(survive ~ age +  sex + shock_type + sbp + map+
                hr + dbp + mcvp+ bsi+ ci+ at+ uo + pvi +
                rci + hg + hct , family = binomial(link = "logit"), data = train)
summary(fit.t4)
vif(fit.t4)
fit.t5 <- glm(survive ~ age +  sex + shock_type + sbp + map+
                hr + dbp + mcvp+ bsi+ ci+ at+ uo + pvi +
                rci + hg  , family = binomial(link = "logit"), data = train)
summary(fit.t5)
vif(fit.t5)
fit.t6 <- glm(survive ~ age +  sex + shock_type + sbp + map+
                hr + dbp + mcvp+  ci+ at+ uo + pvi +
                rci + hg  , family = binomial(link = "logit"), data = train)
summary(fit.t6)
vif(fit.t6)
fit.t7 <- glm(survive ~ age +  sex + shock_type +  map+
                hr + dbp + mcvp+  ci+ at+ uo + pvi +
                rci + hg  , family = binomial(link = "logit"), data = train)
summary(fit.t7)
vif(fit.t7)

fit.t8 <- glm(survive ~ age +  sex + shock_type +  map+
                hr  + mcvp+  ci+ at+ uo + pvi +
                rci + hg  , family = binomial(link = "logit"), data = train)
summary(fit.t8)
vif(fit.t8)
fit.t9 <- glm(survive ~ age +  sex + shock_type +  map+
                hr  + mcvp+   at+ uo + pvi +
                rci + hg  , family = binomial(link = "logit"), data = train)
summary(fit.t9)##good enough
vif(fit.t9)
fit.t10 <- glm(survive ~ age +   shock_type +  map+
                hr  + mcvp+   at+ uo + pvi +
                rci + hg  , family = binomial(link = "logit"), data = train)
summary(fit.t10)
vif(fit.t10)
fit.t11 <- glm(survive ~   shock_type +  map+
                 hr  + mcvp+   at+ uo + pvi +
                 rci + hg  , family = binomial(link = "logit"), data = train)
summary(fit.t11)
vif(fit.t11)

fit.tt11 <- glm(survive ~   shock_type +  map+
                hr  + mcvp+   at+ uo + pvi +
                rci + hg  , family = binomial(link = "logit"), data = test) ##fit11
summary(fit.tt11)
vif(fit.tt11)
fit.tt9 <- glm(survive ~ age +  sex + shock_type +  map+
                 hr  + mcvp+   at+ uo + pvi +
                 rci + hg  , family = binomial(link = "logit"), data = test) #fit9
summary(fit.tt9)
vif(fit.tt9) ##good enough
fit.tt5 <-  glm(survive ~ age +  sex + shock_type + sbp + map+
                  hr + dbp + mcvp+ bsi+ ci+ at+ uo + pvi +
                  rci + hg  , family = binomial(link = "logit"), data = test) #fit5
summary(fit.tt5)
vif(fit.tt5)#doesn't work vif is too high
roc(survive, age +  sex + shock_type +  map+
      hr  + mcvp+   at+ uo + pvi +
      rci + hg, data = train, plot = T) ##fit.t9

roc(survive, age +  sex + shock_type +  map+
      hr  + mcvp+   at+ uo + pvi +
      rci + hg, data = test, plot = T) ##fit.tt9

roc(survive,    shock_type +  map+
      hr  + mcvp+   at+ uo + pvi +
      rci + hg , data = train, plot = T) #fit.t11
roc(survive,    shock_type +  map+
      hr  + mcvp+   at+ uo + pvi +
      rci + hg , data = test, plot = T) #fit.tt11
roc(survive ~ shock_type + map+
      mcvp+ bsi+ mct + uo + pvi +
      rci + hg, plot = T, data = test)

#multicollinearity measured by vif. 1-2 low, 2-5 moderate, 6-8 moderate, high 9+ high


fit.step <- glm(formula = survive ~ shock_type + map + mcvp + bsi + mct + 
uo + pvi + rci + hg, family = binomial(link = "logit"), data = data)
summary(fit.step)
vif(fit.step)
stepAIC(fit.step)

###ratio###
summary(fit11)
#shock_type
exp(0.337376)
exp(0.337376 +1.96*  0.136617)  
exp(0.337376 -1.96*  0.136617)  
#map
exp(-0.061592)
exp(-0.061592 +1.96*  0.011481) 
exp(-0.061592 -1.96*  0.011481) 
#mcvp 
exp(0.013513)
exp(0.013513   +1.96*0.004237 )  
exp(0.013513   -1.96*0.004237 )  
#bsi 
exp(-0.030317)
exp(-0.030317  +1.96* 0.012412)
exp(-0.030317  -1.96* 0.012412)
#mct 
exp(0.005163)
exp(0.005163  +1.96* 0.002435)  
exp(0.005163  -1.96* 0.002435)  
#uo 
exp(-0.014550)
exp(-0.014550   +1.96*0.004982)
exp(-0.014550   -1.96*0.004982)
#pvi    
exp(-0.005830)
exp(-0.005830   +1.96*0.001723)
exp(-0.005830   -1.96*0.001723)
#rci      
exp(-0.004586)
exp(-0.004586  +1.96* 0.002136)  
exp(-0.004586  -1.96* 0.002136)  
#hg 
exp(-0.028661 )
exp(-0.028661   +1.96*0.012094) 
exp(-0.028661   -1.96*0.012094) 

