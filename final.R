library(readxl)  # To read xlsx
library(readr)   # Fast csv write
library(MASS) #stepAIC
library(car) #vif
library(xtable)

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

#eda#

hist(age)
hist(ht)
hist(sbp)
hist(pre$sbp)
hist(map)
hist(pre$map)
hist(hr)
hist(pre$hr)
hist(dbp)
hist(pre$dbp)
hist(mcvp)
hist(pre$mcvp)
hist(log(mcvp))
hist(bsi)
hist(pre$bsi)
hist(ci)
hist(pre$ci)
hist(log(ci))
hist(at)
hist(pre$at)
hist(log(at)) #normalize
hist(mct)
hist(pre$mct)
hist(log(mct))
hist(uo)
hist(pre$uo)
hist(log(uo))
hist(pvi)
hist(pre$pvi)
hist(log(pvi))#normalize
hist(log(pre$pvi))
hist(rci)
hist(pre$rci)
hist(log(rci))
hist(hg)
hist(pre$hg)
hist(hct)
hist(pre$hct)


model <- glm(survive ~ age + ht + sex + shock_type + sbp + map+
               hr + dbp + mcvp+ bsi+ ci+ at+ mct + uo + pvi +
               rci + hg + hct , family = binomial(link = "logit"), data = pre)
summary(model)
stepAIC(model)
final <- glm(formula = survive ~ shock_type + map + mcvp + bsi + uo + 
               rci, family = binomial(link = "logit"), data = pre)
summary(final)
coef(final)
exp(coef(final))
exp(final$coefficients)
exp(confint(final))
xtable(exp(confint(final)))


threshold <- 43/112 
predict.final <- ifelse(predict(final,type = "response")>threshold,1,0)
actual.final <- final$y
confused <- table(predict.final,actual.final)
confused
xtable(confused)

cont <- data.frame(age, ht, sbp,map,hr,dbp,mcvp,bsi,ci,at,mct,uo,pvi,rci,hg,hct)
corrplot(cor(cont))
xtable(corrplot(cor(cont)))

final.cont <- data.frame(map,bsi,uo,mcvp,rci)
corrplot(cor(final.cont))
####
one.fourth.root=function(x){
  x^0.25
}
source("examine.logistic.reg.R")
# Consider model 
dat.glm <- glm(formula = survive ~ shock_type + map + mcvp + bsi + uo + 
                 rci, family = binomial, data = pre)
dat.mf <- model.frame(dat.glm)
## Covariate pattern: too many EVPs!
w <- aggregate(formula = survive ~ shock_type + map + mcvp + bsi + uo + 
                 rci, data = pre, FUN = sum)
n <- aggregate(formula = survive ~ shock_type + map + mcvp + bsi + uo + 
                 rci, data = pre, FUN = length)
w.n <- data.frame(w, trials = n$survive, prop = round(w$survive/n$survive,2))
dim(w.n)
#[1] 112 9

# Create EVPs by binning continuous covariates
g = 8 # number of categories
mcvp_interval = cut(mcvp, quantile(mcvp, 0:g/g), include.lowest = TRUE)  # Creates factor with levels 1,2,...,g
levels(mcvp_interval)

# Diagnostic plots
w <- aggregate(formula = survive ~ shock_type + map + mcvp + bsi + uo + 
                 rci, data = pre, FUN = sum)
n <- aggregate(formula = survive ~ shock_type + map + mcvp + bsi + uo + 
                 rci, data = pre, FUN = length)
w.n <- data.frame(w, trials = n$survive, prop = round(w$survive/n$survive,2))
mod.prelim1 <- glm(formula = survive/trials ~ mcvp+rci+map+shock_type+bsi,
                   family = binomial(link = logit), data = w.n, weights = trials)
save1 = examine.logistic.reg(mod.prelim1, identify.points=T, scale.n=one.fourth.root, scale.cookd=sqrt)

# Evaluation of EVPs for potential outlying sets of points
w.n.diag1 = data.frame(w.n, pi.hat=round(save1$pi.hat, 2), std.res=round(save1$stand.resid, 2), 
                       cookd=round(save1$cookd, 2), h=round(save1$h, 2))
p = length(mod.prelim1$coef) # number of parameters in model (# coefficients)
ck.out = abs(w.n.diag1$std.res)>2 | w.n.diag1$cookd>4/nrow(w.n) | w.n.diag1$h > 3*p/nrow(w.n)
extract.EVPs = w.n.diag1[ck.out, ]
extract.EVPs

#HL test
source("HLTest.R")
HLTest(mod.prelim1, 3)
