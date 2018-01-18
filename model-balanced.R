rm(list = ls())
setwd("~/Downloads/FengYu/")

library(tidyverse)

case <- readRDS("case.rds")
names(case)
case <- case %>%
  dplyr::select(-residence, -id, -blood.type, -age, -surgery.date, -gravidity.parity, 
                -M.smoke, -M.smoke.start.age, -M.pregnancy.flu, -M.pregnancy.med, 
                -F.smoke, -F.smoke.start.age, -(fullname:county), -(year:day), -age.y) %>%
  mutate(diagnosis = 1) %>%
  filter(!is.na(sex)) %>%   # 92 cases with NA sex info
  filter(M.smoked.years >= 0) %>% # 1 case whose M.production.age is less than M.smoke.start.age
  dplyr::select(diagnosis, sex, term:F.smoked.years)
which(is.na(case), arr.ind = TRUE)

control <- readRDS("control.rds")
names(control)
control <- control %>%
  dplyr::select(-residence, -id, -input.date, -age, -gravidity.parity, -M.smoke, 
                -M.smoke.start.age, -M.pregnancy.flu, -M.pregnancy.med, -F.smoke, 
                -F.smoke.start.age, -(fullname:county), -(year:day), -age.y) %>%
  mutate(diagnosis = 0) %>%
  filter(!is.na(F.drink)) %>% # 1 case with NA F.drink:IR.congenital.disease info
  dplyr::select(diagnosis, sex:F.smoked.years)
which(is.na(control), arr.ind = TRUE)

setdiff(names(case), names(control))

set.seed(8506)
# # sample case with the amount of control
# idx <- sample.int(nrow(case), nrow(control))
# data <- rbind(case[idx, ], control)
# full dataset
data <- rbind(case, control)
names(data)

data <- data %>%
  mutate(M.smoke.freq = case_when(
    .$M.smoke.freq %in% c("0") ~ "NONE",
    .$M.smoke.freq %in% c("1", "2", "3") ~ "1~3",
    .$M.smoke.freq %in% c("4", "5", "6") ~ "4~6",
    .$M.smoke.freq %in% c("7", "8", "9") ~ "7~9", 
    .$M.smoke.freq == ">=10" ~ ">=10"
  ), M.pregnancy.flu.time = as.character(M.pregnancy.flu.time), 
  M.pregnancy.flu.time = case_when(
    .$M.pregnancy.flu.time %in% c("0") ~ "NONE",
    .$M.pregnancy.flu.time %in% c("1", "2", "3") ~ "1st", 
    .$M.pregnancy.flu.time %in% c("4", "5", "6") ~ "2nd", 
    .$M.pregnancy.flu.time %in% c("7", "8", "9", "10") ~ "3rd"
  ), M.pregnancy.med.time = as.character(M.pregnancy.med.time),
  M.pregnancy.med.time = case_when(
    .$M.pregnancy.med.time %in% c("0") ~ "NONE",
    .$M.pregnancy.med.time %in% c("1", "2", "3") ~ "1st", 
    .$M.pregnancy.med.time %in% c("4", "5", "6") ~ "2nd", 
    .$M.pregnancy.med.time %in% c("7", "8", "9", "10") ~ "3rd"
  ), F.smoke.freq = case_when(
    .$F.smoke.freq %in% c("0") ~ "NONE",
    .$F.smoke.freq %in% c("1", "2", "3") ~ "1~3",
    .$F.smoke.freq %in% c("4", "5", "6") ~ "4~6",
    .$F.smoke.freq %in% c("7", "8", "9") ~ "7~9", 
    .$F.smoke.freq == ">=10" ~ ">=10"
  ))

# using propensity score matching to avoid the problem of data imbalance
# http://pareonline.net/pdf/v19n18.pdf
library(MatchIt)
data <- data %>%
  mutate(diagnosis = case_when(
    .$diagnosis == 1 ~ 0,
    .$diagnosis == 0 ~ 1
  ))
m.out <- matchit(diagnosis ~ sex,
                 data = data, method = "nearest", ratio = 1)
summary(m.out)
# plot(m.out, type = "jitter")
plot(m.out, type = "hist")

data <- match.data(m.out)
data <- data %>%
  dplyr::select(-distance, -weights) %>%
  mutate(diagnosis = case_when(
    .$diagnosis == 0 ~ 1,
    .$diagnosis == 1 ~ 0
  ))

diagnosis <- data$diagnosis
sex <- factor(data$sex)
term <- factor(data$term)
tube.baby <- factor(data$tube.baby)
production.mode <- factor(data$production.mode)
M.edu <- factor(data$M.edu)
M.toxic.exposure <- factor(data$M.toxic.exposure)
M.radioactive.exposure <- factor(data$M.radioactive.exposure)
# ordinal variable
M.smoke.freq <- factor(data$M.smoke.freq, 
                       levels = c("NONE", "1~3", "4~6", "7~9", ">=10"))
M.pregnancy.smoke <- factor(data$M.pregnancy.smoke)
M.pregnancy.2nd.hand.smoke <- factor(data$M.pregnancy.2nd.hand.smoke)
M.drink <- factor(data$M.drink)
M.production.age <- data$M.production.age
M.pregnancy.complication <- factor(data$M.pregnancy.complication)
M.pregnancy.flu.time <- factor(data$M.pregnancy.flu.time, 
                               levels = c("NONE", "1st", "2nd", "3rd"))
M.pregnancy.med.time <- factor(data$M.pregnancy.med.time, 
                               levels = c("NONE", "1st", "2nd", "3rd"))
M.pregnancy.med.name <- factor(data$M.pregnancy.med.name, 
                               levels = c("NONE", "Analgesics", "Antibiotics", 
                                          "Antidepressant","Antitumor drug", 
                                          "Diet pill",  "Tocolytic agent", 
                                          "Tranquilizer"))
M.oral.contraceptive <- factor(data$M.oral.contraceptive)
F.production.age <- data$F.production.age
F.edu <- factor(data$F.edu)
F.toxic.exposure <- factor(data$F.toxic.exposure)
F.radioactive.exposure <- factor(data$F.radioactive.exposure)
F.smoke.freq <- factor(data$F.smoke.freq, 
                       levels = c("NONE", "1~3", "4~6", "7~9", ">=10"))
F.drink <- factor(data$F.drink)
decoration <- factor(data$decoration)
HV.cable <- factor(data$HV.cable)
chemical.plant <- factor(data$chemical.plant)
IR.cardiopathy <- factor(data$IR.cardiopathy)
IR.CHD <- factor(data$IR.CHD)
IR.congenital.disease <- factor(data$IR.congenital.disease)
gravidity <- factor(data$gravidity)
parity <- factor(data$parity)
abortion <- factor(data$abortion)
M.smoked.years <- data$F.smoked.years
F.smoked.years <- data$F.smoked.years
xfactors <- model.matrix(diagnosis ~ sex + term + tube.baby + production.mode + 
                           M.edu + M.toxic.exposure + M.radioactive.exposure + 
                           M.smoke.freq + M.pregnancy.smoke + M.pregnancy.2nd.hand.smoke + 
                           M.drink + M.pregnancy.complication + M.pregnancy.flu.time + 
                           M.pregnancy.med.time + M.pregnancy.med.name + 
                           M.oral.contraceptive + F.edu + F.toxic.exposure + 
                           F.radioactive.exposure + F.smoke.freq + F.drink + decoration + 
                           HV.cable + chemical.plant + IR.cardiopathy + IR.CHD + 
                           IR.congenital.disease + gravidity + parity + abortion)[, -1]
x <- as.matrix(data.frame(xfactors, M.production.age, F.production.age, 
                          M.smoked.years, F.smoked.years))


# logistic regression
# http://www.ats.ucla.edu/stat/r/dae/logit.htm
mydata <- data.frame(diagnosis, x)
logitmod <- glm(diagnosis ~ ., data = mydata, family = "binomial")
summary(logitmod)

logitmod1 <- step(logitmod, direction = "both")
summary(logitmod1)

save(logitmod1, file = "logitmod1-balanced.rda")


# partial least squares logistic regression
library(plsRglm)

cv.modpls <- cv.plsRglm(dataY = diagnosis, dataX = x, nt = 10, 
                        modele = "pls-glm-logistic", K = 8)
res.cv.modpls <- cvtable(summary(cv.modpls, MClassed = TRUE))

res10 <- plsRglm(dataY = diagnosis, dataX = x, nt = 10, 
                 modele = "pls-glm-logistic", pvals.expli = TRUE)
colSums(res10$pvalstep)

modpls2 <- plsRglm(dataY = diagnosis, dataX = x, nt = 10, 
                   modele = "pls-glm-logistic", sparse = TRUE, 
                   sparseStop = TRUE)

set.seed(123)
cv.modpls.logit <- cv.plsRglm(dataY = diagnosis, dataX = x, nt = 10, 
                              modele = "pls-glm-logistic", K = 8, NK = 100)
res.cv.modpls.logit <- cvtable(summary(cv.modpls.logit, MClassed = TRUE))
plot(res.cv.modpls.logit)

res <- plsRglm(dataY = diagnosis, dataX = x, nt = 2, modele = "pls-glm-logistic", 
               pvals.expli = TRUE)
res$wwetoile
biplot(res$tt,res$pp)
res$Std.Coeffs

modpls3 <- plsRglm(dataY = diagnosis, dataX = x, nt = 10, modele = "pls-glm-logistic", 
                   sparse = FALSE, sparseStop = TRUE)
modpls4 <- plsRglm(dataY = diagnosis, dataX = x, nt = 10, modele = "pls-glm-logistic", 
                   sparse = TRUE, sparseStop = FALSE)

CHD.bootYX2 <- bootplsglm(res, typeboot = "plsmodel", R = 10000)
boxplots.bootpls(CHD.bootYX2, las = 2, mar = c(8, 2, 1, 1) + 0.1)
temp.ci <- confints.bootpls(CHD.bootYX2)
plots.confints.bootpls(temp.ci, typeIC = "BCa", colIC = c("blue", "blue", "blue", "blue"),
                       legendpos = "topright", las = 2, mar = c(5, 2, 1, 1) + 0.1)
save(CHD.bootYX2, temp.ci, file = "model-balanced.rda")