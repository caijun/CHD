rm(list = ls())

load("output/case_control_matched.rda")

library(tidyverse)

# subtypes: VSD, ASD, PDA
subtype <- "PDA"

if (subtype %in% c("VSD", "ASD", "PDA")) {
  case.m <- case.m %>% 
    mutate(CHD = ifelse(grepl(subtype, ignore.case = TRUE, diagnosis), 1, 0))
}

cases <- case.m %>% 
  filter(CHD == 1) %>% 
  select(pair.id, CHD, age.y, sex, tube.baby, M.production.age, term, production.mode, 
         abortion, parity, gravidity, M.edu, F.production.age, F.edu, 
         M.toxic.exposure, M.radioactive.exposure, M.smoke, M.smoked.years, M.smoke.freq, 
         M.pregnancy.smoke, 
         M.pregnancy.passive.smoke, F.toxic.exposure, F.radioactive.exposure, 
         F.smoke, F.smoked.years, F.smoke.freq, 
         M.drink, F.drink, decoration, HV.cable, chemical.plant, 
         M.pregnancy.flu, M.pregnancy.flu.time, 
         M.pregnancy.complication, M.pregnancy.med, 
         M.pregnancy.med.time, M.pregnancy.med.name, M.pregnancy.folic.acid, 
         M.oral.contraceptive) %>% 
  arrange(pair.id)

controls <- control.m %>% 
  filter(pair.id %in% cases$pair.id) %>%
  mutate(CHD = 0) %>% 
  select(pair.id, CHD, age.y, sex, tube.baby, M.production.age, term, production.mode, 
         abortion, parity, gravidity, M.edu, F.production.age, F.edu, 
         M.toxic.exposure, M.radioactive.exposure, M.smoke, M.smoked.years, M.smoke.freq, 
         M.pregnancy.smoke, 
         M.pregnancy.passive.smoke, F.toxic.exposure, F.radioactive.exposure, 
         F.smoke, F.smoked.years, F.smoke.freq, 
         M.drink, F.drink, decoration, HV.cable, chemical.plant, 
         M.pregnancy.flu, M.pregnancy.flu.time, 
         M.pregnancy.complication, M.pregnancy.med, 
         M.pregnancy.med.time, M.pregnancy.med.name, M.pregnancy.folic.acid, 
         M.oral.contraceptive) %>% 
  arrange(pair.id)

mydata <- rbind(cases, controls)

# Table 2 ----------------------------------------------------------------------
# case和control母亲是否接触有毒物质
x <- xtabs(~ CHD + M.toxic.exposure, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.toxic.exposure <- factor(mydata$M.toxic.exposure)
# use conditional logitstic regression if matching has been done
library(survival)
mylogit <- clogit(CHD ~ M.toxic.exposure + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
# examine the collinearity of variables
library(car)
vif(mylogit)

# case和control母亲是否接触放射性物质
x <- xtabs(~ CHD + M.radioactive.exposure, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.radioactive.exposure <- factor(mydata$M.radioactive.exposure)
mylogit <- clogit(CHD ~ M.radioactive.exposure + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control母亲是否吸烟
x <- xtabs(~ CHD + M.smoke, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.smoke <- factor(mydata$M.smoke)
mylogit <- clogit(CHD ~ M.smoke + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control母亲烟龄
library(plyr)
ddply(mydata[mydata$M.smoked.years > 0, ], ~ CHD, summarise, 
      mean = round(mean(M.smoked.years), 2), sd = round(sd(M.smoked.years), 2))
mydata <- mydata %>%
  mutate(M.smoked.years = case_when(
    .$M.smoked.years == 0 ~ "NONE",
    .$M.smoked.years <= 10 ~ "<=10",
    .$M.smoked.years > 10 ~ ">10"
  ))
mydata$M.smoked.years <- factor(mydata$M.smoked.years, 
                                levels = c("NONE", "<=10", ">10"))

x <- xtabs(~ CHD + M.smoked.years, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- clogit(CHD ~ M.smoked.years + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control母亲每天吸烟支数
mydata <- mydata %>%
  mutate(M.smoke.freq = case_when(
    .$M.smoke.freq == "0" ~ "NONE",
    .$M.smoke.freq %in% c("1", "2", "3", "4", "5") ~ "<=5",
    .$M.smoke.freq %in% c("6", "7", "8", "9", ">=10") ~ ">5"
  ))
mydata$M.smoke.freq <- factor(mydata$M.smoke.freq, 
                              levels = c("NONE", "<=5", ">5"))

x <- xtabs(~ CHD + M.smoke.freq, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- clogit(CHD ~ M.smoke.freq + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control母亲孕期是否吸烟
x <- xtabs(~ CHD + M.pregnancy.smoke, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.smoke <- factor(mydata$M.pregnancy.smoke)
mylogit <- clogit(CHD ~ M.pregnancy.smoke + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control母亲孕期是否在二手烟环境
x <- xtabs(~ CHD + M.pregnancy.passive.smoke, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.passive.smoke <- factor(mydata$M.pregnancy.passive.smoke)
mylogit <- clogit(CHD ~ M.pregnancy.passive.smoke + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control父亲是否接触有毒物质
x <- xtabs(~ CHD + F.toxic.exposure, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$F.toxic.exposure <- factor(mydata$F.toxic.exposure)
mylogit <- clogit(CHD ~ F.toxic.exposure + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control父亲是否接触放射性物质
x <- xtabs(~ CHD + F.radioactive.exposure, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$F.radioactive.exposure <- factor(mydata$F.radioactive.exposure)
mylogit <- clogit(CHD ~ F.radioactive.exposure + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control父亲是否吸烟
x <- xtabs(~ CHD + F.smoke, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$F.smoke <- factor(mydata$F.smoke)
mylogit <- clogit(CHD ~ F.smoke + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control父亲烟龄
ddply(mydata[mydata$F.smoked.years != 0, ], ~ CHD, summarise, 
      mean = round(mean(F.smoked.years), 2), sd = round(sd(F.smoked.years), 2))
mydata <- mydata %>%
  mutate(F.smoked.years = case_when(
    .$F.smoked.years == 0 ~ "NONE",
    .$F.smoked.years <= 10 ~ "<=10",
    .$F.smoked.years > 10 ~ ">10"
  ))
mydata$F.smoked.years <- factor(mydata$F.smoked.years, 
                                levels = c("NONE", "<=10", ">10"))

x <- xtabs(~ CHD + F.smoked.years, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- clogit(CHD ~ F.smoked.years + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control父亲每天吸烟支数
table(mydata$F.smoke.freq, useNA = "ifany")
mydata <- mydata %>%
  mutate(F.smoke.freq = case_when(
    .$F.smoke.freq == "0" ~ "NONE",
    .$F.smoke.freq %in% c("1", "2", "3", "4", "5") ~ "<=5",
    .$F.smoke.freq %in% c("6", "7", "8", "9", ">=10") ~ ">5"
  ))
mydata$F.smoke.freq <- factor(mydata$F.smoke.freq, 
                              levels = c("NONE", "<=5", ">5"))

x <- xtabs(~ CHD + F.smoke.freq, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- clogit(CHD ~ F.smoke.freq + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control母亲是否喝酒
x <- xtabs(~ CHD + M.drink, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.drink <- factor(mydata$M.drink)
mylogit <- clogit(CHD ~ M.drink + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control父亲是否喝酒
x <- xtabs(~ CHD + F.drink, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$F.drink <- factor(mydata$F.drink)
mylogit <- clogit(CHD ~ F.drink + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control房屋是否装修
x <- xtabs(~ CHD + decoration, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$decoration <- factor(mydata$decoration)
mylogit <- clogit(CHD ~ decoration + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control房屋附近是否有高压电缆
x <- xtabs(~ CHD + HV.cable, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$HV.cable <- factor(mydata$HV.cable)
mylogit <- clogit(CHD ~ HV.cable + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)

# case和control房屋附近是否有化工厂
x <- xtabs(~ CHD + chemical.plant, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$chemical.plant <- factor(mydata$chemical.plant)
mylogit <- clogit(CHD ~ chemical.plant + M.production.age + parity + 
                    gravidity + M.edu + strata(pair.id), data = mydata)
summary(mylogit)
vif(mylogit)
