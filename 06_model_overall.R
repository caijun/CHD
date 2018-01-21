rm(list = ls())

load("output/case_control_matched.rda")

library(tidyverse)

cases <- case.m %>% 
  mutate(CHD = 1) %>% 
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

### Table 1 ###
library(plyr)
# case和control年龄范围
ddply(mydata, ~ CHD, summarise, mean = round(mean(age.y), 2), sd = round(sd(age.y), 2))

# case和control性别
x <- xtabs(~ CHD + sex, data = mydata)
round(x / rowSums(x) * 100, 2)

# use conditional logitstic regression if matching has been done
library(survival)
mylogit <- clogit(CHD ~ sex + strata(pair.id), data = mydata)
summary(mylogit)

# case和control试管婴儿
x <- xtabs(~ CHD + tube.baby, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- clogit(CHD ~ tube.baby + strata(pair.id), data = mydata)
summary(mylogit)

# case和control母亲生子年龄
ddply(mydata, ~ CHD, summarise, mean = round(mean(M.production.age), 2), 
      sd = round(sd(M.production.age), 2))
mydata <- mydata %>%
  mutate(M.production.age = case_when(
    .$M.production.age < 20 ~ "<20",
    .$M.production.age > 30 ~ ">30",
    TRUE ~ "20~30"
  ))
mydata$M.production.age <- factor(mydata$M.production.age, levels = c("20~30", "<20", ">30"))

x <- xtabs(~ CHD + M.production.age, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- clogit(CHD ~ M.production.age + strata(pair.id), data = mydata)
summary(mylogit)

# case和control是否足月产
x <- xtabs(~ CHD + term, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$term <- factor(mydata$term, levels = c("1", "0"))
mylogit <- clogit(CHD ~ term + strata(pair.id), data = mydata)
summary(mylogit)

# case和control是否顺产
x <- xtabs(~ CHD + production.mode, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$production.mode <- factor(mydata$production.mode, levels = c("1", "0"))
mylogit <- clogit(CHD ~ production.mode + strata(pair.id), data = mydata)
summary(mylogit)

# case和control是否流产
mydata <- mydata %>%
  mutate(abortion = case_when(
    .$abortion > 0 ~ "1",
    TRUE ~ "0"
  ))
mydata$abortion <- factor(mydata$abortion, levels = c("0", "1"))
x <- xtabs(~ CHD + abortion, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- clogit(CHD ~ abortion + strata(pair.id), data = mydata)
summary(mylogit)

# case和control生产次数
x <- xtabs(~ CHD + parity, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$parity <- factor(mydata$parity)
mylogit <- clogit(CHD ~ parity + strata(pair.id), data = mydata)
summary(mylogit)

# case和control怀孕次数
mydata <- mydata %>%
  mutate(gravidity = case_when(
    .$gravidity >= 3 ~ ">=3",
    TRUE ~ as.character(gravidity)
  ))
mydata$gravidity <- factor(mydata$gravidity, levels = c("1", "2", ">=3"))
x <- xtabs(~ CHD + gravidity, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- clogit(CHD ~ gravidity + strata(pair.id), data = mydata)
summary(mylogit)

# case和control母亲教育水平
x <- xtabs(~ CHD + M.edu, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.edu <- factor(mydata$M.edu, levels = c("4", "3", "2", "1"))
mylogit <- clogit(CHD ~ M.edu + strata(pair.id), data = mydata)
summary(mylogit)

# case和control父亲生子年龄
ddply(mydata, ~ CHD, summarise, mean = round(mean(F.production.age), 2), 
      sd = round(sd(F.production.age), 2))
# 中国男性法定结婚年龄不早于22周岁
mydata <- mydata %>%
  mutate(F.production.age = case_when(
    .$F.production.age < 22 ~ "<22",
    .$F.production.age > 30 ~ ">30",
    TRUE ~ "22~30"
  ))
mydata$F.production.age <- factor(mydata$F.production.age, levels = c("22~30", "<22", ">30"))

x <- xtabs(~ CHD + F.production.age, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- clogit(CHD ~ F.production.age + strata(pair.id), data = mydata)
summary(mylogit)

# case和control父亲教育水平
x <- xtabs(~ CHD + F.edu, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$F.edu <- factor(mydata$F.edu, levels = c("4", "3", "2", "1"))
mylogit <- clogit(CHD ~ F.edu + strata(pair.id), data = mydata)
summary(mylogit)

# correlation between maternal and paternal production ages
# since both maternal and paternal production ages are nominal variables, we use Cramer's V to measure their association.
library(vcd)
hist(cases$M.production.age)
hist(cases$F.production.age)
summary(cases$M.production.age)
summary(cases$F.production.age)
hist(cases$F.production.age - cases$M.production.age)
summary(cases$F.production.age - cases$M.production.age)

cor.test(cases$M.production.age, cases$F.production.age)
cor.test(controls$M.production.age, controls$F.production.age)
plot(cases$M.production.age, cases$F.production.age)

mydata1 <- data.frame(M.production.age = mydata$M.production.age, 
                      F.production.age = mydata$F.production.age)
x <- xtabs(~ M.production.age + F.production.age, data = mydata1)
assocstats(x)

cor.test(cases$M.edu, cases$F.edu)
cor.test(controls$M.edu, controls$F.edu)
plot(cases$M.edu, cases$F.edu)

mydata1 <- data.frame(M.edu = mydata$M.edu, 
                      F.edu = mydata$F.edu)
x <- xtabs(~ M.edu + F.edu, data = mydata1)
assocstats(x)
### Table 1 ###


### Table 2 ###
# case和control母亲是否接触有毒物质
x <- xtabs(~ CHD + M.toxic.exposure, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.toxic.exposure <- factor(mydata$M.toxic.exposure)
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
### Table 2 ###



### Table 3 ###
# case和control母亲孕期是否感冒
x <- xtabs(~ CHD + M.pregnancy.flu, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.flu <- factor(mydata$M.pregnancy.flu)
mylogit <- glm(CHD ~ M.pregnancy.flu, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(CHD ~ M.pregnancy.flu + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲孕期感冒时期:月
mydata <- mydata %>%
  mutate(M.pregnancy.flu.time = case_when(
    .$M.pregnancy.flu.time == 0 ~ "NONE",
    .$M.pregnancy.flu.time %in% c(1, 2, 3) ~ "Early",
    .$M.pregnancy.flu.time %in% c(4, 5, 6) ~ "Middle",
    .$M.pregnancy.flu.time %in% c(7, 8, 9, 10) ~ "Late"
  ))
mydata$M.pregnancy.flu.time <- factor(mydata$M.pregnancy.flu.time, 
                                      levels = c("NONE", "Early", "Middle", "Late"))

x <- xtabs(~ CHD + M.pregnancy.flu.time, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(CHD ~ M.pregnancy.flu.time, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(CHD ~ M.pregnancy.flu.time + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲有无孕期合并症
x <- xtabs(~ CHD + M.pregnancy.complication, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.complication <- factor(mydata$M.pregnancy.complication)
mylogit <- glm(CHD ~ M.pregnancy.complication, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(CHD ~ M.pregnancy.complication + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲孕期是否用药
x <- xtabs(~ CHD + M.pregnancy.med, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.med <- factor(mydata$M.pregnancy.med)
mylogit <- glm(CHD ~ M.pregnancy.med, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(CHD ~ M.pregnancy.med + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))

# case和control母亲孕期用药时期:月
mydata <- mydata %>%
  mutate(M.pregnancy.med.time = case_when(
    .$M.pregnancy.med.time == 0 ~ "NONE",
    .$M.pregnancy.med.time %in% c(1, 2, 3) ~ "Early",
    .$M.pregnancy.med.time %in% c(4, 5, 6) ~ "Middle",
    .$M.pregnancy.med.time %in% c(7, 8, 9, 10) ~ "Late"
  ))
mydata$M.pregnancy.med.time <- factor(mydata$M.pregnancy.med.time, 
                                      levels = c("NONE", "Early", "Middle", "Late"))

x <- xtabs(~ CHD + M.pregnancy.med.time, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(CHD ~ M.pregnancy.med.time, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(CHD ~ M.pregnancy.med.time + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲孕期使用何种药物
x <- xtabs(~ CHD + M.pregnancy.med.name, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.med.name <- factor(mydata$M.pregnancy.med.name, 
                                      levels = c("NONE", "Antibiotics", "Diet pill", "Antidepressant", 
                                                 "Antitumor drug", "Tocolytic agent", "Analgesics", 
                                                 "Tranquilizer"))
mylogit <- glm(CHD ~ M.pregnancy.med.name, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(CHD ~ M.pregnancy.med.name + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲孕前1-3月是否口服避孕药
x <- xtabs(~ CHD + M.oral.contraceptive, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.oral.contraceptive <- factor(mydata$M.oral.contraceptive)
mylogit <- glm(CHD ~ M.oral.contraceptive, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(CHD ~ M.oral.contraceptive + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲孕期是否使用叶酸
x <- xtabs(~ CHD + M.pregnancy.folic.acid, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.folic.acid <- factor(mydata$M.pregnancy.folic.acid)
mylogit <- glm(CHD ~ M.pregnancy.folic.acid, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(CHD ~ M.pregnancy.folic.acid + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)
### Table 3 ###



# multivariate logistic regression for Table 2
mylogit1 <- glm(CHD ~ M.toxic.exposure + M.radioactive.exposure + M.smoke + M.smoked.years + M.smoke.freq + M.pregnancy.smoke + M.pregnancy.passive.smoke + F.toxic.exposure + F.radioactive.exposure + F.smoke + F.smoked.years + F.smoke.freq + M.drink + F.drink + decoration + HV.cable + chemical.plant, data = mydata, family = "binomial")
summary(mylogit1)
## adjusted odds ratios and 95% CI
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# remove M.smoke, F.smoke
mylogit1.1 <- glm(CHD ~ M.toxic.exposure + M.radioactive.exposure + M.smoked.years + M.smoke.freq + M.pregnancy.smoke + M.pregnancy.passive.smoke + F.toxic.exposure + F.radioactive.exposure + F.smoked.years + F.smoke.freq + M.drink + F.drink + decoration + HV.cable + chemical.plant, data = mydata, family = "binomial")
summary(mylogit1.1)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit1.1), confint(mylogit1.1)))

cor(mydata$M.smoke.freq, mydata$M.smoked.years)
# remove M.smoke.freq, F.smoke.freq
mylogit1.2 <- glm(CHD ~ M.toxic.exposure + M.radioactive.exposure + M.smoked.years + M.pregnancy.smoke + M.pregnancy.passive.smoke + F.toxic.exposure + F.radioactive.exposure + F.smoked.years + M.drink + F.drink + decoration + HV.cable + chemical.plant, data = mydata, family = "binomial")
summary(mylogit1.2)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit1.2), confint(mylogit1.2)))
# examine collinearity
vif(mylogit1.2)

# remove M.smoked.years, F.smoked.years
mylogit1.3 <- glm(CHD ~ M.toxic.exposure + M.radioactive.exposure + M.smoke.freq + M.pregnancy.smoke + M.pregnancy.passive.smoke + F.toxic.exposure + F.radioactive.exposure + F.smoke.freq + M.drink + F.drink + decoration + HV.cable + chemical.plant, data = mydata, family = "binomial")
summary(mylogit1.3)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit1.3), confint(mylogit1.3)))

# remove M.smoke.freq, F.smoke.freq, M.smoked.years, F.smoked.years
mylogit1.4 <- glm(CHD ~ M.toxic.exposure + M.radioactive.exposure + M.pregnancy.smoke + M.pregnancy.passive.smoke + F.toxic.exposure + F.radioactive.exposure + F.smoke + M.drink + F.drink + decoration + HV.cable + chemical.plant, data = mydata, family = "binomial")
summary(mylogit1.4)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit1.4), confint(mylogit1.4)))


# multivariate logistic regression for Table 3
mylogit2 <- glm(CHD ~ M.pregnancy.flu + M.pregnancy.flu.time + M.pregnancy.complication + M.pregnancy.med + M.pregnancy.med.time + M.pregnancy.med.name + M.oral.contraceptive + M.pregnancy.folic.acid, data = mydata, family = "binomial")
summary(mylogit2)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit2), confint(mylogit2)))


# remove M.pregnancy.flu, M.pregnancy.med
mylogit2.1 <- glm(CHD ~ M.pregnancy.flu.time + M.pregnancy.complication + M.pregnancy.med.time + M.pregnancy.med.name + M.oral.contraceptive + M.pregnancy.folic.acid, data = mydata, family = "binomial")
summary(mylogit2.1)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit2.1), confint(mylogit2.1)))


# multivariate logistic regression
mylogit3 <- glm(CHD ~ M.toxic.exposure + M.radioactive.exposure + M.smoke + M.smoked.years + M.smoke.freq + M.pregnancy.smoke + M.pregnancy.passive.smoke + F.toxic.exposure + F.radioactive.exposure + F.smoke + F.smoked.years + F.smoke.freq + M.drink + F.drink + decoration + HV.cable + chemical.plant + M.pregnancy.flu + M.pregnancy.flu.time + M.pregnancy.complication + M.pregnancy.med + M.pregnancy.med.time + M.pregnancy.med.name + M.oral.contraceptive + M.pregnancy.folic.acid, data = mydata, family = "binomial")
summary(mylogit3)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit3), confint(mylogit3)))
