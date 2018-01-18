rm(list = ls())
setwd("~/Downloads/FengYu/")

library(tidyverse)

load("dat/case_control_matched.rda")

case.m$TOF <- ifelse(grepl("TOF", ignore.case = TRUE, case.m$diagnosis), 1, 0)
case.m1 <- subset(case.m, TOF == 1)
control.m1 <- subset(control.m, id %in% id.lookup1[id.lookup1$case.id %in% case.m1$id, "control.id"])
control.m1$TOF <- 0

case.m <- case.m1
control.m <- control.m1
TOF <- c(case.m$TOF, control.m$TOF)
sex <- c(case.m$sex, control.m$sex)
age.y <- c(case.m$age.y, control.m$age.y)
tube.baby <- c(case.m$tube.baby, control.m$tube.baby)
M.production.age <- c(case.m$M.production.age, control.m$M.production.age)
term <- c(case.m$term, control.m$term)
production.mode <- c(case.m$production.mode, control.m$production.mode)
abortion <- c(case.m$abortion, control.m$abortion)
parity <- c(case.m$parity, control.m$parity)
gravidity <- c(case.m$gravidity, control.m$gravidity)
M.edu <- c(case.m$M.edu, control.m$M.edu)
F.production.age <- c(case.m$F.production.age, control.m$F.production.age)
F.edu <- c(case.m$F.edu, control.m$F.edu)
M.toxic.exposure <- c(case.m$M.toxic.exposure, control.m$M.toxic.exposure)
M.radioactive.exposure <- c(case.m$M.radioactive.exposure, control.m$M.radioactive.exposure)
M.smoke <- c(case.m$M.smoke, control.m$M.smoke)
M.smoked.years <- c(case.m$M.smoked.years, control.m$M.smoked.years)
M.smoke.freq <- c(case.m$M.smoke.freq, control.m$M.smoke.freq)
M.pregnancy.smoke <- c(case.m$M.pregnancy.smoke, control.m$M.pregnancy.smoke)
M.pregnancy.passive.smoke <- c(case.m$M.pregnancy.passive.smoke, control.m$M.pregnancy.passive.smoke)
F.toxic.exposure <- c(case.m$F.toxic.exposure, control.m$F.toxic.exposure)
F.radioactive.exposure <- c(case.m$F.radioactive.exposure, control.m$F.radioactive.exposure)
F.smoke <- c(case.m$F.smoke, control.m$F.smoke)
F.smoked.years <- c(case.m$F.smoked.years, control.m$F.smoked.years)
F.smoke.freq <- c(case.m$F.smoke.freq, control.m$F.smoke.freq)
M.drink <- c(case.m$M.drink, control.m$M.drink)
F.drink <- c(case.m$F.drink, control.m$F.drink)
decoration <- c(case.m$decoration, control.m$decoration)
HV.cable <- c(case.m$HV.cable, control.m$HV.cable)
chemical.plant <- c(case.m$chemical.plant, control.m$chemical.plant)
M.pregnancy.flu <- c(case.m$M.pregnancy.flu, control.m$M.pregnancy.flu)
M.pregnancy.flu.time <- c(case.m$M.pregnancy.flu.time, control.m$M.pregnancy.flu.time)
M.pregnancy.complication <- c(case.m$M.pregnancy.complication, control.m$M.pregnancy.complication)
M.pregnancy.med <- c(case.m$M.pregnancy.med, control.m$M.pregnancy.med)
M.pregnancy.med.time <- c(case.m$M.pregnancy.med.time, control.m$M.pregnancy.med.time)
M.pregnancy.med.name <- c(case.m$M.pregnancy.med.name, control.m$M.pregnancy.med.name)
M.pregnancy.folic.acid <- c(case.m$M.pregnancy.folic.acid, control.m$M.pregnancy.folic.acid)
M.oral.contraceptive <- c(case.m$M.oral.contraceptive, control.m$M.oral.contraceptive)

mydata <- data.frame(TOF, age.y, sex, tube.baby, M.production.age, term, production.mode, 
                     abortion, parity, gravidity, M.edu, F.production.age, F.edu, 
                     M.toxic.exposure, M.radioactive.exposure, M.smoke, M.smoked.years, M.smoke.freq, 
                     M.pregnancy.smoke, 
                     M.pregnancy.passive.smoke, F.toxic.exposure, F.radioactive.exposure, 
                     F.smoke, F.smoked.years, F.smoke.freq, 
                     M.drink, F.drink, decoration, HV.cable, chemical.plant, 
                     M.pregnancy.flu, M.pregnancy.flu.time, 
                     M.pregnancy.complication, M.pregnancy.med, 
                     M.pregnancy.med.time, M.pregnancy.med.name, M.pregnancy.folic.acid, 
                     M.oral.contraceptive)

### Table 1 ###
library(plyr)
# case和control年龄范围
ddply(mydata, ~ TOF, summarise, mean = round(mean(age.y), 2), sd = round(sd(age.y), 2))

# case和control性别
x <- xtabs(~ TOF + sex, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(TOF ~ sex, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# case和control试管婴儿
x <- xtabs(~ TOF + tube.baby, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(TOF ~ tube.baby, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# case和control母亲生子年龄
ddply(mydata, ~ TOF, summarise, mean = round(mean(M.production.age), 2), 
      sd = round(sd(M.production.age), 2))
mydata <- mydata %>%
  mutate(M.production.age = case_when(
    .$M.production.age < 20 ~ "<20",
    .$M.production.age > 30 ~ ">30",
    TRUE ~ "20~30"
  ))
mydata$M.production.age <- factor(mydata$M.production.age, levels = c("20~30", "<20", ">30"))

x <- xtabs(~ TOF + M.production.age, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(TOF ~ M.production.age, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# case和control是否足月产
x <- xtabs(~ TOF + term, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$term <- factor(mydata$term, levels = c("1", "0"))
mylogit <- glm(TOF ~ term, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# case和control是否顺产
x <- xtabs(~ TOF + production.mode, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$production.mode <- factor(mydata$production.mode, levels = c("1", "0"))
mylogit <- glm(TOF ~ production.mode, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# case和control是否流产
mydata <- mydata %>%
  mutate(abortion = case_when(
    .$abortion > 0 ~ "1",
    TRUE ~ "0"
  ))
mydata$abortion <- factor(mydata$abortion, levels = c("0", "1"))
x <- xtabs(~ TOF + abortion, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(TOF ~ abortion, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# case和control生产次数
x <- xtabs(~ TOF + parity, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$parity <- factor(mydata$parity)
mylogit <- glm(TOF ~ parity, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# case和control怀孕次数
mydata <- mydata %>%
  mutate(gravidity = case_when(
    .$gravidity >= 3 ~ ">=3",
    TRUE ~ as.character(gravidity)
  ))
mydata$gravidity <- factor(mydata$gravidity, levels = c("1", "2", ">=3"))
x <- xtabs(~ TOF + gravidity, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(TOF ~ gravidity, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# case和control母亲教育水平
x <- xtabs(~ TOF + M.edu, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.edu <- factor(mydata$M.edu, levels = c("4", "3", "2", "1"))
mylogit <- glm(TOF ~ M.edu, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# case和control父亲生子年龄
ddply(mydata, ~ TOF, summarise, mean = round(mean(F.production.age), 2), 
      sd = round(sd(F.production.age), 2))
# 中国男性法定结婚年龄不早于22周岁
mydata <- mydata %>%
  mutate(F.production.age = case_when(
    .$F.production.age < 22 ~ "<22",
    .$F.production.age > 30 ~ ">30",
    TRUE ~ "22~30"
  ))
mydata$F.production.age <- factor(mydata$F.production.age, levels = c("22~30", "<22", ">30"))

x <- xtabs(~ TOF + F.production.age, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(TOF ~ F.production.age, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# case和control父亲教育水平
x <- xtabs(~ TOF + F.edu, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$F.edu <- factor(mydata$F.edu, levels = c("4", "3", "2", "1"))
mylogit <- glm(TOF ~ F.edu, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
### Table 1 ###


### Table 2 ###
# case和control母亲是否接触有毒物质
x <- xtabs(~ TOF + M.toxic.exposure, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.toxic.exposure <- factor(mydata$M.toxic.exposure)
mylogit <- glm(TOF ~ M.toxic.exposure, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.toxic.exposure + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
library(car)
vif(mylogit1)

# case和control母亲是否接触放射性物质
x <- xtabs(~ TOF + M.radioactive.exposure, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.radioactive.exposure <- factor(mydata$M.radioactive.exposure)
mylogit <- glm(TOF ~ M.radioactive.exposure, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.radioactive.exposure + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲是否吸烟
x <- xtabs(~ TOF + M.smoke, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.smoke <- factor(mydata$M.smoke)
mylogit <- glm(TOF ~ M.smoke, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.smoke + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)


# case和control母亲烟龄
ddply(mydata[mydata$M.smoked.years > 0, ], ~ TOF, summarise, mean = round(mean(M.smoked.years), 2), 
      sd = round(sd(M.smoked.years), 2))
mydata <- mydata %>%
  mutate(M.smoked.years = case_when(
    .$M.smoked.years == 0 ~ "NONE",
    .$M.smoked.years <= 10 ~ "<=10",
    .$M.smoked.years > 10 ~ ">10"
  ))
mydata$M.smoked.years <- factor(mydata$M.smoked.years, 
                                levels = c("NONE", "<=10", ">10"))
# mydata <- mydata %>%
#   mutate(M.smoked.years = case_when(
#     .$M.smoked.years == 0 ~ "NONE",
#     .$M.smoked.years < 5 ~ "<5",
#     .$M.smoked.years >= 5 & .$M.smoked.years < 10 ~ "[5, 10)", 
#     .$M.smoked.years >= 10 ~ ">=10"
#   ))
# mydata$M.smoked.years <- factor(mydata$M.smoked.years, 
#                                 levels = c("NONE", "<5", "[5, 10)", ">=10"))

x <- xtabs(~ TOF + M.smoked.years, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(TOF ~ M.smoked.years, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.smoked.years + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲每天吸烟支数
mydata <- mydata %>%
  mutate(M.smoke.freq = case_when(
    .$M.smoke.freq == "0" ~ "NONE",
    .$M.smoke.freq %in% c("1", "2", "3", "4", "5") ~ "<=5",
    .$M.smoke.freq %in% c("6", "7", "8", "9", ">=10") ~ ">5"
  ))
mydata$M.smoke.freq <- factor(mydata$M.smoke.freq, 
                              levels = c("NONE", "<=5", ">5"))
# mydata <- mydata %>%
#   mutate(M.smoke.freq = case_when(
#     .$M.smoke.freq == "0" ~ "NONE",
#     .$M.smoke.freq %in% c("1", "2", "3", "4") ~ "<5",
#     .$M.smoke.freq %in% c("5", "6", "7", "8", "9") ~ "[5, 10)", 
#     .$M.smoke.freq %in% c(">=10") ~ ">=10"
#   ))
# mydata$M.smoke.freq <- factor(mydata$M.smoke.freq, 
#                               levels = c("NONE", "<5", "[5, 10)", ">=10"))

x <- xtabs(~ TOF + M.smoke.freq, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(TOF ~ M.smoke.freq, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.smoke.freq + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲孕期是否吸烟
table(mydata$M.pregnancy.smoke, useNA = "ifany")
x <- xtabs(~ TOF + M.pregnancy.smoke, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.smoke <- factor(mydata$M.pregnancy.smoke)
mylogit <- glm(TOF ~ M.pregnancy.smoke, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.pregnancy.smoke + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲孕期是否在二手烟环境
x <- xtabs(~ TOF + M.pregnancy.passive.smoke, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.passive.smoke <- factor(mydata$M.pregnancy.passive.smoke)
mylogit <- glm(TOF ~ M.pregnancy.passive.smoke, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.pregnancy.passive.smoke + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control父亲是否接触有毒物质
x <- xtabs(~ TOF + F.toxic.exposure, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$F.toxic.exposure <- factor(mydata$F.toxic.exposure)
mylogit <- glm(TOF ~ F.toxic.exposure, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ F.toxic.exposure + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control父亲是否接触放射性物质
x <- xtabs(~ TOF + F.radioactive.exposure, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$F.radioactive.exposure <- factor(mydata$F.radioactive.exposure)
mylogit <- glm(TOF ~ F.radioactive.exposure, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ F.radioactive.exposure + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control父亲是否吸烟
x <- xtabs(~ TOF + F.smoke, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$F.smoke <- factor(mydata$F.smoke)
mylogit <- glm(TOF ~ F.smoke, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ F.smoke + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control父亲烟龄
ddply(mydata[mydata$F.smoked.years != 0, ], ~ TOF, summarise, mean = round(mean(F.smoked.years), 2), 
      sd = round(sd(F.smoked.years), 2))
mydata <- mydata %>%
  mutate(F.smoked.years = case_when(
    .$F.smoked.years == 0 ~ "NONE",
    .$F.smoked.years <= 10 ~ "<=10",
    .$F.smoked.years > 10 ~ ">10"
  ))
mydata$F.smoked.years <- factor(mydata$F.smoked.years, 
                                levels = c("NONE", "<=10", ">10"))
# mydata <- mydata %>%
#   mutate(F.smoked.years = case_when(
#     .$F.smoked.years == 0 ~ "NONE",
#     .$F.smoked.years < 5 ~ "<5",
#     .$F.smoked.years >= 5 & .$F.smoked.years < 10 ~ "[5, 10)", 
#     .$F.smoked.years >= 10 ~ ">=10"
#   ))
# mydata$F.smoked.years <- factor(mydata$F.smoked.years, 
#                                 levels = c("NONE", "<5", "[5, 10)", ">=10"))

x <- xtabs(~ TOF + F.smoked.years, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(TOF ~ F.smoked.years, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ F.smoked.years + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

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
# mydata <- mydata %>%
#   mutate(F.smoke.freq = case_when(
#     .$F.smoke.freq == "0" ~ "NONE",
#     .$F.smoke.freq %in% c("1", "2", "3", "4") ~ "<5",
#     .$F.smoke.freq %in% c("5", "6", "7", "8", "9") ~ "[5, 10)", 
#     .$F.smoke.freq %in% c(">=10") ~ ">=10"
#   ))
# mydata$F.smoke.freq <- factor(mydata$F.smoke.freq, 
#                               levels = c("NONE", "<5", "[5, 10)", ">=10"))

x <- xtabs(~ TOF + F.smoke.freq, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(TOF ~ F.smoke.freq, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ F.smoke.freq + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲是否喝酒
x <- xtabs(~ TOF + M.drink, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.drink <- factor(mydata$M.drink)
mylogit <- glm(TOF ~ M.drink, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.drink + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control父亲是否喝酒
x <- xtabs(~ TOF + F.drink, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$F.drink <- factor(mydata$F.drink)
mylogit <- glm(TOF ~ F.drink, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ F.drink + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control房屋是否装修
x <- xtabs(~ TOF + decoration, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$decoration <- factor(mydata$decoration)
mylogit <- glm(TOF ~ decoration, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ decoration + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control房屋附近是否有高压电缆
x <- xtabs(~ TOF + HV.cable, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$HV.cable <- factor(mydata$HV.cable)
mylogit <- glm(TOF ~ HV.cable, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ HV.cable + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control房屋附近是否有化工厂
x <- xtabs(~ TOF + chemical.plant, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$chemical.plant <- factor(mydata$chemical.plant)
mylogit <- glm(TOF ~ chemical.plant, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ chemical.plant + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)
### Table 2 ###



### Table 3 ###
# case和control母亲孕期是否感冒
x <- xtabs(~ TOF + M.pregnancy.flu, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.flu <- factor(mydata$M.pregnancy.flu)
mylogit <- glm(TOF ~ M.pregnancy.flu, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.pregnancy.flu + M.production.age + parity + gravidity + M.edu, 
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

x <- xtabs(~ TOF + M.pregnancy.flu.time, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(TOF ~ M.pregnancy.flu.time, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.pregnancy.flu.time + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲有无孕期合并症
x <- xtabs(~ TOF + M.pregnancy.complication, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.complication <- factor(mydata$M.pregnancy.complication)
mylogit <- glm(TOF ~ M.pregnancy.complication, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.pregnancy.complication + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲孕期是否用药
x <- xtabs(~ TOF + M.pregnancy.med, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.med <- factor(mydata$M.pregnancy.med)
mylogit <- glm(TOF ~ M.pregnancy.med, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.pregnancy.med + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

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

x <- xtabs(~ TOF + M.pregnancy.med.time, data = mydata)
round(x / rowSums(x) * 100, 2)

mylogit <- glm(TOF ~ M.pregnancy.med.time, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.pregnancy.med.time + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲孕期使用何种药物
x <- xtabs(~ TOF + M.pregnancy.med.name, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.med.name <- factor(mydata$M.pregnancy.med.name, 
                                      levels = c("NONE", "Antibiotics", "Diet pill", "Antidepressant", 
                                                 "Antitumor drug", "Tocolytic agent", "Analgesics", 
                                                 "Tranquilizer"))
mylogit <- glm(TOF ~ M.pregnancy.med.name, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.pregnancy.med.name + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲孕前1-3月是否口服避孕药
x <- xtabs(~ TOF + M.oral.contraceptive, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.oral.contraceptive <- factor(mydata$M.oral.contraceptive)
mylogit <- glm(TOF ~ M.oral.contraceptive, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.oral.contraceptive + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)

# case和control母亲孕期是否使用叶酸
x <- xtabs(~ TOF + M.pregnancy.folic.acid, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata$M.pregnancy.folic.acid <- factor(mydata$M.pregnancy.folic.acid)
mylogit <- glm(TOF ~ M.pregnancy.folic.acid, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(TOF ~ M.pregnancy.folic.acid + M.production.age + parity + gravidity + M.edu, 
                data = mydata, family = "binomial")
summary(mylogit1)
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
vif(mylogit1)
### Table 3 ###
