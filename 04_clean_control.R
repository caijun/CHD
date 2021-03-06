rm(list = ls())

source("R/helper.R")

load("output/control_combine.rda")

library(tidyverse)
# check each variable ----------------------------------------------------------
names(dat)
names(dat) <- c("id" # 编号
                , "input.date" # 登陆日期
                , "residence" # 居住地
                , "sex" # 性别
                , "age" # 年龄
                , "gravidity.parity" # 孕产次
                , "term" # 是否足月产
                , "tube.baby" # 是否试管婴儿
                , "production.mode" # 生产方式
                , "M.edu" # 母亲文化水平
                , "M.toxic.exposure" # 母亲是否接触有毒物质
                , "M.radioactive.exposure" # 母亲是否接触放射性物质
                , "M.smoke" # 母亲是否吸烟
                , "M.smoke.start.age" # 母亲几岁开始吸烟
                , "M.smoke.freq" # 母亲每天吸烟支数
                , "M.pregnancy.smoke" # 母亲孕期是否吸烟
                , "M.pregnancy.passive.smoke" # 母亲孕期是否在二手烟环境
                , "M.drink" # 母亲是否喝酒
                , "M.production.age" # 母亲生子年龄
                , "M.pregnancy.complication" # 母亲有无孕期合并症
                , "M.pregnancy.flu" # 母亲孕期是否感冒
                , "M.pregnancy.flu.time" # 母亲孕期感冒时期
                , "M.pregnancy.med" # 母亲孕期是否用药
                , "M.pregnancy.med.time" # 母亲孕期用药时期
                , "M.pregnancy.med.name" # 母亲孕期使用何种药物
                , "M.pregnancy.folic.acid" # 母亲孕期是否使用叶酸
                , "M.oral.contraceptive" # 母亲孕前1-3月是否口服避孕药
                , "F.production.age" # 父亲生子年龄
                , "F.edu" # 父亲文化水平
                , "F.toxic.exposure" # 父亲是否常接触有害物质
                , "F.radioactive.exposure" # 父亲是否常接触放射性物质
                , "F.smoke" # 父亲是否吸烟
                , "F.smoke.start.age" # 父亲几岁开始吸烟
                , "F.smoke.freq" # 父亲每天吸烟支数
                , "F.drink" # 父亲是否经常饮酒
                , "decoration" # 房屋是否装修
                , "HV.cable" # 房屋附近是否有高压电缆
                , "chemical.plant" # 房屋附近是否有化工厂
                , "IR.cardiopathy" # 直系亲属中是否有心脏病
                , "IR.CHD" # 直系亲属中是否有先天性心脏病
                , "IR.congenital.disease" # 直系亲属中是否有先天性疾病
                )
names(dat)

# check duplicated cases: no duplicated cases
d1 <- dat %>% 
  select(id, input.date, residence, sex, age)
# unique cases based on input date, residence, sex, and age
d2 <- d1 %>% 
  distinct(input.date, residence, sex, age, .keep_all = TRUE)
setdiff(d1$id, d2$id)
c <- subset(dat, id == "nj1546")
c1 <- subset(dat, input.date == "20140908" & residence == "南京" & sex == "女" 
             & age == "2岁2月")

# check 母亲、父亲生子年龄
mean(dat$M.production.age)
mean(dat$F.production.age)

# 登录日期
dat <- dat %>% 
  mutate(input.date = as.Date(as.character(input.date), "%Y%m%d")) %>%
  filter(!is.na(input.date))
range(dat$input.date)
# daily number of inputs
library(incidence)
dno <- incidence(dat$input.date)
plot(dno) + 
  labs(y = "Daily no. of inputs")

# 居住地
unique(dat$residence)
dat <- dat %>%
  mutate(residence = gsub(" ", "", residence))
unique(dat$residence)
residence <- read.csv("data/case_control_residence_fix.csv", fileEncoding = "utf8")
setdiff(unique(dat$residence), residence$residence)
# extract residence county
residence$county <- sapply(seq_along(residence$fullname), function(i) {
  gsub(paste0(residence$province[i], residence$prefecture[i]), "", 
       residence$fullname[i])
})

residence <- residence %>%
  mutate(county = case_when(
    .$county == "" ~ as.character(NA), 
    TRUE ~ .$county
  ))
dat <- merge(dat, residence, by = "residence", all.x = TRUE)
unique(subset(dat, is.na(fullname))$residence)
x <- subset(dat, is.na(fullname))

# by province
x <- dat %>%
  group_by(province) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(desc(n))
# by prefecture, to show this result on map
x <- dat %>%
  mutate(prefecture = paste0(province, prefecture)) %>%
  group_by(prefecture) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(desc(n))
# by county
x <- dat %>%
  mutate(county = paste0(province, prefecture, county)) %>%
  group_by(county) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(desc(n))

# 性别
unique(dat$sex)
length(unique(dat$sex))
dat <- dat %>%
  mutate(sex = case_when(
    .$sex == "男" ~ "M",
    .$sex == "女" ~ "F"
  ))
table(dat$sex, useNA = "ifany")

# 年龄
dat <- dat %>%
  dplyr::mutate(age = gsub("个", "", age), 
                age = gsub("岁", "y", age), 
                age = gsub("月", "m", age), 
                age = gsub("天", "d", age))
unique(dat$age)
table(dat$age, useNA = "ifany")
age <- as.data.frame(t(sapply(dat$age, ext.ymd)))
dat <- dat %>%
  bind_cols(age) %>%
  mutate(age.y = year + month / 12 + day / 365.25)
summary(dat$age.y)
hist(dat$age.y, main = "Histogram of Age")

# 孕产次
unique(dat$gravidity.parity)
dat <- dat %>% 
  mutate(gravidity.parity = str_trim(gravidity.parity)) %>% 
  mutate(gravidity.parity = case_when(
    .$gravidity.parity == "G1P1，双胎" ~ "G1P1.twins", 
    TRUE ~ as.character(.$gravidity.parity)
  )) %>%
  mutate(gravidity = as.integer(substr(gravidity.parity, 2, 2)), 
         parity = as.integer(substr(.$gravidity.parity, 4, 4)), 
         abortion = gravidity - parity)
unique(dat$gravidity.parity, useNA = "ifany")
table(dat$gravidity.parity, useNA = "ifany")
table(dat$gravidity, useNA = "ifany")
table(dat$parity, useNA = "ifany")
table(dat$abortion, useNA = "ifany")

# 是否足月产
unique(dat$term)
dat <- dat %>%
  mutate(term = case_when(
    .$term == 1 ~ 1,
    .$term == 2 ~ 0
  ))
table(dat$term, useNA = "ifany")

# 是否试管婴儿
unique(dat$tube.baby)
dat <- dat %>%
  mutate(tube.baby = case_when(
    .$tube.baby == 1 ~ 1,
    .$tube.baby == 2 ~ 0
  ))
table(dat$tube.baby, useNA = "ifany")

# 生产方式:顺产，剖宫产
unique(dat$production.mode)
dat <- dat %>%
  mutate(production.mode = case_when(
    .$production.mode == 1 ~ 1,
    .$production.mode == 2 ~ 0
  ))
table(dat$production.mode, useNA = "ifany")

# 母亲文化水平
# 1-文盲，2-初中及以下，3-高中，4-本科及以上
unique(dat$M.edu)
table(dat$M.edu, useNA = "ifany")

# 母亲是否接触有毒物质
unique(dat$M.toxic.exposure)
dat <- dat %>%
  mutate(M.toxic.exposure = case_when(
    .$M.toxic.exposure == 1 ~ 1,
    .$M.toxic.exposure == 2 ~ 0
  ))
table(dat$M.toxic.exposure, useNA = "ifany")

# 母亲是否接触放射性物质
unique(dat$M.radioactive.exposure)
dat <- dat %>%
  mutate(M.radioactive.exposure = case_when(
    .$M.radioactive.exposure == 1 ~ 1,
    .$M.radioactive.exposure == 2 ~ 0
  ))
table(dat$M.radioactive.exposure, useNA = "ifany")

# 母亲是否吸烟
unique(dat$M.smoke)
dat <- dat %>%
  mutate(M.smoke = case_when(
    .$M.smoke == 1 ~ 1,
    .$M.smoke == 2 ~ 0
  ))
table(dat$M.smoke, useNA = "ifany")

# 母亲几岁开始吸烟:与母亲是否吸烟数据不吻合，母亲是否吸烟数据存在问题，据此更新母是否吸烟数据
unique(dat$M.smoke.start.age)
dat <- dat %>%
  mutate(M.smoke.start.age = case_when(
    .$M.smoke.start.age == "NA" ~ as.numeric(NA),
    TRUE ~ as.numeric(.$M.smoke.start.age)
  ))
table(dat$M.smoke.start.age, useNA = "ifany")
dat <- dat %>%
  mutate(M.smoke = case_when(
    is.na(.$M.smoke.start.age) ~ 0,
    TRUE ~ 1
  ))
table(dat$M.smoke, useNA = "ifany")
# 将母亲几岁开始吸烟转换为烟龄
dat <- dat %>%
  mutate(M.smoked.years = case_when(
    is.na(.$M.smoke.start.age) ~ 0,
    TRUE ~ .$M.production.age - .$M.smoke.start.age
  ))
summary(dat$M.smoked.years)
hist(dat$M.smoked.years)
table(dat$M.smoked.years)

# 母亲每天吸烟支数
unique(dat$M.smoke.freq)
dat <- dat %>%
  mutate(M.smoke.freq = case_when(
    .$M.smoke.freq == "NA" ~ as.integer(NA),
    TRUE ~ as.integer(.$M.smoke.freq)
  ))
table(dat$M.smoke.freq, useNA = "ifany")
dat <- dat %>% 
  mutate(M.smoke.freq = case_when(
    is.na(.$M.smoke.freq) ~ "0", 
    !is.na(.$M.smoke.freq) & (.$M.smoke.freq > 9) ~ as.character(">=10"), 
    !is.na(.$M.smoke.freq) & TRUE ~ as.character(.$M.smoke.freq)
  ))
table(dat$M.smoke.freq, useNA = "ifany")

# 母亲孕期是否吸烟
unique(dat$M.pregnancy.smoke)
dat <- dat %>%
  mutate(M.pregnancy.smoke = case_when(
    .$M.pregnancy.smoke == "NA" ~ as.integer(NA),
    TRUE ~ as.integer(.$M.pregnancy.smoke)
  ))
table(dat$M.pregnancy.smoke, useNA = "ifany")
dat <- dat %>%
  mutate(M.pregnancy.smoke = case_when(
    is.na(.$M.pregnancy.smoke) ~ 0,
    .$M.pregnancy.smoke == 1 ~ 1,
    .$M.pregnancy.smoke == 2 ~ 0
  ))
table(dat$M.pregnancy.smoke, useNA = "ifany")

# 母亲孕期是否在二手烟环境
unique(dat$M.pregnancy.passive.smoke)
dat <- dat %>%
  mutate(M.pregnancy.passive.smoke = case_when(
    .$M.pregnancy.passive.smoke == 1 ~ 1,
    .$M.pregnancy.passive.smoke == 2 ~ 0
  ))
table(dat$M.pregnancy.passive.smoke, useNA = "ifany")

# 母亲是否喝酒
unique(dat$M.drink)
dat <- dat %>%
  mutate(M.drink = case_when(
    .$M.drink == 1 ~ 1,
    .$M.drink == 2 ~ 0
  ))
table(dat$M.drink, useNA = "ifany")

# 母亲生子年龄
unique(dat$M.production.age)
table(dat$M.production.age, useNA = "ifany")

# 母亲有无孕期合并症
unique(dat$M.pregnancy.complication)
dat <- dat %>%
  mutate(M.pregnancy.complication = case_when(
    .$M.pregnancy.complication == 1 ~ 1,
    .$M.pregnancy.complication == 2 ~ 0
  ))
table(dat$M.pregnancy.complication, useNA = "ifany")

# 母亲孕期是否感冒
unique(dat$M.pregnancy.flu)
dat <- dat %>%
  mutate(M.pregnancy.flu = case_when(
    .$M.pregnancy.flu == 1 ~ 1,
    .$M.pregnancy.flu == 2 ~ 0
  ))
table(dat$M.pregnancy.flu, useNA = "ifany")

# 母亲孕期感冒时期:月
unique(dat$M.pregnancy.flu.time)
dat <- dat %>%
  mutate(M.pregnancy.flu.time = case_when(
    .$M.pregnancy.flu.time == "NA" ~ as.integer(NA),
    TRUE ~ as.integer(.$M.pregnancy.flu.time)
  ))
table(dat$M.pregnancy.flu.time, useNA = "ifany")
dat <- dat %>%
  mutate(M.pregnancy.flu.time = case_when(
    is.na(.$M.pregnancy.flu.time) ~ 0L,
    TRUE ~ .$M.pregnancy.flu.time
  ))
table(dat$M.pregnancy.flu.time, useNA = "ifany")

# 母亲孕期是否用药
unique(dat$M.pregnancy.med)
dat <- dat %>%
  mutate(M.pregnancy.med = case_when(
    .$M.pregnancy.med == "NA" ~ 2L,
    TRUE ~ as.integer(.$M.pregnancy.med)
  ))
table(dat$M.pregnancy.med, useNA = "ifany")
dat <- dat %>%
  mutate(M.pregnancy.med = case_when(
    .$M.pregnancy.med == 1 ~ 1,
    .$M.pregnancy.med == 2 ~ 0
  ))
table(dat$M.pregnancy.med, useNA = "ifany")

# 母亲孕期用药时期:月
unique(dat$M.pregnancy.med.time)
dat <- dat %>%
  mutate(M.pregnancy.med.time = case_when(
    .$M.pregnancy.med.time == "NA" ~ as.integer(NA),
    TRUE ~ as.integer(.$M.pregnancy.med.time)
  ))
table(dat$M.pregnancy.med.time, useNA = "ifany")
dat <- dat %>%
  mutate(M.pregnancy.med.time = case_when(
    is.na(.$M.pregnancy.med.time) ~ 0L,
    TRUE ~ .$M.pregnancy.med.time
  ))
table(dat$M.pregnancy.med.time, useNA = "ifany")

# 母亲孕期使用何种药物
# Tocolytic agent-抑制分娩药, Antibiotics-抗生素, Analgesics-止痛剂, 
# Tranquilizer-镇静剂, Diet pill-节食丸, Antidepressant-抗抑郁剂, 
# Antitumor drug-抗癌药
unique(dat$M.pregnancy.med.name)
dat <- dat %>%
  mutate(M.pregnancy.med.name = case_when(
    .$M.pregnancy.med.name == "NA" ~ as.character(NA),
    TRUE ~ as.character(.$M.pregnancy.med.name)
  ))
table(dat$M.pregnancy.med.name, useNA = "ifany")
dat <- dat %>% 
  mutate(M.pregnancy.med.name = gsub("?", " ", M.pregnancy.med.name, fixed = TRUE)) %>% 
  # replace non-breaking space with space
  mutate(M.pregnancy.med.name = gsub("\u00A0", " ", M.pregnancy.med.name, fixed = TRUE)) %>% 
  mutate(M.pregnancy.med.name = case_when(
    is.na(.$M.pregnancy.med.name) ~ "NONE", 
    TRUE ~ .$M.pregnancy.med.name
  ))
table(dat$M.pregnancy.med.name, useNA = "ifany")

# 母亲孕期是否使用叶酸
unique(dat$M.pregnancy.folic.acid)
dat <- dat %>%
  mutate(M.pregnancy.folic.acid = case_when(
    is.na(.$M.pregnancy.folic.acid) ~ 2L,
    TRUE ~ as.integer(.$M.pregnancy.folic.acid)
  ))
table(dat$M.pregnancy.folic.acid, useNA = "ifany")
dat <- dat %>%
  mutate(M.pregnancy.folic.acid = case_when(
    .$M.pregnancy.folic.acid == 1 ~ 1,
    .$M.pregnancy.folic.acid == 2 ~ 0
  ))
table(dat$M.pregnancy.folic.acid, useNA = "ifany")

# 母亲孕前1-3月是否口服避孕药
unique(dat$M.oral.contraceptive)
dat <- dat %>%
  mutate(M.oral.contraceptive = case_when(
    .$M.oral.contraceptive == 1 ~ 1,
    .$M.oral.contraceptive == 2 ~ 0
  ))
table(dat$M.oral.contraceptive, useNA = "ifany")

# 母亲孕前1-3月是否口服避孕药以及母亲孕期是否用药合并为母亲是否用药
dat <- dat %>% 
  mutate(M.med = as.integer(M.pregnancy.med | M.oral.contraceptive))
table(dat$M.med, useNA = "ifany")

# 父亲生子年龄
unique(dat$F.production.age)
table(dat$F.production.age, useNA = "ifany")

# 父亲文化水平
unique(dat$F.edu)
table(dat$F.edu, useNA = "ifany")

# 父亲是否常接触有害物质
unique(dat$F.toxic.exposure)
dat <- dat %>%
  mutate(F.toxic.exposure = case_when(
    .$F.toxic.exposure == 1 ~ 1,
    .$F.toxic.exposure == 2 ~ 0
  ))
table(dat$F.toxic.exposure, useNA = "ifany")

# 父亲是否常接触放射性物质
unique(dat$F.radioactive.exposure)
dat <- dat %>%
  mutate(F.radioactive.exposure = case_when(
    .$F.radioactive.exposure == 1 ~ 1,
    .$F.radioactive.exposure == 2 ~ 0
  ))
table(dat$F.radioactive.exposure, useNA = "ifany")

# 父亲是否吸烟
unique(dat$F.smoke)
dat <- dat %>%
  mutate(F.smoke = case_when(
    .$F.smoke == 1 ~ 1,
    .$F.smoke == 2 ~ 0
  ))
table(dat$F.smoke, useNA = "ifany")

# 父亲几岁开始吸烟:与父亲是否吸烟数据不吻合，父亲是否吸烟数据存在问题，据此更新父亲是否吸烟数据
unique(dat$F.smoke.start.age)
dat <- dat %>%
  mutate(F.smoke.start.age = case_when(
    .$F.smoke.start.age == "NA" ~ as.numeric(NA),
    TRUE ~ as.numeric(.$F.smoke.start.age)
  ))
table(dat$F.smoke.start.age, useNA = "ifany")
dat <- dat %>%
  mutate(F.smoke = case_when(
    is.na(.$F.smoke.start.age) ~ 0,
    TRUE ~ 1
  ))
table(dat$F.smoke, useNA = "ifany")
# 将父亲几岁开始吸烟转换为烟龄
dat <- dat %>%
  mutate(F.smoked.years = case_when(
    is.na(.$F.smoke.start.age) ~ 0,
    TRUE ~ .$F.production.age - .$F.smoke.start.age
  ))
summary(dat$F.smoked.years)
hist(dat$F.smoked.years)

# 父亲每天吸烟支数
unique(dat$F.smoke.freq)
table(dat$F.smoke.freq, useNA = "ifany")
dat <- dat %>% 
  mutate(F.smoke.freq = as.character(F.smoke.freq)) %>%
  mutate(F.smoke.freq = case_when(
    is.na(.$F.smoke.freq) ~ "0",
    .$F.smoke.freq == ">10" ~ ">=10",
    TRUE ~ as.character(.$F.smoke.freq)
  ))
table(dat$F.smoke.freq, useNA = "ifany")

# 父亲是否经常饮酒
unique(dat$F.drink)
dat <- dat %>%
  mutate(F.drink = case_when(
    is.na(.$F.drink) ~ 0,
    .$F.drink == 1 ~ 1,
    .$F.drink == 2 ~ 0
  ))
table(dat$F.drink, useNA = "ifany")

# 房屋是否装修
unique(dat$decoration)
dat <- dat %>%
  mutate(decoration = case_when(
    is.na(.$decoration) ~ 0, 
    .$decoration == 1 ~ 1,
    .$decoration == 2 ~ 0
  ))
table(dat$decoration, useNA = "ifany")

# 房屋附近是否有高压电缆
unique(dat$HV.cable)
dat <- dat %>%
  mutate(HV.cable = case_when(
    is.na(.$HV.cable) ~ 0,
    .$HV.cable == 1 ~ 1,
    .$HV.cable == 2 ~ 0
  ))
table(dat$HV.cable, useNA = "ifany")

# 房屋附近是否有化工厂
unique(dat$chemical.plant)
dat <- dat %>%
  mutate(chemical.plant = case_when(
    is.na(.$chemical.plant) ~ 0,
    .$chemical.plant == 1 ~ 1,
    .$chemical.plant == 2 ~ 0
  ))
table(dat$chemical.plant, useNA = "ifany")

# 直系亲属中是否有心脏病
unique(dat$IR.cardiopathy)
dat <- dat %>%
  mutate(IR.cardiopathy = case_when(
    is.na(.$IR.cardiopathy) ~ 0,
    .$IR.cardiopathy == 1 ~ 1,
    .$IR.cardiopathy == 2 ~ 0
  ))
table(dat$IR.cardiopathy, useNA = "ifany")

# 直系亲属中是否有先天性心脏病
unique(dat$IR.CHD)
dat <- dat %>%
  mutate(IR.CHD = case_when(
    is.na(.$IR.CHD) ~ 0,
    .$IR.CHD == 1 ~ 1,
    .$IR.CHD == 2 ~ 0
  ))
table(dat$IR.CHD, useNA = "ifany")

# 直系亲属中是否有先天性疾病
unique(dat$IR.congenital.disease)
dat <- dat %>%
  mutate(IR.congenital.disease = case_when(
    is.na(.$IR.congenital.disease) ~ 0,
    .$IR.congenital.disease == 1 ~ 1,
    .$IR.congenital.disease == 2 ~ 0
  ))
table(dat$IR.congenital.disease, useNA = "ifany")

# 根据登录日期和手术时年龄计算出生时年月
library(lubridate)
dat <- dat %>%
  mutate(birthYMD = as.Date(ymd(input.date) - dyears(age.y))) %>%
  mutate(birthY = year(birthYMD), birthM = month(birthYMD))

saveRDS(dat, "output/control.rds")
