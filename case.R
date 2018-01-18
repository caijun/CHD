rm(list = ls())
setwd("~/Downloads/FengYu/")

source("src/helper.R")

library(plyr)
library(tidyverse)

dat.nanjing <- read.csv("dat/case.csv", fileEncoding = "utf8")
# 编号2133不存在
dat.nanjing$编号 <- paste0("nj", dat.nanjing$编号)
dat.nanjing$血型 <- NULL
dat.suzhou <- read.csv("dat/case_suzhou.csv", fileEncoding = "utf8")
dat.suzhou$编号 <- paste0("sz", dat.suzhou$编号)
dat.suzhou$血型 <- NULL
dat.nanjing1 <- read.csv("dat/case_new.csv", fileEncoding = "utf8")
dat.nanjing1$编号 <- paste0("nj_new", dat.nanjing1$编号)
dat <- rbind(dat.nanjing, dat.suzhou, dat.nanjing1)
write.csv(dat, file = "dat/case_combine.csv", fileEncoding = "utf8", row.names = F)
library(xlsx)
write.xlsx(dat, file = "dat/case_combine.xlsx")
names(dat)
names(dat) <- c("id" # 编号
                # , "blood.type" # 血型
                , "surgery.date" # 手术日期
                , "residence" # 居住地
                , "sex" # 性别
                , "age" # 年龄
                , "diagnosis" # 诊断结果
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
                , "M.pregnancy.folic.acid" # 母亲孕期是否感冒使用叶酸
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

# check 母亲、父亲生子年龄
mean(dat$M.production.age)
mean(dat$F.production.age)

# check duplicated cases
x <- dat %>% 
  select(id, surgery.date, residence, sex, age)
y <- x %>% 
  distinct(surgery.date, residence, sex, age, .keep_all = TRUE)
setdiff(x$id, y$id)
z <- subset(dat, id == "nj_new565")
t <- subset(dat, surgery.date == "20160806" & residence == "南京" & sex == "女" & age == "1岁5月")

# # 血型: 有研究表明没有影响
# unique(dat$blood.type)
# dat <- dat %>%
#   mutate(blood.type = as.character(blood.type)) %>% 
#   mutate(blood.type = case_when(
#     .$blood.type == "" ~ as.character(NA), 
#     TRUE ~ .$blood.type
#   ))
# table(dat$blood.type, useNA = "ifany")

# 手术日期
dat <- dat %>%
  mutate(surgery.date = as.Date(as.character(surgery.date), "%Y%m%d")) %>%
  filter(!is.na(surgery.date))
range(dat$surgery.date)
x <- table(dat$surgery.date, useNA = "ifany")
plot(x, ylab = "No. of surgeries")
# daily number of surgeries
full.date <- seq.Date(range(dat$surgery.date)[1], range(dat$surgery.date)[2], by = "day")
dno <- plyr::ldply(full.date, function(d) {
  df <- subset(dat, surgery.date == d)
  return(data.frame(date = d, no = nrow(df)))
})
plot(dno$date, dno$no, type = "l", xlab = "Date", ylab = "No. of surgeries")

# 居住地
unique(dat$residence)
dat <- dat %>%
  mutate(residence = gsub(" ", "", residence))
unique(dat$residence)
write.csv(unique(dat$residence), file = "dat/case_residence.csv", row.names = F, quote = F)
# residence <- read.csv("case_residence_fix.csv", fileEncoding = "cp936")
residence <- read.csv("dat/case_control_residence_fix.csv", fileEncoding = "utf8")
x <- as.data.frame(table(residence$residence))
setdiff(residence$residence, unique(dat$residence))
setdiff(unique(dat$residence), residence$residence)
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
# # the following commands caused the R session crash.
# dat <- dat %>%
#   left_join(residence, by = "residence")
unique(subset(dat, is.na(fullname))$residence)
x <- subset(dat, is.na(fullname))
# # check 和县，和县病例太多了，需要复查一下
# x <- dat[grepl("和县", dat$residence), ]
# write.csv(x, file = "dat/check_case_residence.csv", row.names = F, 
#           quote = T)

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
    .$sex == "女" ~ "F",
    .$sex == "" ~ as.character(NA)
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
unique(dat$gravidity.parity)
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

# 母亲几岁开始吸烟:与母亲是否吸烟数据不吻合，母亲是否吸烟数据存在问题，据此更新母亲是否吸烟数据
unique(dat$M.smoke.start.age)
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
    is.na(.$M.smoke.start.age) ~ 0L,
    TRUE ~ .$M.production.age - .$M.smoke.start.age
  ))
summary(dat$M.smoked.years)

# 母亲每天吸烟支数
unique(dat$M.smoke.freq)
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
    .$M.pregnancy.med == 1 ~ 1,
    .$M.pregnancy.med == 2 ~ 0
  ))
table(dat$M.pregnancy.med, useNA = "ifany")

# 母亲孕期用药时期:月
unique(dat$M.pregnancy.med.time)
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
  mutate(M.pregnancy.med.name = gsub("?", " ", M.pregnancy.med.name, fixed = TRUE)) %>% 
  # replace non-breaking space with space
  mutate(M.pregnancy.med.name = gsub("\u00A0", " ", M.pregnancy.med.name, fixed = TRUE)) %>%
  mutate(M.pregnancy.med.name = case_when(
    .$M.pregnancy.med.name == "" ~ "NONE", 
    TRUE ~ .$M.pregnancy.med.name
  ))
table(dat$M.pregnancy.med.name, useNA = "ifany")

# 母亲孕期是否使用叶酸
unique(dat$M.pregnancy.folic.acid)
dat <- dat %>%
  mutate(M.pregnancy.folic.acid = case_when(
    .$M.pregnancy.folic.acid == 1 ~ 1,
    .$M.pregnancy.folic.acid == 2 ~ 0
  ))
table(dat$M.pregnancy.folic.acid, useNA = "ifany")

# 母亲孕前1-3月是否口服避孕药
unique(dat$M.oral.contraceptive)
x <- subset(dat, M.oral.contraceptive == 11)
dat <- dat %>%
  mutate(M.oral.contraceptive = case_when(
    .$M.oral.contraceptive == 11 ~ 1, 
    .$M.oral.contraceptive == 1 ~ 1,
    .$M.oral.contraceptive == 2 ~ 0
  ))
table(dat$M.oral.contraceptive, useNA = "ifany")
# x <- subset(dat, is.na(M.oral.contraceptive))
# write.csv(x, file = "dat/check_case_M.oral.contraceptive.csv", row.names = F, 
#           quote = F)

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
    is.na(.$F.smoke.start.age) ~ 0L,
    TRUE ~ .$F.production.age - .$F.smoke.start.age
  ))
summary(dat$F.smoked.years)

# 父亲每天吸烟支数
unique(dat$F.smoke.freq)
dat <- dat %>%
  mutate(F.smoke.freq = case_when(
    is.na(.$F.smoke.freq) ~ "",
    .$F.smoke.freq == ">10" ~ ">=10",
    .$F.smoke.freq == "" ~ "0",
    TRUE ~ as.character(.$F.smoke.freq)
  ))
table(dat$F.smoke.freq, useNA = "ifany")

# 父亲是否经常饮酒
unique(dat$F.drink)
dat <- dat %>%
  mutate(F.drink = case_when(
    .$F.drink == 1 ~ 1,
    .$F.drink == 2 ~ 0
  ))
table(dat$F.drink, useNA = "ifany")

# 房屋是否装修
unique(dat$decoration)
dat <- dat %>%
  mutate(decoration = case_when(
    .$decoration == 1 ~ 1,
    .$decoration == 2 ~ 0
  ))
table(dat$decoration, useNA = "ifany")

# 房屋附近是否有高压电缆
unique(dat$HV.cable)
dat <- dat %>%
  mutate(HV.cable = case_when(
    .$HV.cable %in% c(1, 11) ~ 1,
    .$HV.cable == 2 ~ 0
  ))
table(dat$HV.cable, useNA = "ifany")

# 房屋附近是否有化工厂
unique(dat$chemical.plant)
dat <- dat %>%
  mutate(chemical.plant = case_when(
    .$chemical.plant == 1 ~ 1,
    .$chemical.plant == 2 ~ 0
  ))
table(dat$chemical.plant, useNA = "ifany")

# 直系亲属中是否有心脏病
unique(dat$IR.cardiopathy)
dat <- dat %>%
  mutate(IR.cardiopathy = case_when(
    .$IR.cardiopathy == 1 ~ 1,
    .$IR.cardiopathy == 2 ~ 0
  ))
table(dat$IR.cardiopathy, useNA = "ifany")

# 直系亲属中是否有先天性心脏病
unique(dat$IR.CHD)
dat <- dat %>%
  mutate(IR.CHD = case_when(
    .$IR.CHD == 1 ~ 1,
    .$IR.CHD == 2 ~ 0
  ))
table(dat$IR.CHD, useNA = "ifany")

# 直系亲属中是否有先天性疾病
unique(dat$IR.congenital.disease)
dat <- dat %>%
  mutate(IR.congenital.disease = case_when(
    .$IR.congenital.disease == 1 ~ 1,
    .$IR.congenital.disease == 2 ~ 0
  ))
table(dat$IR.congenital.disease, useNA = "ifany")

# 根据登录日期和手术时年龄计算出生时年月
library(lubridate)
dat <- dat %>%
  mutate(birthYMD = as.Date(ymd(surgery.date) - dyears(age.y))) %>%
  mutate(birthY = year(birthYMD), birthM = month(birthYMD))

saveRDS(dat, "dat/case.rds")
