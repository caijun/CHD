rm(list = ls())

library(xlsx)

# combine multiple data files of controls into one -----------------------------
dat.nj <- read.xlsx("~/Downloads/FengYu/raw/网上手术登记11-1(diagnosis).xls", 
                    sheetName = "control")
dat.nj$编号 <- paste0("nj", dat.nj$编号)

dat.sz <- read.xlsx("~/Downloads/FengYu/raw/苏州儿童医院数据.xls", 
                    sheetName = "control")
dat.sz$编号 <- paste0("sz", dat.sz$编号)

dat.nj1 <- read.xlsx("~/Downloads/FengYu/raw/新增(diagnosis).xls", 
                     sheetName = "control")
dat.nj1$编号 <- paste0("nj_new", dat.nj1$编号)

dat.nj2 <- read.xlsx("~/Downloads/FengYu/raw/control new.xlsx", 
                     sheetName = "control")
dat.nj2$编号 <- paste0("nj_newnew", dat.nj2$编号)

dat.nj3 <- read.xlsx("~/Downloads/FengYu/raw/2017 update.xlsx", 
                     sheetName = "control")
dat.nj3$编号 <- paste0("nj_2017new", dat.nj3$编号)

dat <- rbind(dat.nj, dat.sz, dat.nj1, dat.nj2, dat.nj3)
write.xlsx(dat, file = "output/control_combine.xlsx")
save(dat, file = "output/control_combine.rda")
