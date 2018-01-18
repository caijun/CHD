rm(list = ls())

library(xlsx)

# combine multiple data files into one -----------------------------------------
dat.nj <- read.xlsx("~/Downloads/FengYu/raw/网上手术登记11-1(diagnosis).xls", 
                    sheetName = "case")
# 编号2133不存在
dat.nj$编号 <- paste0("nj", dat.nj$编号)
dat.nj$血型 <- NULL

dat.sz <- read.xlsx("~/Downloads/FengYu/raw/苏州儿童医院数据.xls", 
                    sheetName = "case")
dat.sz$编号 <- paste0("sz", dat.sz$编号)
dat.sz$血型 <- NULL

dat.nj1 <- read.xlsx("~/Downloads/FengYu/raw/新增(diagnosis).xls", 
                     sheetName = "case")
dat.nj1$编号 <- paste0("nj_new", dat.nj1$编号)

dat.nj2 <- read.xlsx("~/Downloads/FengYu/raw/2017 update.xlsx", 
                     sheetName = "case")
dat.nj2$编号 <- paste0("nj_2017new", dat.nj2$编号)

setdiff(names(dat.nj), names(dat.nj2))
setdiff(names(dat.nj2), names(dat.nj))

dat <- rbind(dat.nj, dat.sz, dat.nj1, dat.nj2)
write.xlsx(dat, file = "output/case_combine.xlsx")
save(dat, file = "output/case_combine.rda")
