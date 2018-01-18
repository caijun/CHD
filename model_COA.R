rm(list = ls())
setwd("~/Downloads/FengYu/")

library(tidyverse)

load("dat/case_control_matched.rda")

case.m$COA <- ifelse(grepl("COA", ignore.case = TRUE, case.m$diagnosis), 1, 0)
case.m1 <- subset(case.m, COA == 1)
control.m1 <- subset(control.m, id %in% id.lookup1[id.lookup1$case.id %in% case.m1$id, "control.id"])
control.m1$COA <- 0