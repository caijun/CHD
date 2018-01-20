rm(list = ls())

source("R/helper.R")

library(tidyverse)

case <- readRDS("output/case.rds")
names(case)
control <- readRDS("output/control.rds")
names(control)

# 剔除先天愚型:Down，先天愚型，21-三体综合征
idx <- grepl("Down|愚|三体锁", ignore.case = TRUE, case$diagnosis)
Down <- case[idx, ]
case <- case[!idx, ]

match <- function(control.df) {
  control.id <- control.df$id
  x <- subset(case, sex == control.df$sex & province == control.df$province & prefecture == control.df$prefecture & abs(birthYMD - control.df$birthYMD) <= 15)
  x <- x[!x$id %in% id.lookup$case.id, ]
  if (nrow(x) == 0) {
    case.id <- NA
  } else if (nrow(x) == 1) {
    case.id <- x$id
  } else {
    x1 <- subset(x, !is.na(county) & county == control.df$county)
    if (nrow(x1) == 0) {
      idx <- which.min(abs(x$birthYMD - control.df$birthYMD))
      case.id <- x[idx, "id"]
    } else if (nrow(x1) == 1) {
      case.id <- x1$id
    } else {
      idx <- which.min(abs(x1$birthYMD - control.df$birthYMD))
      case.id <- x1[idx, "id"]
    }
  }
  return(data.frame(control.id, case.id))
}

id.lookup <- data.frame(control.id = NULL, case.id = NULL)
for (i in 1:nrow(control)) {
  control.idx <- control[i, ]
  ret <- match(control.idx)
  id.lookup <- rbind(id.lookup, ret)
}
id.lookup1 <- subset(id.lookup, !is.na(case.id))

case.m <- subset(case, id %in% id.lookup1$case.id)
table(case.m$province, useNA = "ifany")
# 只分析江苏省和安徽省的病例，其它省份的病例较少
case.m <- subset(case.m, province %in% c("江苏省", "安徽省"))
control.m <- subset(control, id %in% id.lookup1$control.id)
control.m <- subset(control.m, province %in% c("江苏省", "安徽省"))
id.lookup <- subset(id.lookup1, control.id %in% control.m$id)
id.lookup$pair.id <- 1:nrow(id.lookup)
case.m <- case.m %>% 
  left_join(id.lookup, by = c("id" = "case.id")) %>%
  select(-control.id)
control.m <- control.m %>% 
  left_join(id.lookup, by = c("id" = "control.id")) %>%
  select(-case.id)
save(case.m, control.m, id.lookup, file = "output/case_control_matched.rda")

# matching criterion of difference in birthday of cases and controls -----------
df <- data.frame(day.diff <- c(5, 10, 15, 20, 25, 30), 
                 pairs <- c(1575, 2111, 2458, 2657, 2805, 2938))

library(ggplot2)
p <- ggplot(df, aes(day.diff, pairs))
p + geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = c(5, 10, 15, 20, 25, 30)) + 
  labs(x = "Difference between birthdays", y = "Matched case-control pairs") + 
  theme_classic(base_size = 14)

# select 15 for difference in birthday of cases and controls -------------------
# 按出生年月统计case和control频率
range(case.m$birthYMD)
x <- case.m %>%
  group_by(birthY, birthM) %>%
  dplyr::summarise(n = n())
x1 <- x %>% 
  mutate(birthYMD = as.Date(ISOdate(birthY, birthM, 1)))

library(ggplot2)
tiff(file = "figs/barplot_case_control_pairs.tiff", width = 10, height = 6, 
     units = "in", res = 300)
plot.case <- ggplot(x1, aes(x = birthYMD, y = n)) + 
  geom_bar(stat = "identity") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  scale_y_continuous(expand = c(0, 0)) + 
  labs(x = "Year", y = "No. of case-control pairs") + 
  theme_publication()
print(plot.case)
dev.off()

# case-control pairs的空间分布
# by province
x <- case.m %>%
  group_by(province) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(desc(n))
# by prefecture, to show this result on map
x1 <- case.m %>%
  mutate(prefecture = paste0(province, prefecture)) %>%
  group_by(prefecture) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(desc(n))
write.csv(x1, file = "output/case_control_pairs_prefecture.csv", row.names = F, 
          quote = F)
# matched cases by county
x2 <- case.m %>%
  mutate(county = paste0(province, prefecture, county)) %>%
  group_by(county) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(desc(n))

# matched controls by county
y2 <- control.m %>%
  mutate(county = paste0(province, prefecture, county)) %>%
  group_by(county) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(desc(n))

# case手术日期范围和control调查日期范围
range(case.m$surgery.date)
range(control.m$input.date)
