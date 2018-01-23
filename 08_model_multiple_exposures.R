rm(list = ls())

load("output/case_control_matched.rda")

library(tidyverse)

cases <- case.m %>% 
  mutate(CHD = 1) %>% 
  dplyr::select(pair.id, CHD, M.production.age, parity, gravidity, M.edu, M.smoke, 
                M.drink, M.pregnancy.flu, M.med) %>% 
  arrange(pair.id)

controls <- control.m %>% 
  mutate(CHD = 0) %>% 
  dplyr::select(pair.id, CHD, M.production.age, parity, gravidity, M.edu, M.smoke, 
                M.drink, M.pregnancy.flu, M.med) %>% 
  arrange(pair.id)

mydata <- rbind(cases, controls)

# 母亲生子年龄
ddply(mydata, ~ CHD, summarise, mean = round(mean(M.production.age), 2), 
      sd = round(sd(M.production.age), 2))
mydata <- mydata %>%
  mutate(M.production.age = case_when(
    .$M.production.age < 20 ~ "<20",
    .$M.production.age > 30 ~ ">30",
    TRUE ~ "20~30"
  ))
mydata$M.production.age <- factor(mydata$M.production.age, 
                                  levels = c("20~30", "<20", ">30"))
table(mydata$M.production.age, useNA = "ifany")

# 生产次数
mydata$parity <- factor(mydata$parity)
table(mydata$parity, useNA = "ifany")

# 怀孕次数
table(mydata$gravidity, useNA = "ifany")
mydata <- mydata %>%
  mutate(gravidity = case_when(
    .$gravidity >= 3 ~ ">=3",
    TRUE ~ as.character(gravidity)
  ))
mydata$gravidity <- factor(mydata$gravidity, levels = c("1", "2", ">=3"))
table(mydata$gravidity, useNA = "ifany")

# 母亲教育水平
table(mydata$M.edu, useNA = "ifany")
mydata$M.edu1 <- ifelse(mydata$M.edu %in% c(1, 2, 3), 1, 0)
table(mydata$M.edu1, useNA = "ifany")

# 母亲是否吸烟
table(mydata$M.smoke, useNA = "ifany")

# 母亲是否喝酒
table(mydata$M.drink, useNA = "ifany")

# 母亲孕期是否感冒
table(mydata$M.pregnancy.flu, useNA = "ifany")

# 母亲是否用药
table(mydata$M.med, useNA = "ifany")

# number of exposures
mydata$n.exp <- rowSums(mydata[, c("M.edu1", "M.smoke", "M.drink", 
                                   "M.pregnancy.flu", "M.med")])

# case和control暴露风险因子个数
x <- xtabs(~ CHD + n.exp, data = mydata)
round(x / rowSums(x) * 100, 2)

mydata <- mydata %>%
  mutate(n.exp = as.character(n.exp)) %>%
  mutate(n.exp = case_when(
    .$n.exp == "0" ~ "0",
    .$n.exp == "1" ~ "1",
    TRUE ~ ">=2"
  ))
mydata$n.exp <- factor(mydata$n.exp, levels = c("0", "1", ">=2"))

library(survival)
mylogit <- clogit(CHD ~ n.exp + M.production.age + parity + 
                    gravidity + strata(pair.id), data = mydata)
summary(mylogit)
res <- exp(cbind(OR = coef(mylogit), confint(mylogit)))
library(car)
vif(mylogit)

library(multcomp)
CHD.aov <- aov(CHD ~ n.exp + M.production.age + parity + gravidity + strata(pair.id), 
               data = mydata)
# multiple comparisions with a control
CHD.mc <- glht(CHD.aov, linfct = mcp(n.exp = "Dunnett"), alternative = "greater")
# all pairwise comparision
CHD.mc <- glht(CHD.aov, linfct = mcp(n.exp = "Tukey"), alternative = "greater")
summary(CHD.mc, test = adjusted(type = "none"))
summary(CHD.mc, test = adjusted(type = "bonferroni"))
# summary(CHD.mc, test = adjusted(type = "single-step"))
# summary(CHD.mc, test = adjusted(type = "free"))
plot(CHD.mc)

# dose response analyses
# detect a dose related trend
# trend test: test for a dose reponse effect
CHD.mc2 <- glht(CHD.aov, linfct = mcp(n.exp = "Williams"), alternative = "greater")
summary(CHD.mc2, test = adjusted(type = "single-step"))

CHD.mc3 <- glht(CHD.aov, linfct = mcp(n.exp = "Marcus"), alternative = "greater")
summary(CHD.mc3, test = adjusted(type = "single-step"))

# plot
pd <- data.frame(group = factor(c("0", "1", ">=2"), levels = c("0", "1", ">=2")), 
                 aORs = c(1, res[1:2, 1]), 
                 ci.l = c(NA, res[1:2, 2]), 
                 ci.u = c(NA, res[1:2, 3]))

tiff(file = "figs/barplot.tiff", width = 5, height = 5, units = "in", res = 300)
p <- ggplot(pd, aes(group, aORs)) + 
  geom_bar(aes(fill = "grey"), stat = "identity", width = .5) + 
  geom_errorbar(aes(ymin = ci.l, ymax = ci.u), width = 0.1, color = "grey30") + 
  geom_smooth(method = "lm", se = FALSE, color = "grey20", size = 0.3, linetype = 2, 
              aes(group = 1)) + 
  scale_x_discrete(labels = c("0", "1", expression("" >= 2))) + 
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) + 
  labs(x = "Number of maternal risk factors", y = "Adjusted odds ratio") + 
  scale_fill_manual(values = c("grey60")) + 
  theme_classic() + 
  theme(legend.position = "none", 
        panel.grid = element_blank(), 
        axis.title = element_text(face = "bold"))

# define arc coordinates
label.df <- data.frame(group = c(1.503, 2.5, 2), aORs = c(2.05, 3.05, 3.36))
p1 <- p + geom_text(data = label.df, aes(group, aORs), label = c("***", "***", "***"))
p1

library(pBrackets)
# 0 - 1
grid.brackets(93, 185, 193, 185, lwd = 1, type = 4)
# 1 - >=2
grid.brackets(193, 105, 294, 105, lwd = 1, type = 4)
# 0 - >=2
grid.brackets(93, 80, 294, 80, lwd = 1, type = 4)

dev.off()
