rm(list = ls())
setwd("~/Downloads/FengYu/")

library(tidyverse)

load("dat/case_control_matched.rda")

case.m$CHD <- 1
control.m$CHD <- 0
CHD <- c(case.m$CHD, control.m$CHD)
M.edu <- c(case.m$M.edu, control.m$M.edu)
F.smoke <- c(case.m$F.smoke, control.m$F.smoke)
M.drink <- c(case.m$M.drink, control.m$M.drink)
M.pregnancy.flu <- c(case.m$M.pregnancy.flu, control.m$M.pregnancy.flu)
M.pregnancy.med <- c(case.m$M.pregnancy.med, control.m$M.pregnancy.med)
M.production.age <- c(case.m$M.production.age, control.m$M.production.age)
parity <- c(case.m$parity, control.m$parity)
gravidity <- c(case.m$gravidity, control.m$gravidity)

mydata <- data.frame(CHD, M.edu, F.smoke, M.drink, M.pregnancy.flu, M.pregnancy.med, 
                     M.production.age, parity, gravidity)
unique(mydata$M.edu)
table(mydata$M.edu)
mydata$M.edu1 <- ifelse(mydata$M.edu %in% c(1, 2, 3), 1, 0)
mydata$n.exp <- rowSums(mydata[, c("M.edu1", "F.smoke", "M.drink", "M.pregnancy.flu", "M.pregnancy.med")])

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

mylogit <- glm(CHD ~ n.exp, data = mydata, family = "binomial")
summary(mylogit)
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))
# adjusted odds ratios and 95% CI
mylogit1 <- glm(CHD ~ n.exp + M.production.age + parity + gravidity, 
                data = mydata, family = "binomial")
summary(mylogit1)
res <- exp(cbind(OR = coef(mylogit1), confint(mylogit1)))
library(car)
vif(mylogit1)

library(multcomp)
CHD.aov <- aov(CHD ~ n.exp + M.production.age + parity + gravidity, data = mydata)
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
                 aORs = c(1, res[2:3, 1]), 
                 ci.l = c(NA, res[2:3, 2]), 
                 ci.u = c(NA, res[2:3, 3]))

tiff(file = "fig/barplot.tiff", width = 5, height = 5, units = "in", res = 300)
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
        panel.grid = element_blank())

# # define arc coordinates
label.df <- data.frame(group = c(1.505, 2.5, 2), aORs = c(2.17, 3.28, 3.59))
p1 <- p + geom_text(data = label.df, aes(group, aORs), label = c("*", "***", "***"))
p1

library(pBrackets)
# 0 - 1
grid.brackets(93, 175, 193, 175, lwd = 1, type = 4)
# 1 - >=2
grid.brackets(193, 85, 294, 85, lwd = 1, type = 4)
# 0 - >=2
grid.brackets(93, 60, 294, 60, lwd = 1, type = 4)

dev.off()
