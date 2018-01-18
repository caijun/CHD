rm(list = ls())
setwd("~/Downloads/FengYu/")

source("src/helper.R")

library(tidyverse)

load("dat/case_control_matched.rda")

case.m$CHD <- 1
control.m$CHD <- 0
CHD <- c(case.m$CHD, control.m$CHD)
M.edu <- c(case.m$M.edu, control.m$M.edu)
M.pregnancy.smoke <- c(case.m$M.pregnancy.smoke, control.m$M.pregnancy.smoke)
M.drink <- c(case.m$M.drink, control.m$M.drink)
M.pregnancy.flu <- c(case.m$M.pregnancy.flu, control.m$M.pregnancy.flu)
M.pregnancy.med <- c(case.m$M.pregnancy.med, control.m$M.pregnancy.med)
M.production.age <- c(case.m$M.production.age, control.m$M.production.age)
parity <- c(case.m$parity, control.m$parity)
gravidity <- c(case.m$gravidity, control.m$gravidity)

mydata <- data.frame(CHD, M.edu, M.pregnancy.smoke, M.drink, M.pregnancy.flu, M.pregnancy.med, 
                     M.production.age, parity, gravidity)
mydata$M.edu1 <- ifelse(mydata$M.edu == 2 | mydata$M.edu == 3, 1, 0)
mydata$n.exp <- rowSums(mydata[, c("M.edu1", "M.pregnancy.smoke", "M.drink", "M.pregnancy.flu", "M.pregnancy.med")])

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

library(multcomp)
CHD.aov <- aov(CHD ~ n.exp + M.production.age + parity + gravidity, data = mydata)
CHD.mc <- glht(CHD.aov, linfct = mcp(n.exp = "Dunnett"), alternative = "greater")
summary(CHD.mc, test = adjusted(type = "single-step"))
summary(CHD.mc, test = adjusted(type = "free"))
plot(CHD.mc)

CHD.mc2 <- glht(CHD.aov, linfct = mcp(n.exp = "Williams"), alternative = "greater")
summary(CHD.mc2, test = adjusted(type = "single-step"))

CHD.mc3 <- glht(CHD.aov, linfct = mcp(n.exp = "Marcus"), alternative = "greater")
summary(CHD.mc3, test = adjusted(type = "single-step"))

# plot
pd <- data.frame(group = factor(c("0", "1", ">=2"), levels = c("0", "1", ">=2")), 
                 aORs = c(1, res[2:3, 1]), 
                 ci.l = c(NA, res[2:3, 2]), 
                 ci.u = c(NA, res[2:3, 3]))

tiff(file = "barplot.tiff", width = 5, height = 5, units = "in", res = 300)
p <- ggplot(pd, aes(group, aORs)) + 
  geom_bar(aes(fill = "grey"), stat = "identity", width = .5) + 
  geom_errorbar(aes(ymin = ci.l, ymax = ci.u), width = 0.1, color = "grey30") + 
  geom_smooth(method = "lm", se = FALSE, color = "grey20", size = 0.3, linetype = 2, 
              aes(group = 1)) + 
  scale_x_discrete(labels = c("0", "1", expression("" >= 2))) + 
  labs(x = "Number of maternal risk factors", y = "Adjusted odds ratio") + 
  coord_cartesian(ylim = c(0, 6)) +
  scale_fill_manual(values = c("grey60")) +
  theme_publication() + 
  theme(legend.position = "none", 
        panel.grid = element_blank())

# # define arc coordinates
# arc.df <- data.frame(group = c(1, 1, 2, 2), aORs = c(1.2, 3, 3, 2))
# 
# p1 <-
#   p + geom_line(data = arc.df, aes(group, aORs), lty = 2)

label.df <- data.frame(group = c(1.5, 2.52, 2), aORs = c(2.6, 4.8, 5.4))
p1 <- p + geom_text(data = label.df, aes(group, aORs), label = "***")
p1

library(pBrackets)
grid.brackets(103, 211, 196, 211, lwd = 1, type = 4)
grid.brackets(200, 115, 290, 115, lwd = 1, type = 4)
grid.brackets(103, 90, 290, 90, lwd = 1, type = 4)

dev.off()




# Dose Response Analysis from Frank Bretz, Torsten Hothorn, Peter Westfall-Multiple Comparisons Using R-Chapman and Hall_CRC (2010)

data("litter")
litter.aov <- aov(weight ~ dose + gesttime + number, data = litter)
litter.mc <- glht(litter.aov, linfct = mcp(dose = "Dunnett"), alternative = "less")
summary(litter.mc, test = adjusted(type = "single-step"))
summary(litter.mc, test = adjusted(type = "free"))

litter.mc2 <- glht(litter.aov, linfct = mcp(dose = "Williams"), alternative = "less")
summary(litter.mc2)

litter.mc3 <- glht(litter.aov, linfct = mcp(dose = "Marcus"), alternative = "less")
summary(litter.mc3)
