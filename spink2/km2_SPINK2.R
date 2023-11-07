library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)

data <- read.table("km_X206310_at.csv", row.names=1,header=TRUE, sep=",",
                   stringsAsFactors=FALSE)
head(data)
data$Time <- data$Time
data$Time <- as.numeric(data$Time)
#survfit(Surv(Time, status) ~ 1, data = data)
median(data$score)
data <- mutate(data, status = ifelse((score < 0.5496449), 0, 1), 
              status = factor(status))
#write.csv(data, "update_data.csv")
data$status
data$status <- as.numeric(data$Status)

fit <- survfit(Surv(Time, Status) ~ data$Expression, data = data)
autoplot(fit, conf.int = F)
data

ggsurvplot(fit, xlab= "Time in days",
           pval = TRUE, conf.int = F,
           data = data, 
           title ="\t\t\t\t SPINK2" ,
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           dpi=300,
           risk.table = TRUE,
           ggtheme = theme_classic2(base_size = 15), # Change ggplot2 theme
           palette = c("red", "blue"),
           font.tickslab = c(15),
           font.family = "Arial")###c("#E7B800", "#2E9FDF")
res.cox <- coxph(Surv(Time, status) ~Expression, data = data)
summary(res.cox)
