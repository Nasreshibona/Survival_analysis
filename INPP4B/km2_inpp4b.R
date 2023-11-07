library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)

data <- read.table("km_X235046_at.csv", row.names=1,header=TRUE, sep=",", 
                   stringsAsFactors=FALSE)
head(data)
data$Time <- data$Time
data$Time <- as.numeric(data$Time)
median(data$score)
data <- mutate(data, status = ifelse((score < 0.3342826), 0, 1), 
              status = factor(status))
#write.csv(data, "update_data.csv")
data$status
data$status <- as.numeric(data$Status)

fit <- survfit(Surv(Time, status) ~ data$Expression, data = data)
autoplot(fit, conf.int = F)
data

ggsurvplot(fit, xlab="Time in days",
           pval = TRUE, conf.int = F,
           data = data, 
           title ="\t\t\t\t INPP4B" ,
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           dpi=600,
           risk.table = TRUE,
           ggtheme = theme_classic2(base_size = 15), # Change ggplot2 theme
           palette = c("red", "blue"),
           font.tickslab = c(15),
           font.family= "Arial")###c("#E7B800", "#2E9FDF")


res.cox <- coxph(Surv(Time, Status) ~Expression, data = data)
summary(res.cox)
