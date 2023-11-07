library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)

data <- read.table("km_226545_at.csv", row.names=1,header=TRUE, sep=",",
                   stringsAsFactors=FALSE)
k1 <- which(data$Risk=="I" &  data$Time > 365 & data$Time < 3650)
length(k1)
data_k <- data[k1,]
dim(data_k)
median(data_k$Time)
data$Time
data$Time <- as.numeric(data$Time)
#survfit(Surv(Time, status) ~ 1, data = data)
median(data$score)
data <- mutate(data, status = ifelse((score < 0.5539854), 0, 1), 
              status = factor(status))
#write.csv(data, "update_data.csv")
data$status
data$status <- as.numeric(data$Status)

fit <- survfit(Surv(Time, status) ~ data$Expression, data = data)
autoplot(fit, conf.int = F)
data

ggsurvplot(fit, xlab= "Time in days",
           pval = TRUE, conf.int = F,
           data = data, 
           title ="\t\t\t\t CD109" ,
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           dpi=600,
           risk.table = TRUE,
           palette = c( "red" , "blue"),###c("#E7B800", "#2E9FDF"),
           font.family = "Arial",
           ggtheme = theme_classic2(base_size = 15), # Change ggplot2 theme
           font.tickslab = c(15),
           text = element_text(size = 15))
          
res.cox <- coxph(Surv(Time, status) ~Expression, data = data)
summary(res.cox)
