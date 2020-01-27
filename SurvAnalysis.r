install.packages("survminer")
library(survival)
library(survminer)
library(dplyr)



d1<-data(GBSG2, package = "TH.data")
levels(GBSG2$tgrade)<-c("1","2","3")
GBSG2$horTh
levels(GBSG2$menostat)<-c(0,1)
d1<-GBSG2
d1<-d1[,c(9,2,3,4,5,6,7,8,1,10)]
d1<-d1[order(d1$time),]
write.csv(d1, file="cancer_data.csv")

# Dichotomize age and change data labels
ovarian$rx <- factor(ovarian$rx, 
                     levels = c("1", "2"), 
                     labels = c("A", "B"))
ovarian$resid.ds <- factor(ovarian$resid.ds, 
                           levels = c("1", "2"), 
                           labels = c("no", "yes"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps, 
                          levels = c("1", "2"), 
                          labels = c("good", "bad"))

# Data seems to be bimodal
hist(ovarian$age) 
ovarian <- ovarian %>% mutate(age_group = ifelse(age >=50, "old", "young"))
ovarian$age_group <- factor(ovarian$age_group)


# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)
surv_object 
fit1 <- survfit(surv_object ~ rx, data = ovarian)
summary(fit1)

ggsurvplot(fit1, data = ovarian, pval = TRUE)
# Examine prdictive value of residual disease status
fit2 <- survfit(surv_object ~ resid.ds, data = ovarian)
ggsurvplot(fit2, data = ovarian, pval = TRUE)

# Fit a Cox proportional hazards model
fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps, 
                   data = ovarian)
ggforest(fit.coxph, data = ovarian)

