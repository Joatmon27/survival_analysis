library(data.table)
library(survival)
library(survminer)
library(ggfortify)
 
data <- fread('Section1_9.dat',col.names = c('Surv_Time','Treatment','Censored'),colClasses = c('numeric', 'numeric', 'numeric'))

surv_obj <- survfit(Surv(time = data$Surv_Time, 
                         event = data$Censored)~offset(exp(-0.045*data$Surv_Time)), 
                    subset = data$Treatment==1,
                    type="kaplan-meier")

ggsurv <- ggsurvplot(surv_obj , data = data, xlab="Time to infection",
                     ylab="Survival estimate",
                     legend.labs = c("Auto"), linetype = 'strata') + 
  labs(title    = "Leukemia free-survival times (in months) for Autologous and Allogeneic Transplants",
       subtitle = "Based on Kaplan-Meier estimates")

ggsurv <- ggpar(ggsurv, font.title    = c(14, "bold"),
                font.subtitle = c(12, "bold.italic"),        
                font.x        = c(11, "plain"),          
                font.y        = c(11, "plain"))

print(ggsurv)

