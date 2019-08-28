install.packages('installr')

install.packages('survival')

library(data.table)
library(survival)
library(survminer)
library(ggfortify)
 
data <- read.table('Section1_9.dat',col.names = c('Surv_Time','Treatment','Censored'),colClasses = c('numeric', 'numeric', 'numeric'))

surv_obj <- survfit(Surv(time = data$Surv_Time, 
                         event = data$Censored)~data$Treatment, 
                    subset = data$Treatment==2,
                    type="kaplan-meier")

print(summary(surv_obj))

surv.estimates <- surv_obj$surv
surv.times <- surv_obj$time

x <- seq(0, 50, length.out=1000)
y <- exp(-0.045*x)

plot(x=surv.times, y=surv.estimates, type='s', xlab="Time to infection", ylab="Survival estimate", 
     main = "Leukemia free-survival times (in months) for Autologous and Allogeneic Transplants",
     sub = "Based on Kaplan-Meier estimates", col = "blue", lty=2)
lines(x = x,y, lwd=1.5, lty=1)
legend(45,1,legend = c("Auto","Null Hypothesis"))

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

