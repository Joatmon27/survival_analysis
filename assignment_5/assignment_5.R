library(data.table)
library(readxl)
library(survival)

burn_data <- read_xlsx('Section1_6.xlsx')

surv_obj<-survfit(Surv(T3,D3)~Z1, data = burn_data)
plot(surv_obj,xlab="time to infection (days)",lty=c(1,2),ylab="estimated survival",main="Kaplan-Meier estimator for burn patients")
legend(x=35,y=0.98,legend=c("Routine dressing","Chlorhexidine gluconate dressing"),lty=c(1,2))

cox_obj_bres <- coxph(Surv(T3,D3)~Z1, data = burn_data, method="breslow")
summary(cox_obj_bres)

cox_obj_efr <- coxph(Surv(T3,D3)~Z1, data = burn_data, method="efron")
summary(cox_obj_efr)

rat_data <- read_xlsx('rat_data.xlsx')

cox_obj_rat <- coxph(Surv(Death_Times, Censored)~Z1+Z2, data = rat_data, method="breslow")
summary(cox_obj_rat)

exp(-1.81196+(1.96*0.55971))

exp(-1.81196-(1.96*0.55971))

exp(-3.525737+1.81196)

0.3132770447+0.5749487185-(2*(0.2439763104))

exp((-3.525737+1.81196)+(1.96*sqrt(0.4002731)))

exp((-3.525737+1.81196)-(1.96*sqrt(0.4002731)))
