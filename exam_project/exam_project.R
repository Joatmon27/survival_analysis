library(readxl)
library(survival)
library(survminer)

std <- read_xlsx('data/std_2019_pw.xlsx')

summary(std)

head(std)

kmp<-survfit(Surv(std$time,std$cens)~std$type,type="kaplan-meier")

ggsurv <- ggsurvplot(kmp , data = std, xlab="Days to re-infection",
                     ylab="Survival estimate",
                     legend.labs = c("Gonorrhea", "Chlamydia","Both"), linetype = 'strata') + 
  labs(title    = "Survival estimates for time to re-infection",
       subtitle = "Based on Kaplan-Meier estimates")

ggsurv <- ggpar(ggsurv, font.title    = c(14, "bold"),
                font.subtitle = c(12, "bold.italic"),        
                font.x        = c(11, "plain"),          
                font.y        = c(11, "plain"))

print(ggsurv)

res<-residuals(cox)
cox_snell<-(std_rr$cens-res)

surv_obj <- Surv(cox_snell,std_rr$cens)~std_rr$type_both+std_rr$type_chl

aa<-survfit(surv_obj,conf.type = 'none')

plot(aa,fun="cumhaz",main="Cox-Snell residual plot",xlab="residuals",ylab="estimated cumulative H(t)")
abline(0,1,lty=6)

# Stratifying on initial infection type
surv_obj_strat <- Surv(cox_snell,std_rr$cens)~std_rr$type

aa_strat <-survfit(surv_obj_strat,conf.type = 'none')

plot(aa_strat,fun="cumhaz",main="Cox-Snell residual plot",xlab="residuals",ylab="estimated cumulative H(t)",lty=c(1,2,3),col=c('green','blue','red'))
abline(0,1,lty=6,col='orange')
legend(legend=c('Gonorrhea','Both', 'Chlamydia','45` Line'),lty = c(1,2,3,6),col=c('green','blue','red','orange'),'topright')

##### Cox Snell

surv_reg_obj <- survreg(Surv(std_rr$time, std_rr$cens)~ std_rr$type_chl+std_rr$type_both+std_rr$condom, dist="weibull")

hat_sig <- surv_reg_obj$scale

hat_alpha <- 1/hat_sig

reg_linear <- surv_reg_obj$linear.predictor

reg_linear_mdf <- -reg_linear/hat_sig

tt <- cbind(Surv(std_rr$time, std_rr$cens))[,1]

cs_resid <- exp(reg_linear_mdf)*tt^(hat_alpha)

cs_fit = survfit(Surv(cs_resid,std_rr$cens)~1,type="kaplan-meier")

par(mfrow=c(1,1))

plot(x=cs_fit$time, y=-log(cs_fit$surv),type ="s",  
     ylab = "Estimated Cumulative H(t)",  
     xlab= "Cox–Snell Residuals",
     main="Cox–Snell residuals to assess the fit of the Weibull regression model")

lines(c(0,3),c(0,3), lty=2)

#######################################
## Part B
######################################

kmp<-survfit(Surv(std$time,std$cens)~std$type+std$condom,type="kaplan-meier")

summary(kmp)

ggsurv <- ggsurvplot(kmp , data = std, xlab="Days to re-infection",
                     ylab="Survival estimate",
                     legend.labs = c("Gonorrhea-No_Condom","Gonorrhea-Condom_Always","Both-No_Condom","Both-Condom_Always","Chlamydia-No_Condom","Chlamydia-Condom_Always"), linetype = 'strata') + 
  labs(title    = "Survival estimates for time to re-infection",
       subtitle = "Based on Kaplan-Meier estimates")

ggsurv <- ggpar(ggsurv, font.title    = c(14, "bold"),
                font.subtitle = c(12, "bold.italic"),        
                font.x        = c(11, "plain"),          
                font.y        = c(11, "plain"))

print(ggsurv)

kmp<-survfit(Surv(std$time,std$cens)~std$type+std$condom, subset = std$type==1,type="kaplan-meier")

ggsurv <- ggsurvplot(kmp , data = std, xlab="Days to re-infection",
                     ylab="Survival estimate",
                     legend.labs = c("Gonorrhea-No_Condom","Gonorrhea-Condom_Always"), linetype = 'strata') + 
  labs(title    = "Survival estimates for time to re-infection",
       subtitle = "Based on Kaplan-Meier estimates")

ggsurv <- ggpar(ggsurv, font.title    = c(14, "bold"),
                font.subtitle = c(12, "bold.italic"),        
                font.x        = c(11, "plain"),          
                font.y        = c(11, "plain"))

print(ggsurv)

std <- read_xlsx('data/std_2019_pw.xlsx')

kmp<-survfit(Surv(std$time,std$cens)~std$type+std$condom, subset = std$type==2,type="kaplan-meier")

ggsurv <- ggsurvplot(kmp , data = std, xlab="Days to re-infection",
                     ylab="Survival estimate",
                     legend.labs = c("Both-No_Condom","Both-Condom_Always"), linetype = 'strata') + 
  labs(title    = "Survival estimates for time to re-infection",
       subtitle = "Based on Kaplan-Meier estimates")

ggsurv <- ggpar(ggsurv, font.title    = c(14, "bold"),
                font.subtitle = c(12, "bold.italic"),        
                font.x        = c(11, "plain"),          
                font.y        = c(11, "plain"))

print(ggsurv)

kmp<-survfit(Surv(std$time,std$cens)~std$type+std$condom, subset = std$type==3,type="kaplan-meier")

ggsurv <- ggsurvplot(kmp , data = std, xlab="Days to re-infection",
                     ylab="Survival estimate",
                     legend.labs = c("Chlamydia-No_Condom","Chlamydia-Condom_Always"), linetype = 'strata') + 
  labs(title    = "Survival estimates for time to re-infection",
       subtitle = "Based on Kaplan-Meier estimates")

ggsurv <- ggpar(ggsurv, font.title    = c(14, "bold"),
                font.subtitle = c(12, "bold.italic"),        
                font.x        = c(11, "plain"),          
                font.y        = c(11, "plain"))

print(ggsurv)
