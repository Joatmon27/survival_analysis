library(data.table)
library(survival)

section1_11 <- fread('Section1_11.dat')

section1_11_cox <- coxph(Surv(Death,Censored)~Tumor,data=section1_11,method="breslow")

res<-residuals(section1_11_cox)
cox_snell<-(section1_11$Censored-res)

# Does not want to accept surv without a formula arg, so we jippo it a bit
section1_11_temp <- section1_11
section1_11_temp$Tumor <- 1

surv_obj <- Surv(cox_snell,section1_11$Censored)~section1_11_temp$Tumor

aa<-survfit(surv_obj,conf.type = 'none')

plot(aa,fun="cumhaz",main="Cox-Snell residual plot",xlab="residuals",ylab="estimated cumulative H(t)")
abline(0,1,lty=6)

# Stratifying on tumor type
surv_obj_strat <- Surv(cox_snell,section1_11$Censored)~section1_11$Tumor

aa_strat <-survfit(surv_obj_strat,conf.type = 'none')

plot(aa_strat,fun="cumhaz",main="Cox-Snell residual plot",xlab="residuals",ylab="estimated cumulative H(t)",lty=c(1,2))
abline(0,1,lty=6)
legend(legend=c('Aneuploid','Diploid','45` Line'),lty = c(1,2,6),'topright')

#Data prep for two tumor types
aneuploid<-summary(survfit(Surv(section1_11$Death,section1_11$Censored)~section1_11$Tumor,subset=(Tumor==1),data=section1_11,conf.type="none"))
diploid<-summary(survfit(Surv(section1_11$Death,section1_11$Censored)~section1_11$Tumor,subset=(Tumor==2),data=section1_11,conf.type="none"))
naan<-cumsum(aneuploid$n.event/aneuploid$n.risk)
nadi<-cumsum(diploid$n.event/diploid$n.risk)
logan<-log(naan)
logdi<-log(nadi)

plot(sort(aneuploid$time),sort(logan),type="s",main="Check for proportional hazards assumption",
     xlab="time on study",ylab="log cumulative hazard rates",ylim=c(-4,1),xlim=c(0,25))
lines(sort(diploid$time),sort(logdi),type="s",lty=4)
legend(x=15,y=1,legend=c("Aneuploid","Diploid"),lty=c(1,4))

all.times<-sort(unique(c(aneuploid$time,diploid$time)))
log.H<-matrix(0,nrow=length(all.times),ncol=2)
res<-cbind(all.times,log.H)
dimnames(res)<-list(c(),c("all times","logH.aneuploid","logH.diploid"))
res[match(aneuploid$time,all.times),"logH.aneuploid"]<-logan
res[match(diploid$time,all.times),"logH.diploid"]<-logdi
for(i in 1:length(all.times)){
  if(res[i,"logH.aneuploid"]==0)
    res[i,"logH.aneuploid"]<-res[(i-1),"logH.aneuploid"]
  if(res[i,"logH.diploid"]==0)
    res[i,"logH.diploid"]<-res[(i-1),"logH.diploid"]
}
plot(x=all.times,y=(res[,"logH.aneuploid"]-res[,"logH.diploid"]),type="s",xlim=c(0,25),ylim=c(-1.5,1),
     main="Check for proportional hazards assumption - ploidy group",xlab="time on study",
     ylab="difference in log cumulative hazard rates")
abline(h=0,lty=2)

plot(x=exp(res[,"logH.aneuploid"]),y=exp(res[,"logH.diploid"]),type="s",ylim=c(0,0.9),
     main="Anderson plot for proportional hazards assumption",xlab="Aneuploid",ylab="Diploid")
abline(0,1,lty=7)

risk<-0.4609544*section1_11$Tumor
dev.res<-residuals(coxph(Surv(section1_11$Death,section1_11$Censored)~section1_11$Tumor,method="breslow"),type="deviance")
plot(x=risk,y=dev.res,main="Plot for outliers - deviance residuals",xlab="risk scores",ylab="deviance residuals")
abline(h=1.96,lty=7)

section1_6 <- fread('Section1_6.dat')

section1_6_cox <- coxph(Surv(V17,V18)~V5,data=section1_6,method="breslow")

risk_burn <-2.127598*section1_6$V5
mart_res_burn <- residuals(coxph(Surv(section1_6$V17,section1_6$V18)~section1_6$V4,method="breslow"),type="martingale")
plot(x=risk_burn,y=mart_res_burn,main="Plot for outliers - martingale residuals",xlab="risk scores",ylab="martingale residuals")
abline(h=1.96,lty=7)
