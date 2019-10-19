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

