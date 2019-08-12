library(here)
library(dplyr)
library(data.table)
library(survival)

section1_4 <- fread(here('Section1_4.dat'), 
                   sep = ' ', col.names = c('Time_To_Infection','Censored','Treatment'), 
                   colClasses = c('numeric', 'numeric', 'numeric'))

kmp<-survfit(Surv(section1_4$Time_To_Infection,section1_4$Censored)~section1_4$Treatment,type="kaplan-meier")

df_summary <- fortify(kmp)

print(df_summary)

print(kmp, print.rmean=TRUE)