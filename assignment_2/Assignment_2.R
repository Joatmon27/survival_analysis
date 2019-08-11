library(dplyr)
library(data.table)
library(readr)
library(survival)
library(survminer)

data <- read_delim('H:/Werk/Survival Analysis/Data sets/Section1_4.dat', 
                   delim = ' ', col_names = c('Time_To_Infection','Censored','Treatment'), 
                   col_types = cols(Time_To_Infection = 'n', Censored = 'n', Treatment = 'n'))

kmp<-survfit(Surv(data$Time_To_Infection,data$Censored)~data$Treatment,type="kaplan-meier")

df_summary <- fortify(kmp)

df_summary_surg <- df_summary[df_summary$strata == 1,]

df_summary_perc <- df_summary[df_summary$strata == 2,]

df_surg_head_tail <- data.frame(head(df_summary_surg))

df_perc_head_tail <- data.frame(head(df_summary_perc))

temp <- data.frame('...','...','...','...','...','...','...','...','...')
names(temp) <- names(df_summary)

highlight_surg <- df_summary_surg[df_summary_surg$time == 8.5,]

highlight_perc <- df_summary_perc[df_summary_perc$time == 8.5,]

df_surg_head_tail <- rbind(df_surg_head_tail, temp, highlight_surg, temp,tail(df_summary_surg))

df_perc_head_tail <- rbind(df_perc_head_tail, temp, highlight_perc, temp,tail(df_summary_perc))

drops <- c('upper','lower','strata')

df_surg_head_tail <- df_surg_head_tail[ , !(names(df_surg_head_tail) %in% drops)]

df_perc_head_tail <- df_perc_head_tail[ , !(names(df_perc_head_tail) %in% drops)]

names(df_surg_head_tail) <- c('Event Time','# at risk','# of events','# of cenosred','est. S(t)','Std Error')

names(df_perc_head_tail) <- c('Event Time','# at risk','# of events','# of cenosred','est. S(t)','Std Error')

print(kable(df_surg_head_tail))

print(kable(df_perc_head_tail))

ggsurv <- ggsurvplot(kmp , data = data, xlab="Months to infection",
                     ylab="Survival estimate",
                     legend.labs = c("Surgical", "Percutaneous"), linetype = 'strata') + 
  labs(title    = "Survival estimates of infection for renal insufficiency patients",
       subtitle = "Based on Kaplan-Meier estimates")

ggsurv <- ggpar(ggsurv, font.title    = c(14, "bold"),
                font.subtitle = c(12, "bold.italic"),        
                font.x        = c(11, "plain"),          
                font.y        = c(11, "plain"))

print(ggsurv)

kmp_na <-survfit(Surv(data$Time_To_Infection,data$Censored)~data$Treatment,type="fleming-harrington")

ggsurv_ch <- ggsurvplot(kmp_na, data = data, xlab="Months to infection",
                        ylab="Cumulative hazard estimate",
                        legend.labs = c("Surgical", "Percutaneous"), 
                        linetype = 'strata', fun="cumhaz") + 
  labs(title = "Cumulative hazard estimates for renal insufficiency patients",
       subtitle = "Based on Nelson-Aalen estimates")

ggsurv_ch <- ggpar(ggsurv_ch, 
                   font.title    = c(14, "bold"),
                   font.subtitle = c(12, "bold.italic"),        
                   font.x        = c(11, "plain"),          
                   font.y        = c(11, "plain"))

print(ggsurv_ch)

df_na_summary <- fortify(kmp_na)

cumhaz_45 <- kmp_na$cumhaz[(kmp_na$time == 4.5) & (df_na_summary$strata == 1)]

cumhaz_55 <- kmp_na$cumhaz[(kmp_na$time == 5.5) & (df_na_summary$strata == 1)]

print(cumhaz_45)
print(cumhaz_55)

hazard_estimate_surg <- cumhaz_45 * 0.5 + cumhaz_55 * 0.5

print(hazard_estimate_surg)

cumhaz_35 <- kmp_na$cumhaz[(kmp_na$time == 3.5) & (df_na_summary$strata == 2)]

cumhaz_65 <- kmp_na$cumhaz[(kmp_na$time == 6.5) & (df_na_summary$strata == 2)]

print(cumhaz_35)
print(cumhaz_65)

hazard_estimate_perc <- cumhaz_35 * 1.5/3 + cumhaz_65 * 1.5/3

print(hazard_estimate_perc)