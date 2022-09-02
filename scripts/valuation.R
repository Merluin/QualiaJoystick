#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    face emotion - binocular rivalry
#
#################################################

############### Parameters ----
## library ----
rm(list=ls())
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(lsmeans)
library(afex)
library(ggpirate)
source("05.functions/add_object_to_rda.R")

## loading data ----

load("04.data_preprocessing/Qualia_joystick.RData") 


## Data
############### Valuation ----
VAVplot<-valuation_dataset%>%
  mutate(score= case_when(procedure=="Valence" & score== 1 ~ -3,
                          procedure=="Valence" & score== 2 ~ -2,
                          procedure=="Valence" & score== 3 ~ -1,
                          procedure=="Valence" & score== 4 ~ 0,
                          procedure=="Valence" & score== 5 ~ 1,
                          procedure=="Valence" & score== 6 ~ 2,
                          procedure=="Valence" & score== 7 ~ 3,
                          procedure=="Arousal" & score== 1 ~ 1,
                          procedure=="Arousal" & score== 2 ~ 2,
                          procedure=="Arousal" & score== 3 ~ 3,
                          procedure=="Arousal" & score== 4 ~ 4,
                          procedure=="Arousal" & score== 5 ~ 5,
                          procedure=="Arousal" & score== 6 ~ 6,
                          procedure=="Arousal" & score== 7 ~ 7))%>%
  na.omit()%>%
  group_by(subject,group,file,procedure)%>%
  summarise_at(vars(score), list(mean))%>%
  spread(procedure,score)%>%
  mutate(Arousal= round(Arousal,2),
         Valence= round(Valence,2))%>%
  as.data.frame()%>%
  mutate(emotion= case_when(file=="HF" | file=="HM" ~ "happy",
                            file=="NF" | file=="NM" ~ "neutral"),
         gender=  case_when(file=="HF" | file=="NF" ~ "female",
                            file=="HM" | file=="NM" ~ "male"))


# plot valuatioon  ----
#plot
ggplot(VAVplot, aes(x=Valence, y=Arousal, color=file, shape=group)) +
  geom_point(size=6, alpha=0.6)+
  coord_cartesian(ylim = c(1,7),xlim = c(-3,3))+
  labs(x="Valence valuation",y="Arousal Valuation",fill="Categories")+theme_classic()




############### ANOVA VALENCE ----
# dataset ----
temp<-VAVplot

x<-temp%>%
  select( subject,group,emotion, gender,Valence )

colnames(x)<-c("Subject","Group","Emotion","Gender","score")

x%>%
  group_by(Emotion)%>%
  summarise_at(vars(score), list(mean,sd))

x$Subject<-as.factor(x$Subject)

a1 <- aov_ez("Subject", "score", x,within = c("Gender", "Emotion"))
m1<-emmeans(a1,pairwise~ Group, adjust="bonf")

m1<-emmeans(a1,pairwise~ Emotion, adjust="bonf")

m1<-tidy(m1$contrasts)

t <- 18.16
df<- 27

t_to_d(t,df,paired = TRUE)


 a1 <- aov_ez("Subject", "score", x, between = "Group",within = c("Gender", "Emotion"))
# emmeans(a1,pairwise~ Group, adjust="bonf")
# emmeans(a1,pairwise~ Emotion, adjust="bonf")
 emmeans(a1,pairwise~ Group|Emotion, adjust="bonf")

# afex_plot(a1, x = "Emotion", trace = "Condition", error = "within",mapping = c("color", "fill"),
#           data_geom = geom_boxplot, data_arg = list(width = 0.4),
#           point_arg = list(size = 1.5), line_arg = list(size = 1))+theme_classic()
# m1<-emmeans(a1,pairwise~ Emotion,adjust="bonf")

############### ANOVA AROUSAL ----
# dataset ----
x<-temp%>%
  select( subject,group,emotion, gender, Arousal)
colnames(x)<-c("Subject","Group","Emotion","Gender","score")

x%>%
  group_by(Emotion)%>%
  summarise_at(vars(score), list(mean,sd))

x$Subject<-as.factor(x$Subject)
a1 <- aov_ez("Subject", "score", x, between = "Group",within = c("Gender", "Emotion"))
a1 <- aov_ez("Subject", "score", x,within = c("Gender", "Emotion"))
m2<-emmeans(a1,pairwise~ Emotion, adjust="bonf")
m3<-emmeans(a1,pairwise~ Gender|Emotion, adjust="bonf")
m4<-emmeans(a1,pairwise~ Group|Emotion, adjust="bonf")


m2<-tidy(m2$contrasts)
t_to_d(m2$statistic,m2$df,paired = TRUE)
t_to_d(17.99,38,paired = TRUE)

# emmeans(a1,pairwise~ Emotion, adjust="bonf")
# 
# afex_plot(a1, x = "Emotion", trace = "Condition", error = "within",mapping = c("color", "fill"),
#           data_geom = geom_boxplot, data_arg = list(width = 0.4),
#           point_arg = list(size = 1.5), line_arg = list(size = 1))+theme_classic()
# m1<-emmeans(a1,pairwise~ Emotion,adjust="bonf")

######## save data


val_typical<-VAVplot
save(val_typical,file="04.data_preprocessing/typical.RData")
add_object_to_rda(val_typical,"04.data_preprocessing/typical.RData", overwrite = TRUE)


#################################################
# 
# END
#
#################################################