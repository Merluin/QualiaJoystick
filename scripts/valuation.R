#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    valence and arousal - binocular rivalry
#
#################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggeffects)
library(afex)
library(emmeans)
library(broom)
library(effectsize)

# Data --------------------------------------------------------------------

load("data/Qualia_joystick.RData") 

# Valuation
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
  mutate(Emotion= case_when(file=="HF" | file=="HM" ~ "happy",
                            file=="NF" | file=="NM" ~ "neutral"),
         Gender=  case_when(file=="HF" | file=="NF" ~ "female",
                            file=="HM" | file=="NM" ~ "male"),
         Face=  case_when(file=="HF"  ~ "Happy female",
                           file=="NF"  ~ "Neutral female",
                           file=="HM"  ~ "Happy male",
                           file=="NM"  ~ "Neutral male"),
         group=  case_when(group=="female"  ~ "Female participants",
                           group=="male"  ~ "Male participants"))

# Plots -------------------------------------------------------------
VAVplot%>%
  group_by(subject, Emotion)%>%
  summarise_at(vars(Arousal,Valence), list(mean))%>%
  ggplot( aes(x=Valence, y=Arousal, color=Emotion, shape = Emotion)) +
  geom_point(size=4, alpha=0.6)+
  coord_cartesian(ylim = c(1,7),xlim = c(-3,3))+
  labs(x="Valence",y="Arousal",fill="Categories")+theme_classic()+
#facet_grid(.~group)+
  coord_fixed(ratio = 1)+
  xlab("Valence")+
  ylab("Arousal")+
  theme(axis.title.x = element_text(size = 16, family="Arial"),
        axis.title.y = element_text(size = 16, family="Arial"),
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 14, face = "bold", family="Arial"),
        strip.text.y = element_text(size = 14, face = "bold", family="Arial"),
        panel.background = element_rect(fill = "white", color = NA))+
  theme(legend.position = "right")+
  scale_colour_manual(values=c("#008b39","#fd345a"))
  

# Stats -------------------------------------------------------------

# Valenza
x<-VAVplot%>%
  select( subject,group,Emotion, Gender,Valence )%>%
'colnames<-'(c("Subject","Group","Emotion","Gender","score"))%>%
  mutate(Subject = as.factor(Subject))

# summary
x%>%
  group_by(Emotion)%>%
  summarise_at(vars(score), list(mean,sd))

# ANOVA Valence
a1 <- aov_ez("Subject", "score", x,within = c("Gender", "Emotion"))
m1<-emmeans(a1,pairwise~ Emotion, adjust="bonf")

m1<-tidy(m1$contrasts)

t <- m1$statistic
df<- m1$df
t_to_d(t,df,paired = TRUE)


# Arousal
x<-VAVplot%>%
  select( subject,group,emotion, gender,Arousal )%>%
  'colnames<-'(c("Subject","Group","Emotion","Gender","score"))%>%
  mutate(Subject = as.factor(Subject))

# summary
x%>%
  group_by(Emotion)%>%
  summarise_at(vars(score), list(mean,sd))

# ANOVA Arousal
a1 <- aov_ez("Subject", "score", x,within = c("Gender", "Emotion"))
m2<-emmeans(a1,pairwise~ Emotion, adjust="bonf")
m3<-emmeans(a1,pairwise~ Gender|Emotion, adjust="bonf")

m2<-tidy(m2$contrasts)
t_to_d(m2$statistic,m2$df,paired = TRUE)

m3<-tidy(m3$contrasts)
t_to_d(m3$statistic[1],m3$df[1],paired = TRUE)
t_to_d(m3$statistic[2],m3$df[2],paired = TRUE)

#################################################
# 
# END
#
#################################################