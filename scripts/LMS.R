#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    face emotion - binocular rivalry
#
#################################################

# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

# Library & dataset
rm(list=ls()) # remove all objects
# neaded packages
library(ggridges)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(afex)
library(hrbrthemes)
library(emmeans)
library(effects)
library(lmerTest)
## loading data 
load("04.data_preprocessing/Qualia_joystick.RData") 


## dataset preparation
velocity<-joystick_dataset%>% 
  select(subject,group,attribut,procedure,row,vector,x)%>%
  filter(vector != 0 )%>%
  mutate(direction = ifelse(  vector > 0 ,"plus","minus" ),
         count = 1)%>%
  select(subject,group,procedure,direction,row,x,vector,count)%>%
  mutate(x = (x+1)/2,
         x= ifelse(direction == "minus",1-x,x),
         x= round(x,2),
         vector= abs(vector),
         direction= case_when(direction=="minus" & procedure == "emotion"  ~ "to happy",
                              direction=="minus" & procedure == "gender"  ~ "to male",
                              direction=="plus" & procedure == "emotion"  ~ "to neutral",
                              direction=="plus" & procedure == "gender"  ~ "to female" ))%>%
  'colnames<-'(c("subject","group" , "procedure", "direction" ,"row", "joystick", "vector","count"))%>%
group_by(subject,procedure,direction,joystick)%>%
  summarise_at(vars(vector), list(mean))%>%
  'colnames<-'(c("subject" ,"procedure", "direction" ,"joystick", "vector"))%>%
  mutate(direction= case_when(direction == "to happy" ~ "neutral to happy",
                              direction == "to neutral" ~ "happy to neutral",
                              direction == "to male"  ~ "female to male",
                              direction == "to female"  ~ "male to female"))%>% 
  mutate(joystick = ifelse(  joystick > 0.5 ,"enter","exit" ))

velocity$direction<- factor(velocity$direction, levels = c("neutral to happy","happy to neutral","female to male","male to female"))


emotion<-velocity%>%
  filter(procedure == "emotion")
gender<-velocity%>%
  filter(procedure == "gender")


### LMS

# models ----
fit0 <- glmer(vector ~ direction + joystick + (direction + joystick|subject),
             data = emotion,
             family = Gamma(link= "log"),
             na.action = na.fail)
plot(allEffects(fit0))
summary(fit0)
qqnorm(residuals(fit0))

fit <- glmer(vector ~ direction * joystick + (direction * joystick|subject),
            data = emotion,
            family = Gamma(link= "log"),
            na.action = na.fail)

plot(allEffects(fit))
anova(fit0,fit)
summary(fit)
qqnorm(residuals(fit))

emmeans(fit,pairwise~ direction* joystick, adjust="bonf")

fit2 <- lmer(vector ~ direction* joystick  + (direction * joystick|subject),
            data = gender,
            na.action = na.fail)

simulate(fit)

options(scipen = 999)

plot(allEffects(fit2))
anova(fit2)
summary(fit2)
qqnorm(residuals(fit2))

emmeans(fit2,pairwise~ direction* joystick, adjust="bonf")


# plots
plot<-velocity%
plot%>% 
  mutate(joystick = ifelse(  joystick > 0.5 ,"enter","exit" )) %>%
  ggplot(aes(x=vector, y=direction, color=direction)  )+
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+
  facet_grid(joystick ~ .)

#################################################
# 
# END
#
#################################################
