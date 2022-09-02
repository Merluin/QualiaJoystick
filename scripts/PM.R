#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    face emotion - binocular rivalry
#
#################################################

rm(list=ls()) # remove all objects
# neaded packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpirate)
library(entropy)
library(ggeffects)
library(lsmeans)
library(afex)
library(hrbrthemes)
source("05.functions/add_object_to_rda.R")

## loading data ----
#load("dataset/Qualia_joystick.RData") 
load("04.data_preprocessing/Qualia_joystick.RData") 


pm<-joystick_dataset%>% 
  na.omit()%>%#cf function trial vec
  select(subject,group,procedure,row,x)%>%
  group_by( subject,group,procedure,row)%>%
  summarise_at(vars(x), list(mean))%>%
  group_by( subject,group,procedure)%>%
  summarise_at(vars(x), list(mean))%>%
  'colnames<-'(c("subject","group","procedure","pm"))%>%
  mutate(pm = round(pm,3))%>%
  as.data.frame()%>%
  mutate(attribut = case_when(pm>0  & procedure == "emotion" ~ "neutral",
                              pm<0 & procedure == "emotion" ~ "happy",
                              pm>0 & procedure == "gender" ~ "female",
                              pm<0 & procedure == "gender" ~ "male"))


# plot MT 


pm%>%
  'colnames<-'(c("subject","group","Condition","PM","attribut"))%>%
  ggplot( aes(x = Condition, y = PM)) +
  geom_pirate(aes(fill = Condition))+
  geom_hline(yintercept = 0)+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_discrete(name="",labels=c("Neutral/Happy", "Female/Male"))+
  facet_grid(group ~ .)
ggsave("07.figures/PM_bygroup_typical.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

pm%>%
  'colnames<-'(c("subject","group","Condition","PM","attribut"))%>%
  ggplot( aes(x = Condition, y = PM)) +
  geom_pirate(aes(fill = Condition))+
  geom_hline(yintercept = 0)+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_discrete(name="",labels=c("Neutral/Happy", "Female/Male"))
ggsave("07.figures/PM_typical.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# by gender
# scatter<-pm%>%
#   'colnames<-'(c("subject","group","Condition","PM","attribut"))%>%
#   ggplot( aes(x = Condition, y = PM)) +
#   geom_pirate(aes(fill = Condition))+
#   geom_hline(yintercept = 0)+
#   theme(text=element_text(size=16,  family="Helvetica"),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"))+
#   facet_grid(group~.)



emotion<-pm%>%
  filter(procedure=="emotion" )%>%
  select(pm)%>%
  t.test()
gender<-pm%>%
  filter(procedure=="gender")%>%
  select(pm)%>%
  t.test()
p.adjust(emotion$p.value,method ="bonferroni",2) < 0.05
p.adjust(gender$p.value,method ="bonferroni",2) < 0.05

# emotion_mob<-pm%>%
#   filter(procedure=="emotion" & group == "male")%>%
#   select(pm)%>%
#   t.test()
# gender_mob<-pm_full%>%
#   filter(procedure=="gender"& group == "female")%>%
#   select(pm)%>%
#   t.test()
# p.adjust(emotion_mob$p.value,method ="bonferroni",4) < 0.05
# p.adjust(gender_mob$p.value,method ="bonferroni",4) < 0.05


#a1 <- aov_ez("subject", "pm", data,  within = c("condition","direction"), between = "group")
a1 <- aov_ez("subject", "pm", pm,  within = "procedure", between = "group")


emmeans(a1,pairwise~ procedure, adjust="bonf")
sqrt(0.0296)

######## save data


pm_tylical<-pm

add_object_to_rda(pm_tylical,"04.data_preprocessing/typical.RData", overwrite = TRUE)

#################################################
# 
# END
#
#################################################