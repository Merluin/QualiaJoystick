#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    Movement transition measure  - binocular rivalry
#
#################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggeffects)
library(afex)
library(emmeans)

# Data --------------------------------------------------------------------

load("data/Qualia_joystick.RData") 

# dataset  MT
velocity<-joystick_dataset%>% #cf function trial vec
  select(subject,group,attribut,procedure,vector,x)%>%
  filter(vector != 0 )%>%
  mutate(direction = ifelse(  vector > 0 ,"plus","minus" ),
         count = 1)%>%
  select(subject,group,procedure,direction,x,vector,count)%>%
  mutate(x = (x+1)/2,
         x= ifelse(direction == "minus",1-x,x),
         x= ifelse(x >= .5,"enter","exit"),
         vector= abs(vector),
         vector = round(vector,3),
         direction= case_when(direction=="minus" & procedure == "emotion"  ~ "to happy",
                              direction=="minus" & procedure == "gender"  ~ "to male",
                              direction=="plus" & procedure == "emotion"  ~ "to neutral",
                              direction=="plus" & procedure == "gender"  ~ "to female" ))%>%
  'colnames<-'(c("subject","group" , "procedure", "direction" , "joystick", "vector","count"))%>%
  group_by(subject,group,procedure,direction,joystick)%>%
  summarise_at(vars(vector,count), list(mean,sum))%>%
  select(subject,group ,procedure,direction,joystick,vector_fn1,count_fn2)%>%
  'colnames<-'(c("subject","group" , "procedure", "direction" , "joystick", "vector","count"))%>%
  data.frame()

# Plots -------------------------------------------------------------

# barplot
velocity%>%
  select(-count)%>%
  filter(procedure=="emotion")%>%
  mutate( direction= case_when(direction == "to happy"   & joystick == "enter" ~ "happy",
                               direction == "to neutral" & joystick == "enter" ~ "neutral",
                               direction == "to male"    & joystick == "enter" ~ "male",
                               direction == "to female"  & joystick == "enter"  ~ "female",
                               direction == "to happy"   & joystick == "exit" ~ "neutral",
                               direction == "to neutral" & joystick == "exit"~ "happy",
                               direction == "to male"    & joystick == "exit"~ "female",
                               direction == "to female"  & joystick == "exit" ~ "male"))%>%
  group_by(group,direction,joystick)%>%
  summarise_at(vars(vector), list(mean,sd))%>%
  mutate(direction = factor(direction, levels = c("neutral","happy")),
         joystick = ifelse(joystick == "enter","Formation", "Disformation"))%>%
  'colnames<-'(c("Group","Percept","joystick","Speed","sd"))%>%
  ggplot(aes(fill=Percept, y=Speed, x=Group)) + 
  geom_bar(position=position_dodge(), stat="identity", width=0.5)+
  geom_errorbar( aes(x=Group, ymin=Speed-sd, ymax=Speed+sd),position = position_dodge(width=0.5), width=0.2, size=.5)+
  facet_grid("joystick")+
  theme(text=element_text(size=16,  family="Helvetica"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

ggsave("figures/MT_bar.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# Emotion plot
MT_emotion<-velocity%>%
  select(-count)%>%
  filter(procedure=="emotion")%>%
  mutate( direction= case_when(direction == "to happy" ~ "neutral to happy",
                               direction == "to neutral" ~ "happy to neutral"),
          joystick = case_when( joystick == "enter" ~ "Formation",
                                joystick == "exit" ~ "Disformation"))%>%
  spread(direction,vector)%>%
  data.frame()%>%
  'colnames<-'(c("subject", "Gender","procedure","Joystick","HtN","NtH"))

MT_emotion%>%
  ggplot(aes(y=HtN,x=NtH) )+
  geom_point(aes(  color=Gender, shape=Joystick),size=3)+ 
  geom_abline(intercept = 0, slope = 1)+
  labs(y="MT Happy to neutral",x="MT Neutral to happy")+
  facet_grid("Joystick")+
  coord_fixed()+
  xlim(0, 0.1)+
  ylim(0, 0.1)+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))

ggsave("figures/MT_emotion.tiff", units="in", width=6, height=4, dpi=200, compression = 'lzw')


# gender plot
MT_gender<-velocity%>%
  select(-count)%>%
  filter(procedure=="gender")%>%
  mutate( direction= case_when(direction == "to female" ~ "male to female",
                               direction == "to male" ~ "female to male"),
          joystick = case_when( joystick == "enter" ~ "Formation",
                                joystick == "exit" ~ "Disformation"))%>%
  spread(direction,vector)%>%
  data.frame()%>%
  'colnames<-'(c("subject", "Gender","procedure","Joystick","FtM","MtF"))

MT_gender%>%
  ggplot(aes(x=FtM,y=MtF) )+
  geom_point(aes(  color=Gender, shape=Joystick),size=3)+ 
  geom_abline(intercept = 0, slope = 1)+
  labs(x="MT Female to male",y="MT Male to female")+
  facet_grid("Joystick")+
  coord_fixed()+
  xlim(0, 0.1)+
  ylim(0, 0.1)+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1))

ggsave("figures/MT_gender.tiff", units="in", width=6, height=4, dpi=200, compression = 'lzw')


# Stats -------------------------------------------------------------

#Stat emotion
anova<-velocity%>%
  select(subject,group,procedure,direction,joystick,vector)%>%
  filter(procedure=="emotion")%>%
  group_by(subject,group,procedure,direction,joystick)%>%
  mutate( direction= case_when(direction == "to happy" ~ "neutral to happy",
                               direction == "to neutral" ~ "happy to neutral"),
          joystick = case_when( joystick == "enter" ~ "formation",
                                joystick == "exit" ~ "disformation"))%>%
  summarise_at(vars(vector), list(mean))%>%
  'colnames<-'(c("subject", "gender" , "procedure", "direction" ,"transition", "velocity"))

a1 <- aov_ez("subject", "velocity", anova,  within = c("direction","transition"), between = "gender")
emmeans(a1,pairwise~ transition, adjust="bonf")
emmeans(a1,pairwise~ direction|transition, adjust="bonf")
emmeans(a1,pairwise~ direction|gender|transition, adjust="bonf")

#Stat gender
anova<-velocity%>%
  select(subject,group,procedure,direction,joystick,vector)%>%
  filter(procedure=="gender")%>%
  group_by(subject,group,procedure,direction,joystick)%>%
  mutate( direction= case_when(direction == "to male"  ~ "female to male",
                               direction == "to female"  ~ "male to female"),
  joystick = case_when( joystick == "enter" ~ "formation",
                        joystick == "exit" ~ "disformation"))%>%
  summarise_at(vars(vector), list(mean))%>%
  'colnames<-'(c("subject", "gender" , "procedure", "direction" ,"transition", "velocity"))

a2 <- aov_ez("subject", "velocity", anova,  within = c("direction","transition"), between = "gender")
emmeans(a2,pairwise~ direction|gender|transition, adjust="bonf")

#################################################
# 
# END
#
#################################################