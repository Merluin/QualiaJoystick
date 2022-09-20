#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    TAS and IRI - binocular rivalry
#
#################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(PerformanceAnalytics)

# Data --------------------------------------------------------------------

load("data/Qualia_joystick.RData") 

questionnaire<-questionnaires%>%
  select(id, age,group,fantasy,personal_distress,perspective_taking,empathic_concern,iri_tot,tas_tot)

Questionnaire<-questionnaire%>%
  select(fantasy,perspective_taking,empathic_concern,personal_distress,iri_tot,tas_tot)


# Valuation
val<-valuation_dataset%>%
  select( subject,group,procedure,emotion, score) %>%
  group_by(subject,group,procedure,emotion) %>%
  summarise_at(vars(score), list(mean))%>%
  mutate(procedure=paste0(procedure,".",emotion))%>%
  select( subject,group, procedure, score) %>%
  spread(procedure, score)
val<-cbind(Questionnaire,val[,3:6])%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"AR.HPY", "AR.NEU","VAL.HPY", "VAL.NEU"))


# PM
PM<-joystick_dataset%>% 
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
                              pm<0 & procedure == "gender" ~ "male"))%>% 
  na.omit()%>%#cf function trial vec
  select(subject,group,procedure,pm)%>%
  spread(procedure,pm)
PM<-cbind(Questionnaire,PM[,3:4])%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"PM.EMO", "PM.GEN"))


# MT
MT<-joystick_dataset%>% #cf function trial vec
  select(subject,group,attribut,procedure,vector,x)%>%
  filter(vector != 0 )%>%
  mutate(direction = ifelse(  vector > 0 ,"plus","minus" ))%>%
  select(subject,group,procedure,direction,x,vector)%>%
  mutate(x = (x+1)/2,
         x= ifelse(direction == "minus",1-x,x),
         x= ifelse(x >= .5,"enter","exit"),
         vector= abs(vector),
         vector = round(vector,3),
         direction= case_when(direction=="minus" & procedure == "emotion"  ~ "to happy",
                              direction=="minus" & procedure == "gender"  ~ "to male",
                              direction=="plus" & procedure == "emotion"  ~ "to neutral",
                              direction=="plus" & procedure == "gender"  ~ "to female" ))%>%
  'colnames<-'(c("subject","group" , "procedure", "direction" , "joystick", "vector"))%>%
  group_by(subject,group,procedure,direction,joystick)%>%
  summarise_at(vars(vector), list(mean))%>%
  select(subject,group ,procedure,direction,joystick,vector)%>%
  'colnames<-'(c("subject","group" , "procedure", "direction" , "joystick", "speed"))%>%
  mutate( direction= case_when(direction == "to neutral" & joystick== "enter" ~ "enter to neutral",
                               direction == "to neutral" & joystick== "exit" ~ "exit from happy",
                               direction == "to happy" & joystick== "enter" ~ "enter to happy",
                               direction == "to happy" & joystick== "exit" ~ "exit from neutral",
                               direction == "to male" & joystick== "enter" ~ "enter to male",
                               direction == "to male" & joystick== "exit" ~ "exit from female",
                               direction == "to female" & joystick== "enter" ~ "enter to female",
                               direction == "to female" & joystick== "exit" ~ "exit from male"))%>%
  ungroup()%>%
  select(-procedure,-joystick)%>%
  spread(direction,speed)
MT<-cbind(Questionnaire,MT[,3:10])%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"MT.ENT.FEM", "MT.ENT.HTX", "MT.ENT.MAL", "MT.ENT.NEU","MT.EXT.FEM", "MT.EXT.HTX", "MT.EXT.MAL", "MT.EXT.NEU"))

# Summary --------------------------------------------------------------------

age<-questionnaire%>%
  select(id, group,age)%>%
  group_by(group)%>%
  summarise_at(vars(age), list(length,mean,sd))%>%
  'colnames<-'(c( "n","mean", "Sd"))

IRI<-questionnaire%>%
  select(id, group,iri_tot)%>%
  na.omit()%>%
  group_by(group)%>%
  summarise_at(vars(iri_tot), list(mean,sd))%>%
  'colnames<-'(c( "mean", "Sd"))

TAS<-questionnaire%>%
  select(id,group,tas_tot)%>%
  na.omit()%>%
  group_by(group)%>%
  summarise_at(vars( tas_tot), list(mean,sd))%>%
  'colnames<-'(c( "mean", "Sd"))

# Plots --------------------------------------------------------------------

jpeg("figures/cor_val.jpg", units="in", width=10, height=8, res=200)
chart.Correlation(val, histogram=FALSE, pch=19,method ="pearson")
dev.off()

jpeg("figures/cor_PM.jpg", units="in", width=10, height=8, res=200)
chart.Correlation(PM, histogram=FALSE, pch=19,method ="pearson")
dev.off()

jpeg("figures/cor_MT.jpg", units="in", width=10, height=8, res=200)
chart.Correlation(MT, histogram=FALSE, pch=19,method ="pearson")
dev.off()

################################################
# 
# END
#
################################################