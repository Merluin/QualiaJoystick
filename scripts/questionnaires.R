#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    face emotion - binocular rivalry
#
#################################################

# Library & dataset
rm(list=ls()) # remove all objects
# neaded packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeffects)
library(afex)
library(hrbrthemes)
library(emmeans)
library(som)
library(PerformanceAnalytics)
source("05.functions/add_object_to_rda.R")

load("04.data_preprocessing/Qualia_joystick.RData") 
load("04.data_preprocessing/typical.RData") 

# Questionnaire ----
questionnaire<-questionnaires%>%
  select(id, age,fantasy,personal_distress,perspective_taking,empathic_concern,iri_tot,tas_tot)
#age
age<-questionnaire%>%
  select(id, group,age)%>%
  group_by(group)%>%
  summarise_at(vars(age), list(length,mean,sd))%>%
  'colnames<-'(c( "n","mean", "Sd"))

# IRI TAS summary
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

age
TAS
IRI

############### Data
# Questionnaire ----
Questionnaire<-questionnaire%>%
  select(fantasy,perspective_taking,empathic_concern,personal_distress,iri_tot,tas_tot)

# Valuation ----
val<-val_typical%>%
  select( subject,group,file,emotion, gender,Arousal,Valence) %>%
  gather(procedure,score,6:7)%>%
  select( subject,group,procedure,emotion, score) %>%
  group_by(subject,group,procedure,emotion) %>%
  summarise_at(vars(score), list(mean))%>%
  mutate(procedure=paste0(procedure,".",emotion))%>%
  select( subject,group, procedure, score) %>%
  spread(procedure, score)
val<-cbind(Questionnaire,val[,3:6])

# OR ----
ORT<-ORT_typical%>%
  group_by( subject,group,IP)%>%
  summarise_at(vars(ORT), list(mean))%>%
  spread(IP,ORT)

OR<-cbind(Questionnaire,ORT[,3:6])

# IP ----
IP<-ORT_typical%>%
  mutate(ORT = ifelse(ORT>=1, 1,0))%>%
  na.omit()%>%
  group_by(subject,group, IP) %>%
  summarise_at(vars(ORT), list(sum))%>%
  as.data.frame()%>%
  spread(IP,ORT)
IP<-cbind(Questionnaire,IP[,3:6])

# Cumulative Time ----

PM<-pm_tylical%>% 
  na.omit()%>%#cf function trial vec
  select(subject,group,procedure,pm)%>%
  spread(procedure,pm)

PM<-cbind(Questionnaire,PM[,3:4])

# MDD ----
MT<-MT_typical%>% #cf function trial vec
  select(subject,group,procedure,direction,joystick,velocity)%>%
  mutate( direction= case_when(direction == "happy to neutral" & joystick== "enter" ~ "enter to neutral",
                               direction == "happy to neutral" & joystick== "exit" ~ "exit from happy",
                               direction == "neutral to happy" & joystick== "enter" ~ "enter to happy",
                               direction == "neutral to happy" & joystick== "exit" ~ "exit from neutral",
                               direction == "female to male" & joystick== "enter" ~ "enter to male",
                               direction == "female to male" & joystick== "exit" ~ "exit from female",
                               direction == "male to female" & joystick== "enter" ~ "enter to female",
                               direction == "male to female" & joystick== "exit" ~ "exit from male"))%>%
  ungroup()%>%
  select(-procedure,-joystick)%>%
  spread(direction,velocity)

MT<-cbind(Questionnaire,MT[,3:10])


############### Plot cor ----
jpeg("07.figures/cor_val_exp4.jpg", units="in", width=10, height=8, res=200)
val<-val%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"AR.HPY", "AR.NEU","VAL.HPY", "VAL.NEU"))
chart.Correlation(val, histogram=FALSE, pch=19,method ="pearson")
dev.off()

jpeg("07.figures/cor_ort_exp4.jpg", units="in", width=10, height=8, res=200)
OR<-OR%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"ORT.FEM", "ORT.HPY", "ORT.MAL", "ORT.NEU"))
chart.Correlation(OR, histogram=FALSE, pch=19,method ="pearson")
dev.off()

jpeg("07.figures/cor_ip_exp4.jpg", units="in", width=10, height=8, res=200)
IP<-IP%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"IP.FEM", "IP.HPX", "IP.MAL", "IP.NEU"))
chart.Correlation(IP, histogram=FALSE, pch=19,method ="pearson")
dev.off()

jpeg("07.figures/cor_PM_exp4.jpg", units="in", width=10, height=8, res=200)
PM<-PM%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"PM.EMO", "PM.GEN"))
chart.Correlation(PM, histogram=FALSE, pch=19,method ="pearson")
dev.off()

jpeg("07.figures/cor_MT_exp4.jpg", units="in", width=10, height=8, res=200)
MT<-MT%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"MT.ENT.FEM", "MT.ENT.HTX", "MT.ENT.MAL", "MT.ENT.NEU","MT.EXT.FEM", "MT.EXT.HTX", "MT.EXT.MAL", "MT.EXT.NEU"))
chart.Correlation(MT, histogram=FALSE, pch=19,method ="pearson")
dev.off()

#################################################
# 
# END
#
################################################