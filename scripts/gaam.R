#################################################
# 
# Experiment:     Qualia_Joystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           30/04/2020
# Description:    GAAM
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
library(mgcv)
# loading data ----
#load("dataset/Qualia_joystick.RData") 
load("dataset/Qualia_moebius.RData") 
data<-joystick_dataset%>%
  #filter(row<2)%>%
  select(subject, procedure,row, ms,x)%>%
  mutate(ms= parse_number(ms),
         procedure = ifelse(procedure=="emotion",1,2))



g<-gam( x ~ 1 + procedure + row + s(ms, by=procedure, k=2) + s(ms, by=subject,bs="re"),data=data)

gam.check(g)
plot(g, residuals =T, rug=T, se=T, pch=20)
summary(g)
