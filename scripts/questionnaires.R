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

library(here)

# Data --------------------------------------------------------------------

load("data/Qualia_joystick.RData") 
stab <- readRDS(here("data","stability.rds"))

questionnaire<-questionnaires%>%
  select(id, age,group,fantasy,personal_distress,perspective_taking,empathic_concern,iri_tot,tas_tot)

Questionnaire<-questionnaire%>%
  select(fantasy,perspective_taking,empathic_concern,personal_distress,iri_tot,tas_tot)


# Valuation
val<-VAVplot<-valuation_dataset%>%
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
  group_by(subject,group,emotion,procedure)%>%
  summarise_at(vars(score), list(mean))%>%
  mutate(Face=  emotion,
         procedure = ifelse(procedure =="Arousal", "AR","VL" ),
         Face = paste0(procedure,".",Face))%>%
  ungroup()%>%
  select( subject,Face, score) %>%
  spread(Face, score)

# group_by(subject,group,file,procedure)%>%
#   summarise_at(vars(score), list(mean))%>%
#   mutate(Face=  case_when(file=="HF"  ~ "happy.female",
#                           file=="NF"  ~ "neutral.female",
#                           file=="HM"  ~ "happy.male",
#                           file=="NM"  ~ "neutral.male"),
#          procedure = ifelse(procedure =="Arousal", "AR","VL" ),
#          Face = paste0(procedure,".",Face))%>%
#   ungroup()%>%
#   select( subject,Face, score) %>%
#   spread(Face, score)

v<-val[,-1]
# %>%
#   'colnames<-'(c("AR.happy.female","AR.happy.male","AR.neutral.female","AR.neutral.male" ,"VL.happy.female","VL.happy.male","VL.neutral.female","VL.neutral.male"  ))
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
p<-PM[,-c(1,2)]%>%
  'colnames<-'(c("MP.emotion","MP.gender"))
  
PM<-cbind(Questionnaire,PM[,3:4])%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"PM.EMO", "PM.GEN"))


# MT
MT<- readRDS(here("data","data.rds"))
MT<-MT%>%
  group_by(subject,category)%>%
  summarise_at(vars(mean),list(mean))%>%
  'colnames<-'(c("subject","category","speed"))%>%
  spread(category,speed)
m<-MT[,-1]%>%
  'colnames<-'(c("DS.Female", "FR.Female", "DS.Happy", "FR.Happy", "DS.Male", "FR.Male", "DS.Neutral", "FR.Neutral"))
  

MT<-cbind(Questionnaire,MT[,-1])%>%
  'colnames<-'(c("IRI.F", "IRI.PT", "IRI.EC", "IRI.PD" ,"IRI.TOT","TAS.TOT" ,"MT.ENT.FEM", "MT.ENT.HTX", "MT.ENT.MAL", "MT.ENT.NEU","MT.EXT.FEM", "MT.EXT.HTX", "MT.EXT.MAL", "MT.EXT.NEU"))

#CT
c<-stab%>%
  group_by(subject, category)%>%
  summarise_at(vars(CT),list(mean),na.rm = TRUE)%>%
  mutate(category = case_when(category == "NS" ~ "CT.neutral",
                              category == "HS" ~ "CT.happy",
                              category == "FS" ~ "CT.female",
                              category == "MS" ~ "CT.male"))%>%
  ungroup()%>%
  spread(category, CT)%>%
  select( -subject)



#  --------------------------------------------------------------------
# emo<-cbind(v,p,m)%>%
#   select(AR.happy.female,AR.happy.male,AR.neutral.female,AR.neutral.male ,VL.happy.female,VL.happy.male,VL.neutral.female,VL.neutral.male, 
#      MP.emotion, DS.Happy, FR.Happy, DS.Neutral, FR.Neutral)
# gen<-cbind(v,p,m)%>%
#   select(AR.happy.female,AR.happy.male,AR.neutral.female,AR.neutral.male ,VL.happy.female,VL.happy.male,VL.neutral.female,VL.neutral.male, 
#          MP.gender, DS.Female, FR.Female,  DS.Male, FR.Male)

emo<-cbind(v,p,m,c)%>%
  select(AR.happy,AR.neutral ,VL.happy,VL.neutral, 
         DS.Happy, FR.Happy, DS.Neutral, FR.Neutral,CT.happy,CT.neutral)
gen<-cbind(v,p,m,c)%>%
  select(AR.happy,AR.neutral ,VL.happy,VL.neutral,  
         DS.Female, FR.Female,  DS.Male, FR.Male,CT.female,CT.male)
saveRDS(emo, file = file.path("data",  "emotion_correlation.rds"))
saveRDS(gen, file = file.path("data",  "gender_correlation.rds"))

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


jpeg("figures/cor_emotion.jpg", units="in", width=10, height=8, res=200)
chart.Correlation(emo, histogram=FALSE, pch=19,method ="pearson")
dev.off()

jpeg("figures/cor_gender.jpg", units="in", width=10, height=8, res=200)
chart.Correlation(gen, histogram=FALSE, pch=19,method ="pearson")
dev.off()
################################################
# 
# END
#
################################################