#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    STB with its relative speed   - binocular rivalry
#
#################################################

rm(list = ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggeffects)
library(afex)
library(emmeans)
library(lme4)
library(effects)
library(cowplot)
library(magick) 
require(grid)
library(ggpirate)
library(effectsize)

annotation_custom2 <- function(grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){ 
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob,
                                          xmin = xmin, xmax = xmax,
                                          ymin = ymin, ymax = ymax))}
devtools::load_all()

# Data --------------------------------------------------------------------
load("data/data_ct.Rdata")
filtered_data<-readRDS("data/data_speed.rds")



# STB with its relative speed in happy

happy<-filtered_data%>%
  filter(rivalry == "happy", phase == "formation")
temp<-filtered_STB%>%
  filter(rivalry == "happy")%>%
  select(subject,row,epoch,STB)

happy<- left_join(temp,happy,by="subject")%>%
  mutate(filter = ifelse(row.x==row.y & epoch.x==epoch.y,1,0))%>%
  filter(filter == 1)%>%
  select(subject,row.x,epoch.x,STB,mean)%>%
  'colnames<-'(c("subject","row","epoch","STB","Speed_in_happy"))

fit<-lmer(STB ~ Speed_in_happy + (1|subject), happy)
anova(fit)

plot(allEffects(fit))

# STB with its relative speed out happy

happy<-filtered_data%>%
  filter(rivalry == "happy", phase == "disolution")
temp<-filtered_STB%>%
  filter(rivalry == "happy")%>%
  select(subject,row,epoch,STB)

happy<- left_join(temp,happy,by="subject")%>%
  mutate(filter = ifelse(row.x==row.y & epoch.x==epoch.y,1,0))%>%
  filter(filter == 1)%>%
  select(subject,row.x,epoch.x,STB,mean)%>%
'colnames<-'(c("subject","row","epoch","STB","Speed_out_happy"))

fit<-lmer(Speed_out_happy ~ STB + (1|subject), happy)
anova(fit)


plot(allEffects(fit))

# STB with its relative speed in neutral
neutral<-filtered_data%>%
  filter(rivalry == "neutral", phase == "formation")
temp<-filtered_STB%>%
  filter(rivalry == "neutral")%>%
  select(subject,row,epoch,STB)

neutral<- left_join(temp,neutral,by="subject")%>%
  mutate(filter = ifelse(row.x==row.y & epoch.x==epoch.y,1,0))%>%
  filter(filter == 1)%>%
  select(subject,row.x,epoch.x,STB,mean)%>%
  'colnames<-'(c("subject","row","epoch","STB","Speed_in_neutral"))

fit<-lmer(STB ~ Speed_in_neutral + (1|subject), neutral)
anova(fit)

plot(allEffects(fit))

# STB with its relative speed out neutral

neutral<-filtered_data%>%
  filter(rivalry == "neutral", phase == "disolution")
temp<-filtered_STB%>%
  filter(rivalry == "neutral")%>%
  select(subject,row,epoch,STB)

neutral<- left_join(temp,neutral,by="subject")%>%
  mutate(filter = ifelse(row.x==row.y & epoch.x==epoch.y,1,0))%>%
  filter(filter == 1)%>%
  select(subject,row.x,epoch.x,STB,mean)%>%
  'colnames<-'(c("subject","row","epoch","STB","Speed_out_neutral"))

fit<-lmer(Speed_out_neutral ~ STB + (1|subject), neutral)
anova(fit)

plot(allEffects(fit))


