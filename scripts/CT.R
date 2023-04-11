#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    cumulative time   - binocular rivalry
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
filtered_data<-readRDS("data/data_speed.rds")
load("data/data_ct.Rdata")
data_emotion <- filtered_data%>%filter(procedure=="emotion")
data_gender <- filtered_data%>%filter(procedure=="gender")

# Stability

filtered_ct<- filtered_ct%>%
  group_by(subject, procedure,group,row,category)%>%
  summarise_at(vars(CT),list(sum))%>%
  select(subject,procedure,group, row, category,CT)%>%
  group_by(subject,row,category)%>%
  dplyr::mutate(epoch = 1:n())%>%
  mutate(emotion = case_when(category == "MS" ~ "male",
                             category == "FS" ~ "female",
                             category == "HS" ~ "happy",
                             category == "NS" ~ "neutral"),
         stage = case_when(category == "MS" ~ "stable",
                           category == "FS" ~ "stable",
                           category == "HS" ~ "stable",
                           category == "NS" ~ "stable"))%>%
  select(subject, procedure,group,row, category,emotion, stage, CT )%>%
  ungroup()%>%
  'colnames<-'(c("subject", "procedure","group","row", "category","rivalry", "phase", "CT"))


#plot
filtered_ct%>%
  mutate(category = case_when(category == "FS" ~ "c",
                              category == "HS" ~ "a",
                              category == "NS" ~ "b",
                              category == "MS" ~ "d"))%>%
  filter(category == "a"| category == "b" )%>%
  select(subject,category,rivalry,phase,CT)%>%
  group_by(subject,category,rivalry,phase)%>%
  summarise_at(vars(CT),list(mean))%>%
  ggplot() +
  geom_pirate( aes(x = category, y = CT, fill = category), bars = FALSE)+
  geom_hline(yintercept = 0)+
  theme(text=element_text(size=16,  family="Arial"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_discrete(name="",labels=c("happy", "neutral", "female", "male"), 
                   guide = guide_axis(angle = 0))+
  ylab("Cumulative time (ms)")
  #scale_fill_manual(values=c("#008b39","#fd345a"))

filtered_ct%>%
  mutate(category = case_when(category == "FS" ~ "d",
                              category == "HS" ~ "a",
                              category == "NS" ~ "b",
                              category == "MS" ~ "c"))%>%
  filter(category == "c"| category == "d" )%>%
  select(subject,category,rivalry,phase,CT)%>%
  group_by(subject,category,rivalry,phase)%>%
  summarise_at(vars(CT),list(mean))%>%
  ggplot() +
  geom_pirate( aes(x = category, y = CT, fill = category), bars = FALSE)+
  geom_hline(yintercept = 0)+
  theme(text=element_text(size=16,  family="Arial"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_discrete(name="",labels=c("female", "male"), 
                   guide = guide_axis(angle = 0))+
  ylab("Cumulative time (ms)")
  #scale_fill_manual(values=c("#becbec","#ffff99"))
 
# emotion
data <- filtered_ct%>%filter(procedure=="emotion")
g1<-lmer(CT ~ 1 + (1|subject), data)
g2<-lmer(CT ~ rivalry + (1|subject), data)
anova(g1,g2)

anova(g2)
F_to_d(407.07, 1, 635.54)

# POST_HOC
#MAIN EFFECT
temp<-data%>%
  na.omit()%>%
  group_by(subject,rivalry)%>%
  summarise_at(vars(CT),list(mean))%>%
  select(subject,rivalry,CT)

happy<-temp%>%
  filter(rivalry == "happy")%>%
  filter(subject != 3,subject != 24, subject != 30,subject != 39)
neutral<-temp%>%
  filter(rivalry == "neutral")

res<-t.test(happy$CT,neutral$CT, paired = TRUE,alternative = "two.sided")
t_to_d(t = res$statistic, res$parameter, paired = TRUE)




emmeans(g2,pairwise~ rivalry, adjust="bonf")
plot(allEffects(g2))


# gender
data <- filtered_ct%>%filter(procedure=="gender")
g1<-lmer(CT ~ 1 + (1|subject), data)
g2<-lmer(CT ~ rivalry + (1|subject), data)
anova(g1,g2)

anova(g2)
plot(allEffects(g2))







