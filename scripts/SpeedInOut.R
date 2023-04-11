#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    speed in and out   - binocular rivalry
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
data_emotion <- filtered_data%>%filter(procedure=="emotion")
data_gender <- filtered_data%>%filter(procedure=="gender")


# lmer emotion
e1<-lmer(mean ~ 1 + (1|subject), data_emotion)
e2<-lmer(mean ~ phase + (1|subject), data_emotion)
e3<-lmer(mean ~ phase + rivalry + (1|subject), data_emotion)
e4<-lmer(mean ~ rivalry*phase + (1|subject), data_emotion)
modelselection<- anova(e1,e2,e3,e4)
anova(e4)

# Effect size
F_to_d(63.2887, 1, 7021.0) 
F_to_d(144.7613, 1, 7017.0) 
F_to_eta2(4.6279, 1, 7017.9)

# Post_hoc emotion
emmeans(e4,pairwise~ rivalry, adjust="bonf")
z_to_d(-7.955,7059, paired = TRUE, ci = 0.95, alternative = "two.sided")

emmeans(e4,pairwise~ phase, adjust="bonf")
z_to_d(12.032,7059, paired = TRUE, ci = 0.95, alternative = "two.sided")

emmeans(e4,pairwise~ phase|rivalry, adjust="bonf")
z_to_d(8.060,7059, paired = TRUE, ci = 0.95, alternative = "two.sided")
z_to_d(8.969,7059, paired = TRUE, ci = 0.95, alternative = "two.sided")

emmeans(e4,pairwise~ rivalry|phase, adjust="bonf")
z_to_d(-4.267,7059, paired = TRUE, ci = 0.95, alternative = "two.sided")
z_to_d(-6.961,7059, paired = TRUE, ci = 0.95, alternative = "two.sided")
plot(allEffects(e4))

#plot emotion
data_emotion%>%
  filter(category != "HS",category != "NS")%>%
  mutate(category = case_when(category == "HD" ~ "c",
                              category == "HF" ~ "a",
                              category == "NF" ~ "b",
                              category == "ND" ~ "d"))%>%
  select(subject,row,category,rivalry,phase,epoch,mean)%>%
  group_by(subject,category,rivalry,phase)%>%
  summarise_at(vars(mean),list(mean))%>%
  ggplot() +
  geom_pirate( aes(x = category, y = mean, fill = rivalry))+
  geom_hline(yintercept = 0)+
  theme(text=element_text(size=14,  family="Arial"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_discrete(name="",labels=c("happy in", "neutral in", "happy out","neutral out"))+
  xlab("Speed labels")+
  ylab("Joystick Speed")+
  scale_fill_manual(values=c("#008b39","#fd345a"))
ggsave("figures/Speed_emotion.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

#plot main effect phase emotion
data_emotion$phase <- factor(data_emotion$phase, levels = c("formation","disolution"))
data_emotion$mean<-abs(data_emotion$median)

data_emotion%>%
  filter(category != "HS",category != "NS")%>%
  select(subject,category,row,rivalry,phase,epoch,mean)%>%
  group_by(subject,rivalry,phase)%>%
  summarise_at(vars(mean),list(mean))%>%
  ggplot() +
  geom_pirate( aes(x = phase, y = mean, fill= rivalry), bars = FALSE)+
  theme(text=element_text(size=14,  family="Arial"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_discrete(name="",labels=c("formation", "dissolution"))+
  xlab("Speed labels")+
  ylab("Joystick Speed")
 # scale_fill_manual(values=c("#008b39","#fd345a"))

ggsave("figures/main_phase.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

data_emotion%>%
  filter(category != "HS",category != "NS")%>%
  select(subject,category,row,rivalry,phase,epoch,mean)%>%
  group_by(subject,rivalry,phase)%>%
  summarise_at(vars(mean),list(mean))%>%
  ggplot() +
  geom_pirate( aes(x = rivalry, y = mean, fill= phase))+
  theme(text=element_text(size=14,  family="Arial"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_discrete(name="",labels=c("happy", "neutral"))+
  xlab("Speed labels")+
  ylab("Joystick Speed")
ggsave("figures/main_rivalry.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

#MAIN EFFECT
temp<-filtered_data%>%
  na.omit()%>%
  group_by(subject, phase, procedure)%>%
  summarise_at(vars(mean),list(mean))%>%
  select(subject,phase,procedure,mean)

dissolution<-temp%>%
  filter(phase=="disolution" , procedure == "emotion")
formation<-temp%>%
  filter(phase=="formation", procedure == "emotion")
res<-t.test(dissolution$mean,formation$mean, paired = TRUE,alternative = "two.sided", adjust="bonf")
t_to_d(t = res$statistic, res$parameter, paired = TRUE)

dissolution<-temp%>%
  filter(phase=="disolution" , procedure == "gender")
formation<-temp%>%
  filter(phase=="formation", procedure == "gender")
res<-t.test(dissolution$mean,formation$mean, paired = TRUE,alternative = "two.sided", adjust="bonf")
t_to_d(t = res$statistic, res$parameter, paired = TRUE)

#INTERACTION
temp<-data%>%
  na.omit()%>%
  group_by(subject, phase, rivalry)%>%
  summarise_at(vars(mean),list(mean))%>%
  select(subject,phase,rivalry,mean)

dissolution_happy<-temp%>%
  filter(phase=="disolution" , rivalry == "happy")

dissolution_neutral<-temp%>%
  filter(phase=="disolution" , rivalry == "neutral")%>%
  filter(subject != 24)

formation_happy<-temp%>%
  filter(phase=="formation" , rivalry == "happy")%>%
  filter(subject != 24)

formation_neutral<-temp%>%
  filter(phase=="formation" , rivalry == "neutral")

res<-t.test(dissolution_happy$mean,dissolution_neutral$mean, paired = TRUE,alternative = "two.sided")
t_to_d(t = res$statistic, res$parameter, paired = TRUE)


res<-t.test(formation_happy$mean,formation_neutral$mean, paired = TRUE,alternative = "two.sided")
t_to_d(t = res$statistic, res$parameter, paired = TRUE)


# lmer gender
g1<-lmer(mean ~ 1 + (1|subject), data_gender)
g2<-lmer(mean ~ phase + (1|subject), data_gender)
g3<-lmer(mean ~ phase + rivalry + (1|subject), data_gender)
g4<-lmer(mean ~ rivalry*phase + (1|subject), data_gender , contrasts = list(phase = contr.sum(2)/2, rivalry = contr.sum(2)/2))
anova(g1,g2,g3,g4)
anova(g2)

# Effect size
F_to_d(22.175, 1, 7094.3)

# Post_hoc gender
emmeans(g2,pairwise~ phase, adjust="bonf")
z_to_d(4.709,7134, paired = TRUE, ci = 0.95, alternative = "two.sided")
plot(allEffects(g2))

#Plot emotion
data_gender$phase <- factor(data_gender$phase, levels = c("formation","disolution"))
data_gender%>%
  filter(category != "FS",category != "MS")%>%
  mutate(category = case_when(category == "FD" ~ "d",
                              category == "FF" ~ "b",
                              category == "MF" ~ "a",
                              category == "MD" ~ "c"))%>%
  select(subject,row,category,rivalry,phase,epoch,mean)%>%
   group_by(subject,category,rivalry,phase)%>%
   summarise_at(vars(mean),list(mean))%>%
  ggplot() +
  geom_pirate( aes(x = phase, y = mean, fill = rivalry), bars = FALSE) +
  theme(text=element_text(size=14,  family="Arial"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_discrete(name="",labels=c("formation", "dissolution"))+
  xlab("Speed labels")+
  ylab("Joystick Speed")
  #scale_fill_manual(values=c("#becbec","#ffff99"))
ggsave("figures/Speed.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')


#################################################
# 
# END
#
#################################################