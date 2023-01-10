#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    Predominance mean - binocular rivalry
#
#################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(ggpirate)
library(afex)


# Data --------------------------------------------------------------------

load("data/Qualia_joystick.RData") 

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

# Plots -------------------------------------------------------------

# MT
pm%>%
  'colnames<-'(c("subject","group","Condition","PM","attribut"))%>%
  ggplot( aes(x = Condition, y = PM)) +
  geom_pirate(aes(fill = Condition))+
  geom_hline(yintercept = 0)+
  theme(text=element_text(size=16,  family="Arial"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_discrete(name="",labels=c("emotion rivalry", "gender rivalry"))
ggsave("figures/PM.tiff", units="in", width=5, height=4, dpi=200, compression = 'lzw')

# Stats -------------------------------------------------------------

emotion<-pm%>%
  filter(procedure=="emotion" )%>%
  select(pm)%>%
  t.test()
p.adjust(emotion$p.value,method ="bonferroni",2) < 0.05

gender<-pm%>%
  filter(procedure=="gender")%>%
  select(pm)%>%
  t.test()
p.adjust(gender$p.value,method ="bonferroni",2) < 0.05

a1 <- aov_ez("subject", "pm", pm,  within = "procedure", between = "group")
emmeans(a1,pairwise~ procedure, adjust="bonf")

#################################################
# 
# END
#
#################################################