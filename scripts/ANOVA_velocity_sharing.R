#################################################
# 
# Experiment:     Qualia_Joystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           04/11/2020
# Description:    Velocity
#
#################################################

# Library & dataset
rm(list=ls()) # remove all objects
source("functions/calling.R")
calling()

# loading data ----
load("dataset/scoring_questionari.rda") 
load("dataset/table_complete.RData")
load("dataset/qualiajoy.RData") 

# get velocity from trials
velocity<-table%>% #cf function trial vector
  mutate(ysharing = c(plus[-1],NA),
         vecsharing = c(ysharing-plus))%>% 
  mutate(neg= ifelse(  vecsharing >0 ,vecsharing,NA ),
         pos= ifelse(  vecsharing <0,abs(vecsharing),NA ),
         sharing= case_when(  vecsharing >0 ~plus,
                              vecsharing <0 ~minus),
         count =1 )%>%
select(subject,group,attribut,procedure,ms,row,sharing,count,pr,neg,pos)%>%
  gather(stim,velocity,10:11)%>%
  na.omit()

# merging trials 
temp<-velocity%>%
  #filter(attribut != "happy" & attribut != "female" )%>%
  select(subject,group,procedure,stim,sharing,row,count,pr,velocity)%>%
  group_by(subject,group,procedure,stim,sharing)%>%
  summarise_at(vars(velocity,pr,count), list(mean,sum))%>%
  data.frame()%>%
  mutate(sharing = round(sharing,2))%>%
  select(subject,group,procedure,stim,sharing,velocity_fn1,pr_fn1, count_fn2)%>%
'colnames<-'(c("subject","group" , "procedure", "stim" , "sharing", "velocity","pr","count"))
  
# merging subject (sharing by %)
cent<-temp%>%
  group_by(group,procedure,stim,sharing)%>%
  summarise_at(vars(velocity,count,pr), list(mean))

# merging subject (sharing by 5%)
cinq<-temp%>%
  select(subject,group,procedure,stim,sharing,velocity)%>%
  mutate(sharing_cat= case_when(sharing >= 0 & sharing < .05 ~ "0-05",
                                sharing >=.05 & sharing < .1 ~ "05-10",
                                sharing >=.1 & sharing < .15 ~ "10-15",
                                sharing >=.15 & sharing < .2 ~ "15-20",
                                sharing >=.2 & sharing < .25 ~ "20-25",
                                sharing >=.25 & sharing < .3 ~ "25-30",
                                sharing >=.3 & sharing < .35 ~ "30-35",
                                sharing >=.35 & sharing < .4 ~ "35-40",
                                sharing >=.4 & sharing < .45 ~ "40-45",
                                sharing >=.45 & sharing < .5 ~ "45-50",
                                sharing >=.5 & sharing < .55 ~ "50-55",
                                sharing >=.55 & sharing < .6 ~ "55-60",
                                sharing >=.6 & sharing < .65 ~ "60-65",
                                sharing >=.65 & sharing < .7 ~ "65-70",
                                sharing >=.7 & sharing < .75 ~ "70-75",
                                sharing >=.75 & sharing < .8 ~ "75-80",
                                sharing >=.8 & sharing < .85 ~ "80-85",
                                sharing >=.85 & sharing < .9 ~ "85-90",
                                sharing >=.9 & sharing < .95 ~ "90-95",
                                sharing >=.95 & sharing <= 1 ~ "95-100"))%>%
  select(subject,group,procedure,stim,sharing_cat,velocity)%>%
  group_by(group,procedure,stim,sharing_cat)%>%
  summarise_at(vars(velocity), funs(mean))%>%
  'colnames<-'(c("group" , "procedure", "stim" , "sharing", "velocity"))%>%
  spread(group,velocity)

# merging subject (sharing by 10%)
dix<-temp%>%
  select(subject,group,procedure,stim,sharing,velocity)%>%
  mutate(sharing_cat= case_when(sharing >= 0 & sharing < .1 ~ "0-10",
                                sharing >=.1 & sharing < .2 ~ "10-20",
                                sharing >=.2 & sharing < .3 ~ "20-30",
                                sharing >=.3 & sharing < .4 ~ "30-40",
                                sharing >=.4 & sharing < .5 ~ "40-50",
                                sharing >=.5 & sharing < .6 ~ "50-60",
                                sharing >=.6 & sharing < .7 ~ "60-70",
                                sharing >=.7 & sharing < .8 ~ "70-80",
                                sharing >=.8 & sharing < .9 ~ "80-90",
                                sharing >=.9 & sharing <= 1 ~ "90-100"))%>%
  select(subject,group,procedure,stim,sharing_cat,velocity)%>%
  group_by(group,procedure,stim,sharing_cat)%>%
  summarise_at(vars(velocity), funs(mean))%>%
  'colnames<-'(c("group" , "procedure", "stim" , "sharing", "velocity"))%>%
  spread(group,velocity)

# plotting
plot<-cent
plot<-temp
plot<-velocity

# 3d 
plot%>%
  ggplot( aes(x=count, y=sharing, z=velocity,  color=group) )+
  theme_void() +
  axes_3D() +
  stat_3D()+
  facet_grid(procedure ~ stim)

plot%>%
  ggplot( aes(pr, fill=group) )+
  geom_histogram(  alpha=0.6, bins = 50,position = 'identity') +
  facet_grid(procedure ~ stim)+
  theme_classic()

table%>% #cf function trial vector
  filter(vector != 0 )%>%
  select(subject,group,procedure,vector)%>%
  mutate(vector= abs(round(vector,3)))%>%
  ggplot( aes(vector, fill=group) )+
  geom_histogram( color="#e9ecef", alpha=0.6, bins = 100, position = 'identity') +
  facet_grid(procedure ~ .)+
  theme_classic()

plot%>%
  ggplot( aes(sharing, fill=group) )+
  geom_histogram( color="#e9ecef",  bins = 100,alpha=0.6,position = 'identity') +
  facet_grid(procedure ~ stim)+
  theme_classic()


plot%>%
  ggplot(  )+
  geom_abline(intercept = 0.03, slope = 0, color="grey")+
  geom_point(  aes(x=sharing, y=velocity, color=group), size=1 ) +
  facet_grid(procedure ~ stim)+
  scale_x_continuous(name="Joystick position",n.breaks = 10) +
  scale_y_continuous(name="Joystick velocity (10ms)")+
  theme_classic()

  

a1 <- aov_ez("subject", "velocity", temp,  within = c("procedure","stim","sharing"), between = "group")

emmeans(a1,pairwise~ sharing, adjust="bonf")
emmeans(a1,pairwise~ procedure|sharing, adjust="bonf")
emmeans(a1,pairwise~ group|procedure|sharing, adjust="bonf")
emmeans(a1,pairwise~ stim|sharing, adjust="bonf")
emmeans(a1,pairwise~ group|stim|sharing_cat, adjust="bonf")
emmeans(a1,pairwise~ procedure|stim|sharing_cat, adjust="bonf")



#################################################
# 
# END
#
#################################################