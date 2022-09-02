#################################################
# 
# Experiment:     Qualia_Joystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           04/11/2020
# Description:    Correlatioon
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



cor<-cent%>%
  select(group ,procedure, stim ,sharing,velocity,pr)%>%
  mutate(condition = case_when(group=="female" & procedure=="emotion"  & stim=="neg" ~ "fe.em.ne",
                               group=="female" & procedure=="emotion"  & stim=="pos" ~ "fe.em.po",
                               group=="female" & procedure=="gender"  & stim=="neg" ~ "fe.ge.ne",
                               group=="female" & procedure=="gender"  & stim=="pos" ~ "fe.ge.po",
                               group=="male" & procedure=="emotion"  & stim=="neg" ~ "ma.em.ne",
                               group=="male" & procedure=="emotion"  & stim=="pos" ~ "ma.em.po",
                               group=="male" & procedure=="gender"  & stim=="neg" ~ "ma.ge.ne",
                               group=="male" & procedure=="gender"  & stim=="pos" ~ "ma.ge.po"))




velo<-cor%>%
  ungroup()%>%
  select(sharing,condition ,velocity)%>%
  spread(condition,velocity)
chart.Correlation(velo)

pr<-cor%>%
  ungroup()%>%
  select(sharing,condition ,pr)%>%
  spread(condition,pr)
chart.Correlation(pr)

cor(velo,pr)




data.frame()%>%
  filter(sharing < .1 & group == "female" & procedure == "emotion"  & stim == "pos")%>%
  select(pr,velocity)
cor(FE$pr,FE$velocity)
ME<-temp%>%
  data.frame()%>%
  filter(sharing < .1 & group == "male" & procedure == "emotion"  & stim == "pos")%>%
  select(pr,velocity)
cor(ME$pr,ME$velocity)
ME$gender<-"female"
FE$gender<-"male"
rbind(ME,FE)%>%
  ggplot(aes(y=velocity,x=pr) )+
  geom_point(aes(  color=gender),size=1)+ 
  geom_abline(intercept = 45)+
  labs(y="Velocity",x="Pr")+
  theme(axis.text.x = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        axis.text.y = element_text(size = 24),
        axis.title.y = element_text(size = 24)) +
  theme_classic()


FE<-temp%>%
  data.frame()%>%
  filter(sharing < .05 & group == "female" & procedure == "gender"  & stim == "pos")%>%
  select(pr,velocity)
cor(FE$pr,FE$velocity)
FE<-temp%>%
  data.frame()%>%
  filter(sharing < .05 & group == "female" & procedure == "emotion"  & stim == "neg")%>%
  select(pr,velocity)
cor(FE$pr,FE$velocity)
FE<-temp%>%
  data.frame()%>%
  filter(sharing < .05 & group == "female" & procedure == "gender"  & stim == "neg")%>%
  select(pr,velocity)
cor(FE$pr,FE$velocity)

FE<-temp%>%
  data.frame()%>%
  filter(sharing < .05 & group == "male" & procedure == "gender"  & stim == "pos")%>%
  select(pr,velocity)
cor(FE$pr,FE$velocity)
FE<-temp%>%
  data.frame()%>%
  filter(sharing < .05 & group == "male" & procedure == "emotion"  & stim == "neg")%>%
  select(pr,velocity)
cor(FE$pr,FE$velocity)
FE<-temp%>%
  data.frame()%>%
  filter(sharing < .05 & group == "male" & procedure == "gender"  & stim == "neg")%>%
  select(pr,velocity)
cor(FE$pr,FE$velocity)




#################################################
# 
# END
#
#################################################