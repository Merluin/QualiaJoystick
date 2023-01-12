#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    data_speed.rds, data_ct.Rdata   - binocular rivalry
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
#require(httpgb)

annotation_custom2 <- function(grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){ 
    layer(data = data, stat = StatIdentity, position = PositionIdentity, 
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE, params = list(grob = grob,
                                            xmin = xmin, xmax = xmax,
                                            ymin = ymin, ymax = ymax))}
devtools::load_all()

# Data --------------------------------------------------------------------

load("data/Qualia_joystick.RData") 

lag = 50 # x*10 = ms

# dataset 
dataemotion<-joystick_dataset %>%
  filter(procedure == "emotion") %>%
  # filter(subject==1, row == 1 )%>%
  mutate(category = case_when(x > 0 & vector > 0 ~ "NF",
                              x > 0 & vector < 0 ~ "ND",
                              x < 0 & vector < 0 ~ "HF",
                              x < 0 & vector > 0 ~ "HD",
                              x == 1 & vector == 0 ~ "NS",
                              x == -1 & vector == 0 ~ "HS"))
datagender<-joystick_dataset%>%
  filter(procedure == "gender")%>%
  # filter(subject==1, row == 1 )%>%
  mutate(category = case_when(x > 0 & vector > 0 ~ "FF",
                              x > 0 & vector < 0 ~ "FD",
                              x < 0 & vector < 0 ~ "MF",
                              x < 0 & vector > 0 ~ "MD",
                              x >= 0.99 & vector == 0 ~ "FS",
                              x == -1 & vector == 0 ~ "MS"))

data<- rbind(dataemotion,datagender)

# extract epochs from data
datalist = list()
for(i in 1:40){ #nb subjects
  for(ii in 1:48){ #nb rows
    dat<-data%>%
      filter(subject==i, row == ii)
    idex<-paste0(i,"-",ii)
    datalist[[idex]] <- epochs(dat,lag)
  }
}
epoch_data <- dplyr::bind_rows(datalist)



# filter flat 
filtered_data<- epoch_data%>%
  filter(category != "MS" , category != "FS", category != "HS", category != "NS")%>%
  group_by(subject, procedure,group,row,category,epoch)%>%
  summarise_at(vars(x,vector),list(mean,min,max))%>%
  select(subject,procedure,group, row, category, epoch,vector_fn1, x_fn2, x_fn3)%>%
  mutate(flat = x_fn2 - x_fn3)%>%
  filter(flat <= -0.01)%>%
  group_by(subject,row,category)%>%
  dplyr::mutate(epoch = 1:n())%>%
  mutate(emotion = case_when(category == "HD" ~ "happy",
                             category == "HF" ~ "happy",
                             category == "ND" ~ "neutral",
                             category == "NF" ~ "neutral",
                             category == "MD" ~ "male",
                             category == "MF" ~ "male",
                             category == "FD" ~ "female",
                             category == "FF" ~ "female"),
         stage = case_when(category == "HD" ~ "disolution",
                           category == "HF" ~ "formation",
                           category == "ND" ~ "disolution",
                           category == "NF" ~ "formation",
                           category == "MD" ~ "disolution",
                           category == "MF" ~ "formation",
                           category == "FD" ~ "disolution",
                           category == "FF" ~ "formation"))%>%
  select(subject, procedure,group,row, category,emotion, stage,epoch, vector_fn1 )%>%
  'colnames<-'(c("subject", "procedure","group","row", "category","rivalry", "phase", "epoch", "mean"))%>%
  mutate(mean = abs(mean))

saveRDS(filtered_data, file = file.path("data",  "data_speed.rds"))
 
# Stability

filtered_ct<- epoch_data%>%
  filter(category == "MS" | category == "FS" | category == "HS" | category == "NS")%>%
  group_by(subject, procedure,group,row,category)%>%
  mutate(CT = abs(x)*10)%>%
  summarise_at(vars(CT),list(sum))%>%
  select(subject,procedure,group, row, category,CT)%>%
  group_by(subject,row,category)%>%
  dplyr::mutate(epoch = 1:n())%>%
  mutate(emotion = case_when(category == "MS" ~ "male.img",
                             category == "FS" ~ "female.img",
                             category == "HS" ~ "happy.img",
                             category == "NS" ~ "neutral.img"),
         stage = case_when(category == "MS" ~ "stable",
                           category == "FS" ~ "stable",
                           category == "HS" ~ "stable",
                           category == "NS" ~ "stable"))%>%
  select(subject, procedure,group,row, category,emotion, stage, CT )%>%
  ungroup()%>%
  'colnames<-'(c("subject", "procedure","group","row", "category","rivalry", "phase", "CT"))


filtered_STB<- epoch_data%>%
  filter(category == "MS" | category == "FS" | category == "HS" | category == "NS")%>%
  group_by(subject, procedure,group,row,epoch,category)%>%
  mutate(CT = abs(x)*10)%>%
  summarise_at(vars(CT),list(sum))%>%
  select(subject,procedure,group, row,epoch, category,CT)%>%
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
  select(subject, procedure,group,row,epoch, category,emotion, stage, CT )%>%
  ungroup()%>%
  'colnames<-'(c("subject", "procedure","group","row","epoch", "category","rivalry", "phase", "STB"))

save(filtered_ct,filtered_STB, file = file.path("data",  "data_ct.Rdata"))


#################################################
# 
# END
#
#################################################