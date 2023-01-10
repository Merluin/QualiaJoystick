#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    Movement transition measure  - binocular rivalry
#
#################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(ggeffects)
library(afex)
library(emmeans)
library(cowplot)
library(magick) 
require(grid)

annotation_custom2 <- 
  function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){ 
    layer(data = data, stat = StatIdentity, position = PositionIdentity, 
          geom = ggplot2:::GeomCustomAnn,
          inherit.aes = TRUE, params = list(grob = grob,
                                            xmin = xmin, xmax = xmax,
                                            ymin = ymin, ymax = ymax))}
# Data --------------------------------------------------------------------

load("data/Qualia_joystick.RData") 

#image coding
HM <- image_fill(image_read("images/HM.png"), 'none')
HM <- as.raster(HM)
NM <- image_fill(image_read("images/NM.png"), 'none')
NM <- as.raster(NM)


# dataset  trials
correctedtl<-joystick_dataset%>%
  filter(subject == 3,procedure == "emotion", block>1)%>%
  select(block,row,ms,vector,x)%>%
  mutate(ms = parse_number(ms),
         x = x*-1,
         vector = vector*-1,
         trial = as.factor(row),
         block = "Block 2 (x * -1)",
         move = ifelse(vector == 0,NA,x))

tl<-joystick_dataset%>%
  filter(subject == 3,procedure == "emotion")%>%
  select(block,row,ms,vector,x)%>%
  mutate(ms = parse_number(ms),
         trial = as.factor(row),
         block = ifelse(block == 1, "Block 1","Block 2"),
         move = ifelse(vector == 0,NA,x))

data<-rbind(tl,correctedtl)%>%
  mutate(Movement = ifelse(vector >= 0,  "happy to neutral","neutral to happy"))%>%
  drop_na(Movement)


# plot trials time series
OD<-
  
data%>%
  filter(row == 12 & block == "Block 1")%>%
  data.frame()%>%
  group_by(block,row)%>%
  ggplot()+
  geom_line(aes(x=ms,y=x), show.legend = FALSE)+
  #facet_grid(block ~ .)+
  # annotation_custom2(rasterGrob(NM, interpolate=TRUE),ymax = 1,ymin = 0.5,xmin = 0, xmax = 150, data=data[1,])+
  # annotation_custom2(rasterGrob(HM, interpolate=TRUE),ymax = -1,ymin = -0.5,xmin = 0, xmax = 150, data=data[which(data$block=="Block 1")[1],])+
  # annotation_custom2(rasterGrob(NM, interpolate=TRUE),ymax = 1,ymin = 0.5,xmin = 0, xmax = 150, data=data[which(data$block=="Block 2 (x * -1)")[1],])+
  # annotation_custom2(rasterGrob(HM, interpolate=TRUE),ymax = -1,ymin = -0.5,xmin = 0, xmax = 150, data=data[which(data$block=="Block 2 (x * -1)")[1],])+
  # annotation_custom2(rasterGrob(HM, interpolate=TRUE),ymax = 1,ymin = 0.5,xmin = 0, xmax = 150, data=data[which(data$block=="Block 2")[1],])+
  # annotation_custom2(rasterGrob(NM, interpolate=TRUE),ymax = -1,ymin = -0.5,xmin = 0, xmax = 150, data=data[which(data$block=="Block 2")[1],])+
  theme_minimal() +
  xlab("Trial duration (ms)")+
  ylab("Joystick position")+
  ylim(-1, 1)+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold"),
        panel.background = element_rect(fill = "white", color = NA))

MD<-data%>%
  data.frame()%>%
  filter(block != "Block 2")%>%
  group_by(row)%>%
  ggplot()+
  geom_line(aes(x=ms,y=x, color = trial), show.legend = FALSE)+
  theme_minimal() +
  annotation_custom2(rasterGrob(NM, interpolate=TRUE),ymax = 1,ymin = 0.5,xmin = 0, xmax = 190, data=data)+
  annotation_custom2(rasterGrob(HM, interpolate=TRUE),ymax = -1,ymin = -0.5,xmin = 0, xmax = 190, data=data)+
  xlab("Trial duration (ms)")+
  ylab("Joystick position")+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        panel.background = element_rect(fill = "white", color = NA))


# plot trials movement
MJ<-data%>%
  filter(block != "Block 2")%>%
  mutate(move = (move+1)/2)%>%
  data.frame()%>%
  group_by(row)%>%
  ggplot()+
  geom_point(aes(x=ms,y=move, color = Movement), size = 0.5, show.legend = FALSE)+
  annotation_custom2(rasterGrob(NM, interpolate=TRUE),ymax = 1,ymin = 0.75,xmin = 0, xmax = 190, data=data)+
  annotation_custom2(rasterGrob(HM, interpolate=TRUE),ymax = 0,ymin = 0.25,xmin = 0, xmax = 190, data=data)+
  theme_minimal() +
  xlab("Trial duration (ms)")+
  ylab("Joystick position")+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold"),
        panel.background = element_rect(fill = "white", color = NA))


# dataset movements from 0 to 1
HtN<-data%>%
  filter(Movement == "happy to neutral")
  
NtH<-data%>%
  filter(Movement == "neutral to happy")%>%
  mutate(move = move*-1,
         Movement = "neutral to happy (x*-1)")

cordata<-rbind(HtN,NtH)%>%
  filter(block != "Block 2")%>%
  mutate(direction = ifelse(move <=0.5,"dissolution","formation"))

cordat<-cordata%>%
  mutate(move = (move+1)/2)%>%
  data.frame() 

CMJ<-cordat%>%
  group_by(row)%>%
  ggplot()+
  geom_hline(aes(yintercept=0.5), color = "darkgray", linetype = "dashed")+
  geom_point(aes(x=ms,y=move, color = Movement), size = 0.5, show.legend = FALSE)+
  annotate("text", x = 1550, y = 0.75, label = "formation", angle= 270,size = 3 )+
  annotate("text", x = 1550, y = 0.25, label = "disformation", angle= 270,size = 3)+
  facet_grid( Movement~. )+
  annotation_custom2(rasterGrob(NM, interpolate=TRUE),ymax = 1,ymin = 0.75,xmin = 0, xmax = 220, data=cordata[1,])+
  annotation_custom2(rasterGrob(HM, interpolate=TRUE),ymax = 1,ymin = 0.75,xmin = 0, xmax = 220, data=cordata[which(cordata$Movement=="neutral to happy (x*-1)")[1],])+
  annotation_custom2(rasterGrob(HM, interpolate=TRUE),ymax = 0,ymin = 0.25,xmin = 0, xmax = 220, data=cordata[1,])+
  annotation_custom2(rasterGrob(NM, interpolate=TRUE),ymax = 0,ymin = 0.25,xmin = 0, xmax = 220, data=cordata[which(cordata$Movement=="neutral to happy (x*-1)")[1],])+
  theme_minimal() +
  xlab("Trial duration (ms)")+
  ylab("Joystick position")+
  theme(axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 9, face = "bold"),
        strip.text.y = element_text(size = 9, face = "bold"),
        panel.background = element_rect(fill = "white", color = NA))


a<-plot_grid(OD,MD,nrow = 2,rel_heights = c(2,1), labels = c("A) Trials time series","B) Block 1 and aligned block 2"),scale=0.9,hjust = 0)

b<-plot_grid(MJ,CMJ,nrow = 2,rel_heights = c(1,2), labels = c("C) Joystick movements scaled ((x + 1)/2)","D) Joystick movements aligned: 0 dissolution, 1 formation"),scale=0.9,hjust = 0)

plot_grid(a,b)






