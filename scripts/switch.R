#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    chuncks switches   - binocular rivalry
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



#plot figure paper
#image coding
HM <- image_fill(image_read("images/HM.png"), 'none')
HM <- as.raster(HM)
NM <- image_fill(image_read("images/NM.png"), 'none')
NM <- as.raster(NM)


plot<-datagender%>%
  filter(subject == 2, row == 2)%>%
  mutate(Epochs = case_when(category == "MS"~ "stabilisation",
                            category == "FS"~ "stabilisation",
                            category == "MD" ~ "dissolution",
                            category == "MF" ~ "formation",
                            category == "FD" ~ "dissolution",
                            category == "FF" ~ "formation"),
         ms = parse_number(ms)*10)

plot%>%
  epochs(lag = 20)%>%
  group_by(category,epoch)%>%
  summarise_at(vars(x,vector),list(mean,min,max))%>%
  select( category, epoch, vector_fn1, x_fn2, x_fn3)%>%
  mutate(flat = x_fn2 - x_fn3)%>%
  filter(flat <= -0.01)

meanspeed <- c(0.0233, 0.037, 0.0236, 0.083, 0.0331)
stabilisation - c(211,260,241)



plot%>%
  ggplot(aes(y = x, x = ms, colour = Epochs)) +
  geom_line(aes(group=1), size = 1) + 
  scale_colour_manual(values = c("#fd345a", "#008b39", "#0000FF")) +
  theme_minimal() +
  annotation_custom2(rasterGrob(NM, interpolate=TRUE),ymax = 1,ymin = 0.6,xmin = 0, xmax = 160, data=plot) +
  annotation_custom2(rasterGrob(HM, interpolate=TRUE),ymax = -1,ymin = -0.6,xmin = 0, xmax = 160, data=plot) +
  xlab("Trial duration (ms)") +
  ylab("Joystick position") +
  ylim(-1, 1) +
  theme(axis.title.x = element_text(size = 14, family="Arial"),#, face = "bold"),
        axis.title.y = element_text(size = 14, family="Arial"),#, face = "bold"),
        axis.ticks = element_blank(),
        strip.text.x = element_text(size = 11, family="Arial"),#, face = "bold"),
        strip.text.y = element_text(size = 11, family="Arial"),#, face = "bold"),
        panel.background = element_rect(fill = "white", color = NA))+
  theme(legend.position = "bottom")+
  geom_text(aes(x = 2000, y = -0.45, label ="Speed in = 0.0233"),angle = 290,size = 3, color = "#008b39") +
  geom_text(aes(x = 3100, y = -0.93, label ="STB = 2110 ms"),size = 3, color = "#0000FF")+
  geom_text(aes(x = 4000, y = -0.5, label ="Speed out = 0.037"),angle = 86,size = 3, color = "#fd345a") +
  geom_text(aes(x = 5000, y = 0.5, label ="Speed in = 0.0236"),angle = 70,size = 3, color = "#008b39") +
  geom_text(aes(x = 7000, y = 0.93, label ="STB = 2600 ms"),size = 3, color = "#0000FF") +
  geom_text(aes(x = 8500, y = 0.5, label ="Speed out = 0.083"),angle = 283,size = 3, color = "#fd345a") +
  geom_text(aes(x = 12000, y = -0.5, label ="Speed in= 0.0331"),angle = 275,size = 3, color = "#008b39") +
  geom_text(aes(x = 13600, y = -0.93, label ="STB = 2410 ms"),size = 3, color = "#0000FF")

ggsave("figures/trial.tiff", units="px", width=911, height=380, dpi=200, compression = 'lzw')

#################################################
# 
# END
#
#################################################