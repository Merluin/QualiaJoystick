library(fGarch)
library(tidyverse)

set.seed(35486)                    # Create random distributions
data <- data.frame(x1 = rbeta(1000, 5, 2),
                   x2 = rbeta(1000, 2, 5))
plot(density(data$x2), col = 2,    # Overlay all columns as densities
     xlim = c(0, 2),
     ylim = c(0, 3))
lines(density(data$x1), col = 3)


#data<-readRDS("data/data_aru.rds")
saveRDS(data, file = file.path("data",  "data_aru.rds"))

rect <- data.frame(xmxf = c(.83,.19),
                   xmnf = c(.85,.21),
                   api = c(.84,.2),
                   content = c("content A","content B"),
                   For = c(.4,.1),
                   Dis = c(1,.8))
                   

data%>%
  gather(content,value,c(x1:x2))%>%
  mutate(content = ifelse(content == "x1","content A","content B"))%>%
  ggplot()+
  geom_rect(data = rect ,aes(xmin = 0,
                xmax = xmxf,
                ymin = - Inf,
                ymax = Inf),
            fill = "#f7dad8",
            alpha = 0.2)+
  geom_rect(data = rect ,aes(xmin = xmnf,
                xmax = 1.25,
                ymin = - Inf,
                ymax = Inf),
            fill = "#ccebed",
            alpha = 0.2)+
  # geom_rect(aes(xmin = .25,
  #               xmax = .79,
  #               ymin = - Inf,
  #               ymax = .20),
  #           fill = "#f7dad8",
  #           alpha = 1)+
  # geom_rect(aes(xmin = .81,
  #               xmax = 1.25,
  #               ymin = - Inf,
  #               ymax = .20),
  #           fill = "#ccebed",
  #           alpha = 1)+
  geom_text(data = rect ,aes(x = For, y = 2.8, label ="Formation"))+
  geom_text(data = rect ,aes(x = Dis, y = 2.8, label ="Dissolution"))+
  geom_text(aes(x = 1.2, y = 1.6, label ="0.4 y"), color = "gray")+
  geom_segment(aes(x=0,xend= 1.15,y=1.6,yend = 1.6),color = "gray")+
  geom_text(aes(x = 1.2, y = 1.6, label ="0.4 y"), color = "gray")+
  geom_segment(aes(x=0,xend= 1.15,y=2.4,yend = 2.4),color = "gray")+
  geom_text(aes(x = 1.2, y = 2.4, label ="0.7 y"), color = "gray")+
  geom_density(aes(value, color = content))+
  geom_vline(data = rect,aes(xintercept = api), linetype="dotted", 
             color = "red", size=1.5)+
  # geom_text(aes(x = .52, y = 1, label ="Content A"),angle = 45,size = 3, color = "#fd345a") +
  # geom_text(aes(x = .61, y = 1, label ="Content B"),angle = 60,size = 3, color = "#008b39") +
  
  xlab("Time")+
  ylab("Proportion of qualia content \nagainst maximum possible")+
  theme(text=element_text(size=16,  family="Arial"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", arrow = grid::arrow(length = unit(0.3, "cm"))),
        axis.ticks =element_blank(),
        axis.text  =element_blank(),
        strip.text = element_text(size = 11, family="Arial"),
        legend.position = "none")+
  scale_colour_manual(values = c("#fd345a", "#008b39"))+
  facet_grid(content ~ .)
  
  
 



