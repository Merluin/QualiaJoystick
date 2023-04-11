################################################
#
# Experiment: QualiaJoystick_binocular_rivalry
# Programmer: Thomas Quettier
# Date: 02/09/2022
# Description: median of predominance - binocular rivalry
#
#################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------
library(tidyverse)
library(ggpirate)
library(afex)
library(emmeans)

# Data --------------------------------------------------------------------
load("data/Qualia_joystick.RData")

pm <- joystick_dataset %>%
  na.omit() %>%
  select(subject, group, procedure, x) %>%
  group_by(subject, group, procedure) %>%
  summarise(pm = median(x)) %>%
  mutate(pm = round(pm, 3),
         attribut = if_else(procedure == "emotion", if_else(pm > 0, "neutral", "happy"),
                            if_else(pm > 0, "female", "male"))) %>%
  data.frame()

# Plots -------------------------------------------------------------

pm %>%
  ggplot(aes(x = procedure, y = pm, fill = procedure)) +
  geom_pirate(bars = FALSE) +
  geom_hline(yintercept = 0) +
  #facet_wrap(~ group, ncol = 1) +
  theme(text = element_text(size = 16, family = "Arial"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 14)) +
  scale_fill_manual(values = c("emotion" = "#FA8072", "gender" = "#4B0082"),
                    name = "",
                    labels = c("emotion rivalry", "gender rivalry")) +
  labs(x = "", y = "Predominance median")

ggsave("figures/PM.tiff", units = "in", width = 5, height = 4, dpi = 200, compression = 'lzw')

# Stats -------------------------------------------------------------
emotion <- pm %>%
  filter(procedure == "emotion") %>%
  select(pm) %>%
  t.test()

p.adjust(emotion$p.value, method = "bonferroni", n = 2) < 0.05

gender <- pm %>%
  filter(procedure == "gender") %>%
  select(pm) %>%
  t.test()

p.adjust(gender$p.value, method = "bonferroni", n = 2) < 0.05

a1 <- aov_ez("subject", "pm", pm, within = "procedure", between = "group")
emmeans(a1, pairwise ~ procedure, adjust = "bonf")

saveRDS(pm, file = file.path("data",  "data_median.rds"))

#################################################

# END

#################################################

