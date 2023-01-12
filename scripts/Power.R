#################################################
# 
# Experiment:     QualiaJoystick_binocular_rivalry
# Programmer:     Thomas Quettier
# Date:           02/09/2022
# Description:    power analysis
#
#################################################

# Library & dataset
rm(list=ls()) # remove all objects

# neaded packages
library(pwr)

d <- .5
sig.level <- 0.05
power <- 0.8
  
pwr.t.test(  d=d  , sig.level=sig.level , power =power, type =  "paired")

#################################################
# 
# END
#
#################################################