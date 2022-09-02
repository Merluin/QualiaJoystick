#################################################
# 
# Experiment:     Qualia_Soma
# Programmer:     Thomas Quettier
# Date:           12/02/2021
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
