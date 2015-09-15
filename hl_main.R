##
# Hook and Line Analysis

setwd('/Users/peterkuriyama/School/Research/hook_and_line/')

#Load Packages
library(plyr)
library(dplyr)
library(ggplot2)

#Data stored locally
# load('data/Grand 2012 27 Apr 2013.dmp')
load('data/Grand.2014.JF.dmp')
dat <- Grand.2014.JF

#plot catch rates in each location
gang <- dat %>% group_by(SetID, DropNum, AngNum) %>% mutate(Nfish = sum(SurvFish), FishRate = Nfish / 5,
          Nbocac = sum(NumBoc), BocacRate = Nbocac / 5, Comms = paste(ComName, collapse = ';')) %>% 
          as.data.frame




paste(c('po', 'op'), collapse = ';')

dat[dat$SetID == "14-01-06-054", ]





