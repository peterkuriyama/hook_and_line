#Load Packages
library(grid)
library(plyr)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(ggmap)

#Data stored locally
# load('data/Grand 2012 27 Apr 2013.dmp')
load('data/Grand.2014.JF.dmp')
dat <- Grand.2014.JF

#Source Functions
source('R/functions.R')
