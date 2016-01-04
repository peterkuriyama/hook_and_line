############################################################
#Peter Kuriyama 
#Start of 559 Project
#Fitting a GLMM to hook and line to
#estimate vessel, angler, location effects in a hierarchical model

############################################################
rm(list=ls())

setwd('/Users/peterkuriyama/School/Research/hook_and_line')

source('R/functions.R')

library(dplyr)
library(ggplot2)
library(glmmADMB)
library(mgcv)
library(sm)
library(lme4)
library(coefplot2)

############################################################
##Load Data
dat <- load_data()
save(dat, file = '/Users/peterkuriyama/Desktop/hl_04_12.Rdata')

# dat$year <- as.numeric(dat$year)

#Aggregate Data
gang <- group_gangion() #5 hooks, 1 angpos
drop <- group_drop() #three gangions
trip <- group_trip() #setid

############################################################
#Exploratory Plots

png(width = 7, height = 7, units = 'in', res = 200,
  file = 'figs/fishtime_effect.png')
plot(trip$fishtime, trip$fish.rate, pch = 19, 
  xlab = 'Fishing Time (seconds)', ylab = 'Catch Rate (Fish/75 hooks)',
  main = 'Trip Level')
dev.off()

plot_hook()
plot_sitename()


############################################################
#Recreate Harms GLM Analysis
{
  #------------------------------------------------------------
  # #ALL FISH
  # #Year and Site only
  # res1 <- glm(numfish ~ year + sitename , data = dat, family = binomial)
  # dat$fits <- fitted(res1)
  # test.glm <- dat %.% group_by(year, sitename) %.% summarise(numfish = sum(numfish), 
  #               nhooks = length(brtime), fits = unique(fits), 
  #               preds = fits * nhooks)
  # test.glm <- test.glm[-which(test.glm$numfish == max(test.glm$numfish)), ]

  # png(width = 7, height = 7, units = 'in', res = 200, 
  #   file = 'figs/glm_yr_site_fish.png')
  # ggplot(test.glm, aes(x = numfish, y = preds)) + geom_point() + facet_wrap( ~ year) + 
  #   theme_bw() + geom_abline(intercept = 0, slope = 1, colour = 'red') + 
  #   labs(x = 'Observed Fish', y = 'Predicted Fish')
  # dev.off()

  # png(width = 12, height = 9, units = 'in', res = 200,
  #   file = 'figs/glm_site_fish.png')
  # ggplot(test.glm, aes(x = numfish, y = preds)) + geom_point() + facet_wrap( ~ sitename) + 
  #   theme_bw() + geom_abline(intercept = 0, slope = 1, colour = 'red') + 
  #   labs(x = 'Observed Fish', y = 'Predicted Fish')
  # dev.off()

  #------------------------------------------------------------
  #Bocaccio
  #Year and Site only
  #Fixed effects

  res1 <- glm(numboc ~ year + sitename , data = dat, family = binomial)
  dat$fits <- fitted(res1)
  test.glm <- dat %.% group_by(year, sitename) %.% summarise(numboc = sum(numboc), 
                nhooks = length(brtime), fits = unique(fits), 
                preds = fits * nhooks)
  test.glm <- test.glm[-which(test.glm$numboc == max(test.glm$numboc)), ]
  AIC(res1)

  coef(res1)[coef(res1) < -5]

  png(width = 6.8, height = 10.42, res = 200, units = 'in',
    file = 'figs/glm_coefs.png')
  coefplot2(res1)
  dev.off()

  png(width = 7, height = 7, units = 'in', res = 200, 
    file = 'figs/glm_yr_site_boc.png')
  ggplot(test.glm, aes(x = numboc, y = preds)) + geom_point() + facet_wrap( ~ year) + 
    theme_bw() + geom_abline(intercept = 0, slope = 1, colour = 'red') + 
    labs(x = 'Observed Bocaccio', y = 'Predicted Bocaccio')
  dev.off()

  png(width = 12, height = 9, units = 'in', res = 200,
    file = 'figs/glm_site_boc.png')
  ggplot(test.glm, aes(x = numboc, y = preds)) + geom_point() + facet_wrap( ~ sitename) + 
    theme_bw() + geom_abline(intercept = 0, slope = 1, colour = 'red') + 
    labs(x = 'Observed Bocaccio', y = 'Predicted Bocaccio')
  dev.off()

  #------------------------------------------------------------
  #Bocaccio
  

  #Full model from Harms 2010, ran once and now load everything 
  #GANGION LEVEL
  res.full.gang <- glm(boc.rate ~ year + sitename + poly(fishtime, 2) + vesname + 
                  angnum + dropnum + poly(waveht.m, 2) + 
                  dropnum * angnum, data = gang, family = binomial,
                  weights = numhooks)
  gang$fits <- res.full.gang$fitted
  gang$preds <- gang$fits * 5

  summ.gang <- gang %.% group_by(year, sitename) %.% summarise(numboc = sum(numboc), preds = sum(preds))
  # summ.gang <- summ.gang[-which(summ.gang$preds == max(summ.gang$preds)), ]
  summ.gang <- as.data.frame(summ.gang)

  ggplot(summ.gang, aes(x = numboc, y = preds)) + geom_point() + facet_wrap(~ year) + 
    theme_bw() + geom_abline(intercept = 0, slope = 1, colour = 'red') + 
    labs(x = 'Observed Bocaccio', y = 'Predicted Bocaccio')

 #tripION LEVEL
  res.full.trip <- glm(boc.rate ~ year + sitename + poly(fishtime, 2) + vesname + 
                 dropnum + poly(waveht.m, 2),
                  data = trip, family = binomial,
                  weights = numhooks)
  trip$fits <- res.full.trip$fitted
  trip$preds <- trip$fits * 5

  summ.trip <- trip %.% group_by(year, sitename) %.% summarise(numboc = sum(numboc), preds = sum(preds))
  # summ.trip <- summ.trip[-which(summ.trip$preds == max(summ.trip$preds)), ]
  summ.trip <- as.data.frame(summ.trip)

  ggplot(summ.trip, aes(x = numboc, y = preds)) + geom_point() + facet_wrap(~ year) + 
    theme_bw() + geom_abline(intercept = 0, slope = 1, colour = 'red') + 
    labs(x = 'Observed Bocaccio', y = 'Predicted Bocaccio')


  # save(res.full, file = 'output/glm_full_model.Rdata')
  


  # res.full <- glm(numboc ~ year + sitename + poly(fishtime, 2) + vesname + 
  #                 hooknum + angnum + dropnum + poly(waveht.m, 2) + 
  #                 dropnum * angnum, data = dat, family = binomial)

  # save(res.full, file = 'output/glm_full_model.Rdata')
  load('output/glm_full_model.Rdata')
  AIC(res.full)
  coefplot2(res.full)
  res.full$fitted
  dat$fits <- res.full$fitted


  summ <- dat %.% group_by(year, sitename) %.% summarise(numboc = sum(numboc), preds = sum(fits))
  summ <- summ[-which(summ$preds == max(summ$preds)), ]
  
  png(width = 7, height = 7, units = 'in', res = 200, 
    file = 'figs/glm_full_preds.png')
  ggplot(summ, aes(x = numboc, y = preds)) + geom_point() + facet_wrap(~ year) + theme_bw() +
    labs(x = 'Observed Bocaccio', y = 'Predicted Bocaccio') + geom_abline(intercept = 0, slope = 1,
      colour = 'red')
  dev.off()

  

  #Not sure how to plot this
  png(file = 'figs/full_glm_boxplot.png')
  plot(as.factor(dat$numfish), dat$fits, xlab = 'Observed', ylab = 'Predicted', cex.lab = 1.4)
  dev.off()

  full.glm <- dat %.% group_by(year, sitename) %.% summarise(numboc = sum(numboc), 
    nhooks = length(brtime), fits = unique(fits), preds = fits * nhooks)



  gang.res.full <- glm(boc.rate ~ year + sitename + poly(fishtime, 2) + vesname + 
                       angnum + dropnum + poly(waveht.m, 2) + 
                       dropnum * angnum, data = gang, family = binomial, 
                       weights = numhooks)
  gang$fits <- fitted(gang.res.full)
  gang$preds <- gang$fits * gang$numhooks

  png(width = 7, height = 7, units = 'in', res = 200, 
    file = 'figs/full_glm_yr_boc.png')
  ggplot(gang, aes(x = numboc, y = preds)) + geom_point() + facet_wrap(~ year) + 
    theme_bw() + geom_abline(intercept = 0, slope = 1, col = 'red') + 
    labs(x = 'Observed Bocaccio', y = 'Predicted Bocaccio')
  dev.off()

  png(width = 12, height = 9, units = 'in', res = 200,
    file = 'figs/full_glm_site_boc.png')
  ggplot(gang, aes(x = numboc, y = preds)) + geom_point() + facet_wrap(~ sitename) + 
    theme_bw() + geom_abline(intercept = 0, slope = 1, col = 'red') + 
    labs(x = 'Observed Bocaccio', y = 'Predicted Bocaccio')
  dev.off()

  #------------------------------------------------------------
  #Try at a gangion level
  ptest <- glm(boc.rate ~ year + sitename, data = gang, family = binomial, weights = numhooks)
  gang$fits <- fitted(ptest)
  gang.glm <- gang %.% group_by(year, sitename) %.% summarise(numboc = sum(numboc),
    nhooks = length(brtime), fits = unique(fits), preds = fits * nhooks)

  gang.glm <- gang.glm[-which(gang.glm$numboc == max(gang.glm$numboc)), ]

  png(width = 7, height = 7, units = 'in', res = 200, 
    file = 'figs/gang_glm_yr_boc.png')
  ggplot(gang.glm, aes(x = numboc, y = preds)) + geom_point ()+ facet_wrap( ~ year) +
    theme_bw() + geom_abline(intercept = 0, slope = 1, colour = 'red') + 
    labs(x = 'Observed Bocaccio', y = 'Predicted Bocaccio')
  dev.off()

  png(width = 12, height = 9, units = 'in', res = 200,
    file = 'figs/gang_glm_site_boc.png')
  ggplot(gang.glm, aes(x = numboc, y = preds)) + geom_point ()+ facet_wrap( ~ sitename) +
    theme_bw() + geom_abline(intercept = 0, slope = 1, colour = 'red') + 
    labs(x = 'Observed Bocaccio', y = 'Predicted Bocaccio')
  dev.off()
  #------------------------------------------------------------
  #Try at a drop level -- Pretty crappy
  # ptest <- glm(boc.rate ~ year + sitename, data = drop, family = binomial, weights = numhooks)
  # drop$fits <- fitted(ptest)
  # drop.glm <- drop %.% group_by(year, sitename) %.% summarise(numboc = sum(numboc),
  #   nhooks = length(brtime), fits = unique(fits), preds = fits * nhooks)

  # drop.glm <- drop.glm[-which(drop.glm$numboc == max(drop.glm$numboc)), ]

  # ggplot(drop.glm, aes(x = numboc, y = preds)) + geom_point ()+ facet_wrap( ~ sitename) +
  #   theme_bw() + geom_abline(intercept = 0, slope = 1, colour = 'red') + 
  #   labs(x = 'Observed Bocaccio', y = 'Predicted Bocaccio')
}

############################################################
#Try GAMs at different scales
#Picked Deviance Explained maximum
gam.trip <- gam_scenarios(data = trip, filename = 'gam_trip')
gam.drop <- gam_scenarios(data = drop, filename = 'gam_drop')
gam.gang <- gam_scenarios(data = gang, filename = 'gam_gang')

plot_gam_preds(data = trip, filename = 'gam_trip')
plot_gam_preds(data = drop, filename = 'gam_drop')
plot_gam_preds(data = gang, filename = 'gam_gang')

############################################################
#GLMMs with Glmmadmb package
dat$angnum <- as.factor(dat$angnum)
test <- glmmadmb(numboc ~ sitename , data = dat,
  family = 'binomial')
glmm.site <- test 
save(glmm.site, file = 'output/admbglmm_site.Rdata')
dat$fits <- fitted(glmm.site)

############################################################
#Do it with lme because glmmadmb isn't developed enough
run1 <- glmer(boc.rate ~ sitename + 1|vesname, 
  data = trip, family = 'binomial',
  weights = numhooks)




run1 <- glmer(boc.rate ~ sitename + 1|angnum, 
  data = gang, family = 'binomial',
  weights = numhooks)











