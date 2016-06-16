##
# Hook and Line Analysis

#Startup
setwd('/Users/peterkuriyama/School/Research/hook_and_line/')

source('R/hl_startup.R')

#----------------------------------------------------------------------------------------------------
#Mixed Effects Model
library(lme4)
library(TMB)





#######Drop Level#######
  dat %>% group_by(Year, SiteName, VesName, DropNum, AngNum) %>% 
  summarize(lastname = unique(LastName),
    onbsec = mean(OnBSec, na.rm = TRUE),
    brsec = mean(BRSec, na.rm = TRUE),
    fishtime = mean(FishTime, na.rm = TRUE),
    fbsec = mean(FBSec, na.rm = TRUE),
    droptime = mean(DropTime, na.rm = TRUE),
    lat = mean(Lat.DD),
    lon = mean(Lon.DD),
    depth = mean(Depth.m, na.rm = TRUE),
    avg_weight = mean(Weight.kg, na.rm = TRUE),
    avg_length = mean(Length.cm, na.rm = TRUE), 
    wave_height = mean(WaveHt.m, na.rm = TRUE),
    num_boc = sum(NumBoc), 
    num_fish = sum(SurvFish),
    num_hook = length(HookNum), 
    cpue_boc = num_boc / num_hook, 
    cpue_fish = num_fish / num_hook) %>% as.data.frame -> 
drop_cpue

drop_cpue$Year <- as.factor(drop_cpue$Year)


logit <- function(xx){
  out <- xx / (1 - xx)
  return(out)
}

dbinom(6, size = 5, prob = .8, log = FALSE)



logit(aa$data$preds[1])
logit(coef(aa)[1])

#Start simple just estimate fixed effect with year and binomial
aa <- glm(cpue_boc ~ Year, data = drop_cpue, family = binomial, weights = num_hook)
aa$data$preds <- fitted(aa)
coef(aa)


#How to calculate predicted values?
preds <- predict(aa)

sum(fitted == (exp(preds)) / (1 + exp(preds)))
fits <- fitted(aa)



head(fitted(aa))





head(drop_cpue)
unique(drop_cpue$Year)
unique(drop_cpue$SiteName)
unique(drop_cpue$lastname)

#Specify design matrix










#Check to see why there are NaNs
dat %>% filter(Year == 2004, SiteName == 352, VesName == "Mirage", LastName == "Chesley", 
  DropNum == 5)
   %>% summarise(ll = mean(Length.cm, na.rm = TRUE) )


drop_cpue[which(drop_cpue$num_boc > drop_cpue$num_fish), ]
plot(drop_cpue$num_fish, drop_cpue$num_boc)


#Do this model in TMB...
drop_cpue$SiteName <- as.factor(drop_cpue$SiteName)
drop_cpue$Year <- as.factor(drop_cpue$Year)
drop_cpue$lastname <- as.factor(drop_cpue$lastname)

res <- glm(cpue_boc ~ Year + SiteName + lastname,
  data = drop_cpue, family = binomial, weights = num_hook)



head(res$data)

outs <- cbind(res$data, 'preds' = fitted(res))
outs$resid <- outs$preds - outs$cpue_boc

exp(resid(res))

(head(resid(res)))
res_drop <- glm(cpue_boc ~ Year + SiteName + lastname + DropNum, data = drop_cpue,
  family = binomial, weights = num_hook)

predict(res)

X <- cbind("Intercept" = rep(1, 1000))

#----------------------------------------------------------------------------------------------------
#Do it in TMB

library(TMB)
compile('hl_glm.cpp')
dyn.load(dynlib('hl_glm'))

X <- cbind("year" = drop_cpue$Year,
  "site" = drop_cpue$SiteName,
  "lastname" = drop_cpue$lastname)


Data <- list("y_i" = drop_cpue$num_boc, "X_ij" = X)
Params <- list("b_j" = c(0, 0, 0), "theta_z" = c(0, 0))

Obj <- MakeADFun(data = Data, parameters = Params,
  DLL = 'hl_glm')
Opt <- nlminb(start = Obj$par, objective = Obj$fn,
  gradient = Obj$gr)


n_year <- length(unique(drop_cpue$Year))
n_site <- length(unique(drop_cpue$SiteName))
n_lastname <- length(unique(drop_cpue$lastname))



dbinom(4, size = 5, prob = .5  )

library


#----------------------------------------------------------------------------------------------------
#Bocaccio age distributions


#----------------------------------------------------------------------------------------------------
#See if there's a relationship between number of fish caught and the average weight



dat %>% filter(Date == '2004-11-10', VesName == 'Aggressor', DropNum == '1')
#Per Drop
dat %>% group_by(Date, VesName, DropNum,
  SiteName) %>% summarize(avg_weight = mean(Weight.kg, na.rm = TRUE),
  avg_length = mean(Length.cm, na.rm = TRUE), num_boc = sum(NumBoc), num_fish = sum(SurvFish),
  num_hook = length(HookNum), cpue_boc = num_boc / num_hook, cpue_fish = num_fish / num_hook) %>% 
  as.data.frame -> check

plot(check$cpue_fish, check$cpue_boc, pch = 19)
plot(check$cpue_fish, check$avg_weight)
plot(check$cpue_boc, check$avg_weight)
plot(check$cpue_boc, check$avg_length)
plot(check$avg_length, check$avg_weight)

ggplot(check)

#Check only one site





#----------------------------------------------------------------------------------------------------
#Replicate GLM on the Drop level

#Specify the final columns to include?
#Specify columns to average across?


#Bocaccio
drops <- agg_glm_dat(grp_by = c('Year', 'SiteName', 'VesName', 'DropNum'), 
  target_species = 'Bocaccio')

boxplot(cpue ~ SiteName, data = drops)
boxplot(cpue ~ Year + SiteName, data = drops)

boxplot(as.factor(drops$cpue), drops$SiteName)

ggplot(data = drops, aes(x = cpue, y = Year)) + geom_point()
ggplot(data = drops, aes(x = cpue, y = SiteName)) + geom_point()
ggplot(data = drops, aes(x = cpue, y = SSTDrop.C)) + geom_point()
ggplot(data = drops, aes(x = cpue, y = MoonPct)) + geom_point()
ggplot(data = drops, aes(x = cpue, y = PctLite)) + geom_point()
ggplot(data = drops, aes(x = cpue, y = TideFlow2.ftPerHr)) + geom_point()
ggplot(data = drops, aes(x = cpue, y = SwellHt.m)) + geom_point()

hist(log(drops$cpue + 1))
hist(drops$cpue, breaks = 30)

drops$totfish <- as.factor(drops$totfish)

#Drop Level
l_y <- glm(cpue ~ Year, weights = nhooks,  data = drops, 
  family = binomial)
l_ys <- glm(cpue ~ Year + SiteName, weights = nhooks,  data = drops, 
  family = binomial)
l_yst <- glm(cpue ~ Year + SiteName + SSTDrop.C, weights = nhooks,  data = drops, 
  family = binomial)

l_yst <- glm(cpue ~ Year + SiteName + SSTDrop.C + DropNum, weights = nhooks,  data = drops, 
  family = binomial)
summary(l_yst)

plot(resid(l_yst))
plot(fitted(l_yst))
plot(fitted)
plot(l_yst$data$cpue, fitted(l_yst))


plot(resid(l_yst), fitted(l_yst))


AIC(l_y)
AIC(l_ys)
AIC(l_yst)

sapply(X = c("l_y", 'l_ys'), FUN = AIC)


cloglog_res <- glm(totfish ~ Year + SiteName + SSTDrop.C, data = drops, 
  family = binomial(link = cloglog))




plot(drops$cpue, predict(res_drop))


predict(res_drop)


sites <- agg_glm_dat(grp_by = c('Year', 'SiteName', 'VesName'))


#----------------------------------------------------------------------------------------------------
#Explore covariates that affect range of certain fish
yell <- format_glm_dat(target_species = 'Yellowtail')

dotchart(yell$NumYel, group = yell$SiteName)

yell_res1 <- glm(NumYel ~ Year + SiteName, family = binomial, data = yell)
yell_res2 <- glm(NumYel ~ Year + SiteName + SSTDrop.C, family = binomial, data = yell)  
yell_res3 <- glm(NumYel ~ Year + SiteName + SSTDrop.C + 
  CrewStaff , family = binomial, data = yell)  

library(AED); data(Loyn)
dotchart(yell$Year, yell$SiteName)

  Year + SiteName + SSTDrop.C, family = binomial, data = yell)




#Most caught species are
dat %>% group_by(ComName) %>% summarise(nfish = n()) %>% arrange(desc(nfish))
#greenspotted and yellowtail

yel <- subset(dat, ComName == 'Yellowtail Rockfish')

yel <- glm()

#Raw number of yellowtail rockfish per year
yel %>% group_by(Year) %>% summarise(nfish = n()) %>% ggplot(aes(x = Year, y = nfish)) + 
  ylim(c(0, 150)) + geom_point() + geom_line() + theme_bw()

#Plot density of yellowtail rockfish per year
yel %>% group_by(Year, SiteName) %>% summarise(avg_lat = mean(Lat.DD), 
  avg_lon = mean(Lon.DD), nfish = sum(SurvFish), mean_temp = mean(SSTDrop.C),
  min_temp = min(SSTDrop.C), max_temp = max(SSTDrop.C)) -> yel_site
yel_site$std_nfish <- (yel_site$nfish - mean(yel_site$nfish)) / sd(yel_site$nfish)

ggplot(dat = yel_site, aes(x = -avg_lon, y = avg_lat, colour = std_nfish)) + 
geom_point(aes(size = mean_temp)) + scale_colour_gradient(low = 'white', 
  high = 'red') + facet_wrap(~ Year)


res <- glm(nfish ~ mean_temp + Year, dat = yel_site, family = binomial)
plot(resid(res))
plot(yel_site$mean_temp, yel_site$nfish)

yel %>% group_by(SiteName) %>% summarise(nlats = length(unique(Lat.DD)), 
  nlon = length(unique(Lon.DD)))



  as.data.frame %>% group_by(ComName) %>% summarise(avg.nfish = round(mean(nfish), digits = 0)) %>%
  arrange(desc(avg.nfish)) %>% as.data.frame()


dat %>% group_by(ComName, Year) %>% summarise(nfish = length(Length.cm)) %>% 
  as.data.frame %>% group_by(ComName) %>% summarise(avg.nfish = round(mean(nfish), digits = 0)) %>%
  arrange(desc(avg.nfish)) %>% as.data.frame()



#----------------------------------------------------------------------------------------------------
#deltaGLMM to integrate vessel and angler effects for bocaccio cpue

#Explore catch rates of vermilion aggregated at different levels

#Drop
dat %>% group_by(Year, SiteName, VesName, DropNum) %>% summarise(nhooks = length(HookNum),
  nboc = sum(NumBoc), cpue_15 = nboc / nhooks) -> cpue_drop 

#cpue is definitely zero inflated at the drop level
hist(cpue_drop$cpue_15, breaks = 100) 

#--------------------------------
#Site
dat %>% group_by(Year, SiteName, VesName) %>% mutate(nhooks = length(HookNum), 
  nboc = sum(NumBoc), boc_cpue = nboc / nhooks, fish_cpue = sum(SurvFish) / nhooks) %>% 
  filter(NumBoc == 1) %>% mutate(avg_boc_kg = mean(Weight.kg)) %>% as.data.frame %>%
  distinct(Year, SiteName, VesName, boc_cpue) -> cpue_site

#Still zero inflated at the site level 
#little more resolution because of higher number of hooks
hist(cpue_hook$cpue_75, breaks = 100)

ggplot(cpue_hook, aes(x = Year, y = cpue_75)) + geom_point(aes(color = VesName)) + 
  facet_wrap(~ SiteName) 

#Year
dat %>% group_by(Year) %>% summarise(nhooks = length(HookNum), nboc = sum(NumBoc),
  cpue_year = nboc / nhooks) -> cpue_year 
plot(cpue_year$cpue_year, type = 'b')

#--------------------------------
#Angler
dat %>% group_by(Year, SiteName, VesName, DropNum, LastName) %>% mutate(nhooks = length(HookNum),
  nboc = sum(NumBoc), cpue_angler = nboc / nhooks) %>% as.data.frame -> cpue_angler
cpue_angler %>% group_by(Year, SiteName, VesName, DropNum, LastName) %>% filter(NumBoc == 1) %>% 
  mutate(avg_boc_kg = mean(Weight.kg)) %>% distinct(CrewStaff) %>% as.data.frame -> cpue_angler

#Check 
dat %>% filter(DropNum == 1 & AngNum == 3 & Year == 2004 & CrewStaff == 'IanNicholson') %>% head

#----------------------------------------------------------------------------------------------------
#Glm investigation 

#What Calculate Average bocaccio weight
#Pairs plot

png(width = 7, height = 7, units = 'in', file = 'figs/site_pairs.png',
  res = 200)
pairs(cpue_site[, c('avg_boc_kg', 'fish_cpue',
  'FishTime', 'boc_cpue')])
dev.off()


  cpue_angler$avg_boc_kg, cpue_angler$cpue_angler, cpue_angler$FishTime)

test <- head(cpue_angler, n = 10)
test %>% group_by(LastName) %>% distinct(CrewStaff) %>% as.data.frame

summarize(n = n())

#Investigate glm in tmb
length(cpue_site$cpue_75)











#Most caught species are vermilion, bocaccio, greenspotted rockfish

#What are the things of interest
# cpue
# Most caught species / species distributions
# species weight distributions
# species length distributions

#Can be stratified by 
# Region
# Vessel 
# Site
# Drop 
# Hook Location

#----------------------------------------------------------------------------------------------------
#What years caught the most bocaccio?
  #2004 was high, low was in 2010
dat %>% group_by(Year) %>% summarise(numboc = sum(NumBoc), nhooks = length(SetID), 
  cpue = numboc / nhooks) %>% ggplot(aes(x = Year, y = cpue)) + geom_point() + theme_bw()


#----------------------------------------------------------------------------------------------------
#Which hooks catch the most fish?
dat$catch <- 'yes'
dat[which(dat$Sex == ''), 'catch'] <- 'no'
dat$HookNum <- as.factor(dat$HookNum)

#Across all the data
dat %>% group_by(HookNum) %>% summarise(nfish = length(which(catch == 'yes')), nhooks = length(catch),
  cpue = nfish / nhooks) 

#so generally it looks like hooks closer to the bottom catch more fish (no surprise)
#What if we stratify by year?
dat %>% group_by(Year, HookNum) %>% summarise(nfish = length(which(catch == 'yes')), nhooks = length(catch),
          cpue = nfish / nhooks) %>% 
ggplot(aes(x = HookNum, y = cpue)) + geom_point() + facet_wrap(~ Year)

#What if we stratify by region?
dat %>% group_by(GenLoc, HookNum) %>% summarise(nfish = length(which(catch == 'yes')), nhooks = length(catch),
          cpue = nfish / nhooks) %>% 
ggplot(aes(x = HookNum, y = cpue)) + geom_point() + facet_wrap(~ GenLoc)


#----------------------------------------------------------------------------------------------------
#Which hooks catch the most bocaccio?
dat %>% group_by(GenLoc, HookNum) %>% summarise(numboc = sum(NumBoc), nhooks = length(catch),
  cpue = numboc / nhooks) %>% as.data.frame %>% 
ggplot(aes(x = HookNum, y = cpue)) + geom_point() + facet_wrap(~ GenLoc)

#Does this relationship change over year? Probably not..  
dat %>% group_by(Year, HookNum) %>% summarise(numboc = sum(NumBoc), nhooks = length(catch),
  cpue = numboc / nhooks) %>% as.data.frame %>% 
ggplot(aes(x = HookNum, y = cpue)) + geom_point() + facet_wrap(~ Year)


#----------------------------------------------------------------------------------------------------



#########Look at fish/hook at different scales
#Site
dat <- dat %>% group_by(SiteName) %>% mutate(SiteLat = mean(Lat.DD), SiteLon = mean(Lon.DD)) %>%
  as.data.frame
dat <- dat %>% group_by(Year, SiteName) %>% mutate(cpue = sum(SurvFish) / length(SurvFish),
           Nfish = sum(SurvFish), Nspecies = length(unique(ComName))) %>% as.data.frame


#----------------------------------------------------------------------------------------------------
#Do hooks at the bottom catch more fish than hooks at the top?
dat %>% group_by(HookNum)
dat %>% select(HookNum)

#----------------------------------------------------------------------------------------------------
ggplot(dat, aes(x = Weight.kg)) + geom_bar() + facet_wrap(~ VesName + Year)

dat %>% group_by(Year, VesName) 


#----------------------------------------------------------------------------------------------------
#Plot for 2011-2014 bypass proposal
recent <- subset(dat, Year >= 2011)
coast <- borders('world', colour = 'black', height = c(32, 35), width = c(-122, -115))
lon.range <- range(dat$SiteLon)
lon.range <- c(lon.range[1] - .2, lon.range[2] + .3)

lat.range <- range(dat$SiteLat)
lat.range <- c(lat.range[1] - .2, lat.range[2] + .2)

#faceted plot without california map
ggplot(recent, aes(x = -SiteLon, y = SiteLat, colour = cpue)) + geom_point() + facet_wrap(~ Year) 
  
#Try to add california map for bypass proposal
states_map <- map_data("state")
cal <- subset(states_map, region == 'california')

xx <- ggplot(cal, aes(x = long, y = lat)) + geom_polygon() + coord_map(xlim = c(-121, -117), 
  ylim = c(31.5, 35)) + geom_point(data = recent, 
  aes(x = -SiteLon, y = SiteLat, colour = cpue)) + facet_wrap(~ Year) + 
  scale_colour_gradient(low = 'white', high = 'red') + theme_bw() + 
  theme(panel.margin = unit(1, 'lines'), axis.text.x = element_text(angle = 90))
  
pdf(width = 7, height = 7, file = 'figs/recent_cpue.pdf')


png(width = 7, height = 7, file = 'figs/recent_cpue.png', 
  units = 'in', res = 200)
print(xx)
dev.off()


ggplot(recent, aes(x = cpue)) + geom_histogram(binwidth = .05) + facet_wrap(~ GenLoc)













ggplot(recent, aes(x = -SiteLon, y = SiteLat, colour = cpue)) + geom_point() 



socal <- subset(cal, long < -117.0485 & long > -121.0177 & lat < 36)
socal <- socal[, 1:3]

fill1 <- (seq(min(socal$lat), max(socal$lat), by = .1))
fill1 <- data.frame(long = max(socal$long), lat = fill1, group = 4)
fill2 <- seq(min(socal$long), max(socal$long), by = .1)
fill2 <- data.frame(long = fill2, lat = max(socal$lat), group = 4)
socal <- rbind(socal, fill1, fill2)

socal <- socal[order(socal$long), ]
ggplot(socal, aes(x = long, y = lat)) + geom_polygon(aes(fill = group))

ggplot(crimes, aes(map_id = state)) + geom_map(aes(fill = Murder), map = socal, fill = 'black') +
  expand_limits(x = socal$long, y = socal$lat)

soca
ggplot(socal, aes(x = long, y = lat)) + geom_polygon(aes(fill = group))

geom_


ggplot() + coast





ggplot(subset(dat, Year == 2012), aes(x = SiteLat, y = SiteLon))

tt <- subset(dat, SiteName %in% c(205, 193, 187))

ggmap(extent = 'panel')
range(dat$)

coast <- borders('world', colour = 'black')
ggplot(dat, aes(x = Year, y = Nspecies), grouping = SiteName) +
   facet_wrap(~ SiteName) + geom_point() + coast


head(dat[, 1:9], n = 40)

dat %>% group_by(SiteName, VesName, DropNum, Year) %>% summarise(Nspecies = length(unique(ComName))) 
dat %>% group_by(Year, VesName, SiteName, DropNum) %>% summarise(Nspecies = length(unique(ComName))) 

dat %>% group_by(VesName) %>% summarise(nfish = mean(FishTime))

#Fit linear model to cpue at each site
#Goal is to see if nearby sites have similar trends in catch data
sites <- unique(dat$SiteName)
site.fits <- vector('list', length = length(sites))

for(ii in 1:length(sites))
{
  temp <- subset(dat, SiteName == sites[ii])
  site.fits[[ii]] <- lm(cpue ~ Year, data = temp)
}

#Find sites with negative, constant, positive intercept
site.coefs <- ldply(lapply(site.fits, FUN = function(x) return(x$coefficients)))

#Explore different conditions

#Look at everything in a pdf document
ranges <- data.frame(low = seq(-.1, .9, by = .01), 
                     high = seq(-.09, .91, by = .01))
ranges <- ranges[-101, ]

pdf(width = 7, height = 7, file = 'figs/locations_by_coefficient.pdf')
for(ii in 1:nrow(ranges)){
  try(plot_locations_by_coefficient(coefficients = site.coefs[, 2], 
        greater.than = ranges[ii, 1], 
        less.than = ranges[ii, 2]))
  # if(xx == 'nothing matches condition') next
}
dev.off()


#Now do it at at angler location, for anglers, gangion location
#For different models maybe?




#Motivation for mixed effects model because

#######Site level#######
#Bocaccio catch rate
#   dat %>% group_by(Year, SiteName, VesName) %>% summarize(avg_weight = mean(Weight.kg, na.rm = TRUE),
#     avg_length = mean(Length.cm, na.rm = TRUE), num_boc = sum(NumBoc), num_fish = sum(SurvFish),
#     num_hook = length(HookNum), 
#     cpue_boc = num_boc / num_hook, cpue_fish = num_fish / num_hook) %>% as.data.frame -> 
# site_cpue

#######Bocaccio
#Bocaccio cpue variation
# ggplot(site_cpue, aes(factor(VesName), cpue_boc)) + geom_violin() + geom_jitter(height = 0)

# ggplot(site_cpue, aes(factor(SiteName), cpue_boc, colour = VesName)) + 
#   geom_violin() + geom_jitter(height = 0)

# #Fit GLM for bocaccio cpue
# res <- lmer(cpue_boc ~ SiteName + Year + (1|VesName), data = site_cpue)
# res2 <- lmer(cpue_boc ~ SiteName + Year + (1 + |VesName), data = site_cpue)


# #######Fish 
# #Fish cpue variation
# ggplot(site_cpue, aes(factor(VesName), cpue_fish)) + geom_violin() + geom_jitter(height = 0)

# #Explore Variation in cpue by sitename
# ggplot(site_cpue, aes(factor(SiteName), cpue_fish)) + geom_violin() + geom_jitter(height = 0)



