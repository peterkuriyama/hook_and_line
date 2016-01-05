##
# Hook and Line Analysis
setwd('/Users/peterkuriyama/School/Research/hook_and_line/')

source('R/hl_startup.R')

head(dat)

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
#Are certain species caught more on certain hooks?






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







