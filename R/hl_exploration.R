



site.loc <- dat %>% group_by(SiteName) %>% summarise(lat = mean(Lat.DD), lon = -mean(Lon.DD))

#Plot Site Locations
map('worldHires', xlim = c(-121, -120.5), ylim = c(34.3, 35), col = 'black', fill = TRUE, )
points(site.loc$lon, site.loc$lat, pch = 19, col = 'red')
axis(side = 1)
axis(side = 2, las = 2)

#Pt Conception ones
# ptcon <- subset(dat, Lat.DD > 34.3, Lon.DD > 120.5)
locs <- unique(dat[dat$Lon.DD > 120.5, 'SiteName'])

ptcon <- subset(dat, SiteName %in% locs)
map('worldHires', xlim = c(-121, -120.5), ylim = c(34.3, 35), col = 'black', fill = TRUE, )
points(-ptcon$Lon.DD, ptcon$Lat.DD, pch = 19, col = 'red')

ptcon <- ptcon %>% group_by(Year, SiteName) %>% mutate(cpue = sum(SurvFish) / length(SurvFish),
           Nfish = sum(SurvFish)) %>% as.data.frame
ggplot(ptcon, aes(x = Year, y = cpue)) + facet_grid(SiteName~.) + geom_point() + theme_bw()

#Fit Linear models to each site
eight.five <- subset(ptcon, SiteName == '185')
est <- lm(cpue ~ Year, data = eight.five)

plot(eight.five$Year, eight.five$cpue, ylim = c(0, 1), pch = 19)
lines(est$model$Year, est$fitted.values, col = 'red')


est$fitted.values
plo




#Filter furthest north sites to look for trends
site




site <- dat %>% group_by(Year, SiteName) %>% summarise(cpue = sum(SurvFish) / length(SurvFish),
  Nfish = sum(SurvFish)) %>%
          as.data.frame
ggplot(site, aes(x = Year, y = cpue)) + facet_wrap(~SiteName) + geom_point()
ggplot(site, aes(x = Year, y = Nfish)) + facet_wrap(~SiteName) + geom_point()


unique(xx$SiteName)
dat %>% group_by()










#Vessel Level
agg <- subset(dat, VesName == 'Aggressor')

drop <- dat %>% group_by(SetID, DropNum) %>% mutate(Nfish = sum(SurvFish), FishRate = Nfish / length(Nfish),
          Nspecies = length(unique(ComName)),
          Nbocac = sum(NumBoc), BocacRate = Nbocac / length(NumBoc), Comms = paste(ComName, collapse = ';'),
          MeanFishTime = mean(FishTime)) %>% 
          as.data.frame
agg <- subset(drop, VesName == 'Aggressor')

plot(agg$MeanFishTime, agg$FishRate, pch = 19)

#Map Stuff



ggplot(agg, aes(x = MeanFishTime, y = FishRate)) + geom_point() + facet_wrap(~ Year)



ggplot(agg, aes(x = FishRate)) + geom_histogram(binwidth = .1h) + facet_wrap(~ Nspecies)

hist(drop$FishRate)
unique(agg$HookRslt)



#drop Level
#plot catch rates in each location
gang <- dat %>% group_by(SetID, DropNum, AngNum) %>% mutate(Nfish = sum(SurvFish), FishRate = Nfish / 5,
          Nspecies = length(unique(ComName)),
          Nbocac = sum(NumBoc), BocacRate = Nbocac / 5, Comms = paste(ComName, collapse = ';')) %>% 
          as.data.frame

ggplot(gang, grouping = VesName, aes(x = FishRate)) + geom_histogram(binwidth = .2) + 
  facet_wrap(~ VesName)
range(subset(gang, VesName == 'Toronado')$Year)



ggplot(gang, grouping = Nspecies, aes(x = FishRate)) + geom_histogram() + facet_wrap(~ Nspecies)

ggplot(gang, grouping = Year, aes(x = FishRate)) + geom_histogram() + facet_wrap(~ Year)





hist(gang$FishRate)


paste(c('po', 'op'), collapse = ';')

dat[dat$SetID == "14-01-06-054", ]





