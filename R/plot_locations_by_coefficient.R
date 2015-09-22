######in progress, put in it's own script eventually

#Plot Locations with Linear model coefficients that meet conditions

#arguments
# coefficients - VECTOR: grouped by site, vessel, vessel positioning, gangion, angler, 
# condition - to group things

#output
#Just a map

# coefficients <- site.coefs$Year
# condition <- "<= 0"
# title <- 'Negative Slope'

plot_locations_by_coefficient <- function(locations = 1, coefficients, condition, 
  title, title.cex = 2)
{
  condition <- paste('coefficients', condition)
  site.ind <- which(eval(parse(text = condition))) #indexed sites
  site.to.plot <- sites[site.ind]

  to.plot <- dat[dat$SiteName %in% site.to.plot, ]
  #Pull Unique rows
  keepers <- to.plot[duplicated(to.plot$SiteName) == FALSE, ]

  #Mapping
  lon.range <- range(-keepers$SiteLon)
  lon.range <- c(lon.range[1] - .5, lon.range[2] + .5)

  lat.range <- range(keepers$SiteLat)
  lat.range <- c(lat.range[1] - .5, lat.range[2] + .5)

  map('worldHires', xlim = lon.range, ylim = lat.range, col = 'black', fill = TRUE)  
  points(-keepers$SiteLon, keepers$SiteLat, pch = 19, col = 'red')
  mtext(title, side = 3, cex = title.cex)

}