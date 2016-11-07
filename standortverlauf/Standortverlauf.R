
setwd("U:/Github_blog/Takeout/Standortverlauf")

# https://takeout.google.com/

library(jsonlite)
system.time(x <- fromJSON("Standortverlauf.json"))
loc = x$locations
loc$time = as.POSIXct(as.numeric(x$locations$timestampMs)/1000,
                      origin = "1970-01-01")
head(loc)


loc$lat = loc$latitudeE7 / 1e7
loc$lon = loc$longitudeE7 / 1e7

library(sp)
loc.sp = loc
coordinates(loc.sp) = ~lon+lat
proj4string(loc.sp) = CRS("+proj=longlat +datum=WGS84")

library(spacetime)
library(trajectories)
tr = Track(STIDF(geometry(loc.sp), loc.sp$time, loc.sp@data))
plot(tr)

loc$day <- as.factor(format(as.Date(loc$time,format="%Y-%m-%d"), "%Y-%m-%d"))
range(loc$time)

library(ggmap)

map <- get_map(location = 'London', zoom = 1, maptype = "roadmap")

ggmap(map, darken = c(0.3, "white")) + geom_point(aes(x = lon, y = lat), data = loc) +
  theme(legend.position="none") + labs(x="Longitude", y="Latitude", title="Location history 2013 to 2016")

loc <- loc[, -5]
loc2 <- with(loc, subset(loc, loc$time > as.POSIXct('2016-09-09 18:00:00')))
loc2 <- with(loc, subset(loc, loc$time < as.POSIXct('2016-09-27 06:00:00')))

map <- get_map(location = 'Dayton', zoom = 6, maptype = "roadmap")

ggmap(map, darken = c(0.3, "white")) + geom_point(aes(x = lon, y = lat), data = loc2) +
  theme(legend.position="none") + labs(x="Longitude", y="Latitude", title="Location history Roadtrip 2016")


loc3 <- with(loc, subset(loc, loc$time > as.POSIXct('2014-01-03 16:00:00')))
loc3 <- with(loc, subset(loc, loc$time < as.POSIXct('2014-01-13 11:00:00')))

map <- get_map(location = 'Tenerife', zoom = 10, maptype = "roadmap")

ggmap(map, darken = c(0.3, "white")) + geom_point(aes(x = lon, y = lat), data = loc3, alpha = 0.3) +
  theme(legend.position="none") + labs(x="Longitude", y="Latitude", title="Location history Tenerife 2014")

###

map <- get_map(location = 'Germany', zoom = 5, maptype = "roadmap")

ggmap(map, darken = c(0.3, "white")) + geom_point(aes(x = lon, y = lat), data = loc, alpha = 0.3) +
  theme(legend.position="none") + labs(x="Longitude", y="Latitude", title="Location history Germany")

###

map <- get_map(location = 'Munster', zoom = 12, maptype = "roadmap")

ggmap(map, extent = "panel", maprange=FALSE) +
  stat_bin2d(aes(x = lon, y = lat), data = loc, bins = 1000, geom = "tile") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(legend.position="right") + labs(x="Longitude", y="Latitude", title="Location history Münster")


ggmap(map) +
  geom_density2d(data = loc,
                   aes(x = lon, y = lat), size = 0.3) +
  stat_density2d(
  aes(x=lon, y=lat, fill=..level.., alpha = ..level..),
  data=loc,
  size = 0.01,
  bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0, 0.3), guide = FALSE)

###

library(maps)
library(ggplot2)
map.dat <- map_data("world")
ggplot() + geom_polygon(aes(long,lat, group=group), fill="grey65", data=map.dat) + theme_bw() + theme(axis.text = element_blank(), axis.title=element_blank())

ggplot(map.dat, aes(x=long, y=lat, group=group, fill=region))+ geom_polygon() + theme(legend.position = "none")

###

map <- get_map(location = 'London', zoom = 11, maptype = "roadmap")

ggmap(map, darken = c(0.3, "white")) + geom_point(aes(x = lon, y = lat), data = loc, alpha = 0.3) +
  theme(legend.position="none") + labs(x="Longitude", y="Latitude", title="Location history")


map <- get_map(location = 'Warsaw', zoom = 13, maptype = "roadmap")

ggmap(map, darken = c(0.3, "white")) + geom_point(aes(x = lon, y = lat), data = loc, alpha = 0.3) +
  theme(legend.position="none") + labs(x="Longitude", y="Latitude", title="Location history")
