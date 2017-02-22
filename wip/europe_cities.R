library(europop)

data("europop")

head(europop)

library("ggmap")
library(maptools)
library(maps)

library(dplyr)
library(ggplot2)
library(gganimate)
library(mapproj)
library(RColorBrewer)

library(ggrepel)


visited_1 <- unique(europop$city)
ll.visited_1 <- geocode(visited_1)

ll.visited_1 <- cbind(ll.visited_1, visited_1)

write.table(ll.visited_1, "ll.visited_1.txt")

ll.visited_1 <- ll.visited_1[!is.na(ll.visited_1$lon), ]

ll.visited_2 <- left_join(europop, ll.visited_1, by = c("city" = "visited_1"))
ll.visited_2 <- ll.visited_2[!is.na(ll.visited_2$lon), ]

europe <- get_map(location = 'Germany', zoom = 4)

summary(ll.visited_2)

ggmap(europe) +
  geom_point(data = subset(ll.visited_2, year == "1800"), aes(x = lon, y = lat, color = population, size = population), alpha = 0.5) +
  scale_color_gradient(low = "blue", high = "red") +
  theme(legend.position = "right") #+
  #geom_text_repel(data = subset(ll.visited_2, year == "1800" & population > 150), aes(label = city))

subset(ll.visited_2, city == "SOEST") %>%
  ggplot(aes(x = year, y = population)) +
  geom_point() +
  geom_line()
