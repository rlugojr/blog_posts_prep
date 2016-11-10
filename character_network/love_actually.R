library(dplyr)
library(stringr)
library(tidyr)

setwd("U:/Github_blog/blog_posts_prep/character_network")

raw <- readLines("love_actually.txt")

lines <- data_frame(raw = raw) %>%
  filter(raw != "", !str_detect(raw, "(song)")) %>%
  mutate(is_scene = str_detect(raw, " Scene "),
         scene = cumsum(is_scene)) %>%
  filter(!is_scene) %>%
  separate(raw, c("speaker", "dialogue"), sep = ":", fill = "left") %>%
  group_by(scene, line = cumsum(!is.na(speaker))) %>%
  summarize(speaker = speaker[1], dialogue = str_c(dialogue, collapse = " "))

head(lines)

cast <- read.csv(url("http://varianceexplained.org/files/love_actually_cast.csv"))

lines <- lines %>%
  inner_join(cast) %>%
  mutate(character = paste0(speaker, " (", actor, ")"))

by_speaker_scene <- lines %>%
  count(scene, character)

by_speaker_scene

library(reshape2)
speaker_scene_matrix <- by_speaker_scene %>%
  acast(character ~ scene, fun.aggregate = length)

dim(speaker_scene_matrix)

norm <- speaker_scene_matrix / rowSums(speaker_scene_matrix)

h <- hclust(dist(norm, method = "manhattan"))

plot(h)

ordering <- h$labels[h$order]
ordering

library(ggplot2)
scenes <- by_speaker_scene %>%
  filter(n() > 1) %>%        # scenes with > 1 character
  ungroup() %>%
  mutate(scene = as.numeric(factor(scene)),
         character = factor(character, levels = ordering))

ggplot(scenes, aes(scene, character)) +
  geom_point() +
  geom_path(aes(group = scene))

non_airport_scenes <- speaker_scene_matrix[, colSums(speaker_scene_matrix) < 10]

cooccur <- non_airport_scenes %*% t(non_airport_scenes)

heatmap(cooccur)


library(igraph)
g <- graph.adjacency(cooccur, weighted = TRUE, mode = "undirected", diag = FALSE)
plot(g, edge.width = E(g)$weight)

library('igraph')
library('network')
library('sna')
library('ndtv')
library('visNetwork')

net <- simplify(g, remove.multiple = F, remove.loops = T)

plot(net, edge.arrow.size=.4,, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$media, vertex.label.color="black", edge.curved=.1)



