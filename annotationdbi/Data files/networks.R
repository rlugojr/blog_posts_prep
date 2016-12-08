# http://kateto.net/network-visualization

library('network')
library('sna')
library('ndtv')
library('visNetwork')
library(igraph)

nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL


nodes2 <- read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)

head(nodes2)
head(links2)
links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)

library('igraph')
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
net

plot(net) # not a pretty picture!
net <- simplify(net, remove.multiple = F, remove.loops = T)
plot(net, edge.arrow.size=.4,vertex.label=NA)

net2 <- graph_from_incidence_matrix(links2)
table(V(net2)$type)

net2 <- simplify(net2, remove.multiple = F, remove.loops = T)
plot(net2, edge.arrow.size=.4,vertex.label=NA)

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12
plot(net)

legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

plot(net, vertex.shape="none", vertex.label=V(net)$media,
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")

par(mfrow=c(1,2))

# Community detection based on label propagation:
clp <- cluster_label_prop(net)
class(clp)

# Community detection returns an object of class "communities"
# which igraph knows how to plot:
plot(clp, net)

# We can also plot the communities without relying on their built-in plot:
V(net)$community <- clp$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])
dev.off()

netm <- as_adjacency_matrix(net, attr="weight", sparse=F)
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

palf <- colorRampPalette(c("gold", "dark orange"))
heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(100),
        scale="none", margins=c(10,10) )

# Media outlets are blue squares, audience nodes are orange circles:
V(net2)$color <- c("steel blue", "orange")[V(net2)$type+1]
V(net2)$shape <- c("square", "circle")[V(net2)$type+1]

# Media outlets will have name labels, audience members will not:
V(net2)$label <- ""
V(net2)$label[V(net2)$type==F] <- nodes2$media[V(net2)$type==F]
V(net2)$label.cex=.6
V(net2)$label.font=2

plot(net2, vertex.label.color="white", vertex.size=(2-V(net2)$type)*8)


# install.packages('png')
library('png')

img.1 <- readPNG("./images/news.png")
img.2 <- readPNG("./images/user.png")

V(net2)$raster <- list(img.1, img.2)[V(net2)$type+1]

plot(net2, vertex.shape="raster", vertex.label=NA,
     vertex.size=16, vertex.size2=16, edge.width=2)

library('visNetwork')
visNetwork(nodes, links, width="100%", height="400px", main="Network!")

nodes$shape <- "dot"
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$title <- nodes$media # Text on click
nodes$label <- nodes$type.label # Node label
nodes$size <- nodes$audience.size # Node size
nodes$borderWidth <- 2 # Node border width

nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"

visNetwork(nodes, links)

links$width <- 1+links$weight/8 # line width
links$color <- "gray"    # line color
links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
links$smooth <- FALSE    # should the edges be curved?
links$shadow <- FALSE    # edge shadow

visNetwork(nodes, links)
detach('package:visNetwork')

library('network')

net3 <- network(links,  vertex.attr=nodes, matrix.type="edgelist",
                loops=F, multiple=F, ignore.eval = F)

net3[,]
net3 %n% "net.name" <- "Media Network" #  network attribute
net3 %v% "media"    # Node attribute
net3 %e% "type"     # Node attribute

net3 %v% "col" <- c("gray70", "tomato", "gold")[net3 %v% "media.type"]
plot(net3, vertex.cex=(net3 %v% "audience.size")/7, vertex.col="col")


library('ndtv')
net3

par(mar=c(0,0,0,0))

render.d3movie(net3, usearrows = F, displaylabels = F, bg="#111111",
               vertex.border="#ffffff", vertex.col =  net3 %v% "col",
               vertex.cex = (net3 %v% "audience.size")/8,
               edge.lwd = (net3 %e% "weight")/3, edge.col = '#55555599',
               vertex.tooltip = paste("<b>Name:</b>", (net3 %v% 'media') , "<br>",
                                      "<b>Type:</b>", (net3 %v% 'type.label')),
               edge.tooltip = paste("<b>Edge type:</b>", (net3 %e% 'type'), "<br>",
                                    "<b>Edge weight:</b>", (net3 %e% "weight" ) ),
               launchBrowser=F, filename="Media-Network.html" )

#If you are going to embed the plot in a markdown document, use output.mode='inline' above.

###

# Libraries
library("networkD3")
library('randomNames')
library("igraph")

# Create data with the randomNames package :
NUMOFLINKS = 100
relations = data.frame(source = randomNames(1000,which.names='both'), target = "")
relations = relations[rep(seq_len(nrow(relations)), sample(1:10,nrow(relations), replace=T)),]
relations = relations[sample(nrow(relations),NUMOFLINKS),]
relations$target = sample(relations$source,nrow(relations), replace = T)
relations = relations[relations[,1]!=relations[,2], ]
vertices<-data.frame("name" = unique(unlist(relations))) # node names
g = graph.data.frame(relations, directed=F, vertices=vertices) # raw graph
vertices$group = edge.betweenness.community(g)$membership # betweeness centrality for each node for grouping


# create indices for each name to fit forceNetwork data format
relations$source.index = match(relations$source, vertices$name)-1
relations$target.index = match(relations$target, vertices$name)-1

# sophisticated network graph
d3 = forceNetwork(Links = relations, Nodes = vertices,
                  Source = "source.index", Target = "target.index",
                  NodeID = "name",
                  Group = "group", # color nodes by betweeness calculated earlier
                  charge = -70, # node repulsion
                  linkDistance = 25,
                  zoom = T)

# If you want to show the graph
#show(d3)

#If you want to save the graph as html file
saveNetwork(d3,file = '#87_Interactive_network.html',selfcontained = F)

library("networkD3")
library("igraph")

# Download prepared igraph file from github
url <- "https://github.com/andrie/cran-network-structure/blob/master/pdb/depGraph-CRAN.rds?raw=true"
datafile <- tempfile(fileext = ".rds")
download.file(url, destfile = datafile, mode = "wb")
gs <- readRDS(datafile)

# Remove all nodes with fewer than 50 edges
deg <- degree(gs, mode = "out")
idx <- names(which(deg > 50))
gn <- induced.subgraph(gs, idx)

# Extract into data frame and plot
gd <- get.data.frame(gn, what = "edges")
simpleNetwork(gd, fontSize = 12)


edgeList <- relations[, -4]
nodeList <- vertices

# Read a data set.
# Data format: dataframe with 3 variables; variables 1 & 2 correspond to interactions; variable 3 is weight of interaction
colnames(edgeList) <- c("SourceName", "TargetName", "Weight")

# Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
gD <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=FALSE))

# Create a node list object (actually a data frame object) that will contain information about nodes
nodeList <- data.frame(ID = c(0:(igraph::vcount(gD) - 1)), # because networkD3 library requires IDs to start at 0
                       nName = igraph::V(gD)$name)

# Map node names from the edge list to node IDs
getNodeID <- function(x){
  which(x == igraph::V(gD)$name) - 1 # to ensure that IDs start at 0
}
# And add them to the edge list
edgeList <- plyr::ddply(edgeList, .variables = c("SourceName", "TargetName", "Weight"),
                        function (x) data.frame(SourceID = getNodeID(x$SourceName),
                                                TargetID = getNodeID(x$TargetName)))

############################################################################################
# Calculate some node properties and node similarities that will be used to illustrate
# different plotting abilities and add them to the edge and node lists

# Calculate degree for all nodes
nodeList <- cbind(nodeList, nodeDegree=igraph::degree(gD, v = igraph::V(gD), mode = "all"))

# Calculate betweenness for all nodes
betAll <- igraph::betweenness(gD, v = igraph::V(gD), directed = FALSE) / (((igraph::vcount(gD) - 1) * (igraph::vcount(gD)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
nodeList <- cbind(nodeList, nodeBetweenness=100*betAll.norm) # We are scaling the value by multiplying it by 100 for visualization purposes only (to create larger nodes)
rm(betAll, betAll.norm)

#Calculate Dice similarities between all pairs of nodes
dsAll <- igraph::similarity.dice(gD, vids = igraph::V(gD), mode = "all")

F1 <- function(x) {data.frame(diceSim = dsAll[x$SourceID +1, x$TargetID + 1])}
edgeList <- plyr::ddply(edgeList, .variables=c("SourceName", "TargetName", "Weight", "SourceID", "TargetID"),
                        function(x) data.frame(F1(x)))

rm(dsAll, F1, getNodeID, gD)

############################################################################################
# We will also create a set of colors for each edge, based on their dice similarity values
# We'll interpolate edge colors based on the using the "colorRampPalette" function, that
# returns a function corresponding to a collor palete of "bias" number of elements (in our case, that
# will be a total number of edges, i.e., number of rows in the edgeList data frame)
F2 <- colorRampPalette(c("#FFFF00", "#FF0000"), bias = nrow(edgeList), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(edgeList$diceSim)))
edges_col <- sapply(edgeList$diceSim, function(x) colCodes[which(sort(unique(edgeList$diceSim)) == x)])

rm(colCodes, F2)
############################################################################################
# Let's create a network

D3_network_LM <- networkD3::forceNetwork(Links = edgeList, # data frame that contains info about edges
                                         Nodes = nodeList, # data frame that contains info about nodes
                                         Source = "SourceID", # ID of source node
                                         Target = "TargetID", # ID of target node
                                         Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
                                         NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
                                         Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
                                         Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
                                         height = 500, # Size of the plot (vertical)
                                         width = 1000,  # Size of the plot (horizontal)
                                         fontSize = 20, # Font size
                                         linkDistance = networkD3::JS("function(d) { return 10*d.value; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
                                         linkWidth = networkD3::JS("function(d) { return d.value/5; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
                                         opacity = 0.85, # opacity
                                         zoom = TRUE, # ability to zoom when click on the node
                                         opacityNoHover = 0.1, # opacity of labels when static
                                         linkColour = edges_col) # edge colors

# Plot network
D3_network_LM

# Save network as html file
networkD3::saveNetwork(D3_network_LM, "D3_LM.html", selfcontained = TRUE)

