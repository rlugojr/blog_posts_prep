library(annotationTools)

##example Homologene file and its location
homologeneFile<-system.file('extdata','homologene_part.data',package='annotationTools')

##load Homologene file
homologene<-read.delim(homologeneFile,header=FALSE)

##get mouse (species ID 10090) orthologs of several human (species ID 9606) gene ID (among those: 5982, gene symbol RFC2 and 93587, gene symbol: RG9MTD2)
myGenes<-c(5982,93587,NA,100)
getHOMOLOG(myGenes,10090,homologene)

##track origin of annotation failure for the last 2 gene IDs
getHOMOLOG(myGenes,10090,homologene,diagnose=TRUE)

##get mouse gene belonging to homologene cluster IDs 6885 and 6886
myClusters<-c(6885,6886)
getHOMOLOG(myClusters,10090,homologene,cluster=TRUE)

###

library(AnnotationDbi)
library("org.Hs.eg.db")
library("org.Mm.eg.db")
library("hom.Hs.inp.db")
library("hom.Mm.inp.db")

##Some IDs just for the example
library("org.Hs.eg.db")
ids = as.list(org.Hs.egUNIPROT)[10000:10500] ##get some ragged IDs
## Get entrez gene IDs (default) for uniprot IDs mapping from human to mouse.
MouseEGs = inpIDMapper(ids, "HOMSA", "MUSMU")
MousePATHs = idConverter(MouseEGs, srcSpecies="MUSMU", destSpecies="MUSMU",
                         srcIDType="EG", destIDType="PATH")

###

library("biomaRt")

ensembl=useMart("ensembl")
listDatasets(ensembl)

ensembl = useDataset("hsapiens_gene_ensembl",mart=ensembl)

filters = listFilters(ensembl)
filters[1:5,]

attributes = listAttributes(ensembl)
attributes[1:5,]

getLDS(attributes, filters = "", values = "", mart, attributesL,
       filtersL = "", valuesL = "", martL, verbose = FALSE, uniqueRows = TRUE, bmHeader=TRUE)

human = useMart("ensembl", dataset = "hsapiens_gene_ensembl")
mouse = useMart("ensembl", dataset = "mmusculus_gene_ensembl")
getLDS(attributes = c("hgnc_symbol","chromosome_name", "start_position"), filters = "hgnc_symbol", values = "TP53", mart = human, attributesL = c("chromosome_name","start_position"), martL = mouse)

getLDS(attributes = c("hgnc_symbol","chromosome_name", "start_position"),
       filters = "hgnc_symbol", values = "TP53",mart = human,
       attributesL = c("refseq_mrna","chromosome_name","start_position"), martL = mouse)
---

  ```{r echo = TRUE, message = FALSE, warning = FALSE, cache=FALSE, fig.width = 6, fig.height = 5, fig.align = "center"}
human = useMart("ensembl", dataset = "hsapiens_gene_ensembl")
mouse = useMart("ensembl", dataset = "mmusculus_gene_ensembl")
guinea_pig = useMart("ensembl", dataset = "cporcellus_gene_ensembl")
cat = useMart("ensembl", dataset = "fcatus_gene_ensembl")
rat = useMart("ensembl", dataset = "rnorvegicus_gene_ensembl")
chimp = useMart("ensembl", dataset = "ptroglodytes_gene_ensembl")
boar = useMart("ensembl", dataset = "sscrofa_gene_ensembl")
horse = useMart("ensembl", dataset = "ecaballus_gene_ensembl")
rhesus_macaque = useMart("ensembl", dataset = "mmulatta_gene_ensembl")
gorilla = useMart("ensembl", dataset = "ggorilla_gene_ensembl")
cow = useMart("ensembl", dataset = "btaurus_gene_ensembl")
dog = useMart("ensembl", dataset = "cfamiliaris_gene_ensembl")
```

```{r echo = TRUE, message = FALSE, warning = FALSE, cache=FALSE, fig.width = 6, fig.height = 5, fig.align = "center"}
#filters = listFilters(human)
#filters[1:5,]

#attributes = listAttributes(human)
#attributes[1:5,]
```

## get all genes from human with hgnc symbol

https://shiring.github.io/genome/2016/10/23/AnnotationDbi

Because it would take too long for all genes, I'm only taking protein coding genes.

```{r echo = TRUE, message = FALSE, warning = FALSE, cache=FALSE, fig.width = 6, fig.height = 5, fig.align = "center"}
library(AnnotationDbi)
library(EnsDb.Hsapiens.v79)
human_EnsDb <- keys(EnsDb.Hsapiens.v79, keytype = "GENEID")

human_gene_EnsDb <- ensembldb::select(EnsDb.Hsapiens.v79, keys=human_EnsDb,
columns="GENEBIOTYPE", keytype="GENEID")

human_prot_coding_genes <- human_gene_EnsDb[which(human_gene_EnsDb$GENEBIOTYPE == "protein_coding"), ]
```

```{r echo = TRUE, message = FALSE, warning = FALSE, cache=FALSE, fig.width = 6, fig.height = 5, fig.align = "center"}
#library(EnsDb.Mmusculus.v79)
#mouse_EnsDb <- keys(EnsDb.Mmusculus.v79, keytype = "GENEID")

#mouse_gene_EnsDb <- ensembldb::select(EnsDb.Mmusculus.v79, keys=mouse_EnsDb,
#                                          columns="GENEBIOTYPE", keytype="GENEID")
#mouse_prot_coding_genes <- mouse_gene_EnsDb[which(mouse_gene_EnsDb$GENEBIOTYPE == "protein_coding"), ]
```

```{r echo = TRUE, message = FALSE, warning = FALSE, cache=FALSE, fig.width = 6, fig.height = 5, fig.align = "center"}
#library(EnsDb.Rnorvegicus.v79)
#rat_EnsDb <- keys(EnsDb.Rnorvegicus.v79, keytype = "GENEID")

#rat_gene_EnsDb <- ensembldb::select(EnsDb.Rnorvegicus.v79, keys=rat_EnsDb,
#                                      columns="GENEBIOTYPE", keytype="GENEID")

#rat_prot_coding_genes <- rat_gene_EnsDb[which(rat_gene_EnsDb$GENEBIOTYPE == "protein_coding"), ]
```

## how many genes are there for each species?


## how big is the genome of each species?


## homologous genes

```{r echo = TRUE, message = FALSE, warning = FALSE, cache=FALSE, fig.width = 6, fig.height = 5, fig.align = "center"}
specieslist <- c("mouse", "rat", "guinea_pig", "cat", "chimp", "boar", "horse", "rhesus_macaque", "gorilla", "cow", "dog")

for (species in specieslist){
print(species)
assign(paste0("homologs_human_", species), getLDS(attributes = c("ensembl_gene_id", "chromosome_name"),
filters = "ensembl_gene_id",
values = human_prot_coding_genes$GENEID,
mart = human,
attributesL = c("ensembl_gene_id", "chromosome_name"),
martL = get(species)))
}
```


```{r echo = TRUE, message = FALSE, warning = FALSE, cache=FALSE, fig.width = 6, fig.height = 5, fig.align = "center"}
library(dplyr)

for (species in specieslist){
homologs_human <- left_join(human_prot_coding_genes, get(paste0("homologs_human_", species)), by = c("GENEID" = "Ensembl.Gene.ID"))
homologs_human[, paste0(species)] <- ifelse(is.na(homologs_human$Ensembl.Gene.ID.1), 0, 1)
homologs_human <- homologs_human[, c(1, 6)]
homologs_human <- homologs_human[!duplicated(homologs_human$GENEID), ]
assign(paste0("homologs_all_human_", species), homologs_human)
}
```

```{r echo = TRUE, message = FALSE, warning = FALSE, cache=FALSE, fig.width = 6, fig.height = 5, fig.align = "center"}
homologs_all <- left_join(homologs_all_human_mouse, homologs_all_human_rat, by = "GENEID") %>%
left_join(homologs_all_human_guinea_pig, by = "GENEID") %>%
left_join(homologs_all_human_cat, by = "GENEID") %>%
left_join(homologs_all_human_chimp, by = "GENEID") %>%
left_join(homologs_all_human_boar, by = "GENEID") %>%
left_join(homologs_all_human_horse, by = "GENEID") %>%
left_join(homologs_all_human_rhesus_macaque, by = "GENEID") %>%
left_join(homologs_all_human_gorilla, by = "GENEID") %>%
left_join(homologs_all_human_cow, by = "GENEID") %>%
left_join(homologs_all_human_dog, by = "GENEID")

```

```{r echo = TRUE, message = FALSE, warning = FALSE, cache=FALSE, fig.width = 10, fig.height = 10, fig.align = "center"}
gene_matrix <- homologs_all[, -1]
gene_matrix$human <- 1

number_genes <- colSums(gene_matrix)

co_occurrence <- t(as.matrix(gene_matrix)) %*% as.matrix(gene_matrix)
co_occurrence[1:11, 1:11] <- 0

# plot the network graph
library(igraph)
g <- graph_from_adjacency_matrix(co_occurrence,
weighted = TRUE,
diag = FALSE,
mode = "plus")

g <- simplify(g, remove.multiple = F, remove.loops = T, edge.attr.comb = c(weight = "sum", type = "ignore"))

plot(g,
vertex.label.family = "Helvetica",
vertex.label.font = 1,
vertex.shape = "sphere",
vertex.size=number_genes/1000,
vertex.label.cex=0.8,
vertex.label.color="black",
vertex.frame.color = NA,
edge.width = E(g)$weight/1000,
edge.curved=.1,
layout=layout_in_circle)

```

```{r echo = TRUE, message = FALSE, warning = FALSE, cache=FALSE, fig.width = 10, fig.height = 10, fig.align = "center"}
gene_matrix <- homologs_all[, -1]
gene_matrix$human <- 1

number_genes <- colSums(gene_matrix)


# plot the network graph
library(igraph)
g <- graph_from_incidence_matrix(gene_matrix[1:10, ], directed = FALSE, mode = "all")

plot(g)

#clustering
wc <- cluster_walktrap(g)
members <- membership(wc)

library(networkD3)

net_d3 <- igraph_to_networkD3(g, group = members)

# Create force directed network plot
forceNetwork(Links = net_d3$links, Nodes = net_d3$nodes, zoom=T, opacity = 1,
Source = 'source', Target = 'target',
linkDistance = 55, bounded=F, colourScale = JS("d3.scale.category10()"),
NodeID = 'name', Group = 'group',
fontSize=8, opacityNoHover = .7)
```
