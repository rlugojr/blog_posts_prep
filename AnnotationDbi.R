library(AnnotationDbi)

library(org.Hs.eg.db)

columns(org.Hs.eg.db)
keytypes(org.Hs.eg.db)

keys(org.Hs.eg.db, keytype="SYMBOL")

#1st get some example keys
k <- keys(org.Hs.eg.db,keytype="ENTREZID")
length(k)

# then call select
kk <- k[1:3]
columns <- c("SYMBOL","UCSCKG")
kkk <- select(org.Hs.eg.db, keys=kk, columns=columns, keytype="ENTREZID")
kkk

REFSEQ_org <- keys(org.Hs.eg.db, keytype = "REFSEQ")

# Refseq includes the following ID types:
unique(gsub("_[0-9]*", "", REFSEQ_org))

# Here, I only want NM (mRNA) and NR (RNA)
REFSEQ_org <- grep(
  paste(c("NM_", "NR_"), collapse = "|"),
  REFSEQ_org, value = TRUE)


library(GO.db)
GOIDs <- c("GO:0042254","GO:0044183")
select(GO.db, keys=GOIDs, columns="DEFINITION", keytype="GOID")

library(TxDb.Hsapiens.UCSC.hg38.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene
txdb

columns(txdb)
keys <- keys(txdb, keytype="GENEID")
cols <- c("TXID", "TXNAME", "TXTYPE")
select(txdb, keys=keys, columns=cols, keytype="GENEID")

library(EnsDb.Hsapiens.v79)
edb <- EnsDb.Hsapiens.v79
edb
columns(edb)

## Get the first
keys <- head(keys(edb, keytype="GENEID"))
## Get the data
head(select(edb, keys=keys, columns=c("ENTREZID", "GENENAME", "TXID", "TXBIOTYPE"), keytype="GENEID"))


listColumns(edb)
listTxbiotypes(edb)
listTables(edb)

listGenebiotypes(edb)
a <- head(keys(edb, filter=list(GenebiotypeFilter("lincRNA"))))

b <- head(keys(edb, filter=list(SeqnameFilter("1"))))



## Retrieve all gene IDs of all lincRNAs encoded on chromosome Y
linkY <- keys(edb, filter=list(GenebiotypeFilter("lincRNA"), SeqnameFilter("Y")))
length(linkY)

## We get now all transcripts for these genes.
txs <- select(edb, keys=linkY, columns=c("TXID", "TXSEQSTART", "TXBIOTYPE"),
              keytype="GENEID")
nrow(txs)

## Alternatively, we could specify/pass the filters with the keys argument.
txs <- select(edb, keys=list(GenebiotypeFilter("lincRNA"), SeqnameFilter("Y")),
              columns=c("TXID", "TXSEQSTART", "TXBIOTYPE"))
