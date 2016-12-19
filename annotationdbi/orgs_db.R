org.db.databases <- c("org.Ag.eg.db",	# Genome wide annotation for Anopheles
                      "org.At.tair.db",	# Genome wide annotation for Arabidopsis
                      "org.Bt.eg.db",	# Genome wide annotation for Bovine
                      "org.Ce.eg.db",	# Genome wide annotation for Worm
                      "org.Cf.eg.db",	# Genome wide annotation for Canine
                      "org.Dm.eg.db",	# Genome wide annotation for Fly
                      "org.Dr.eg.db",	# Genome wide annotation for Zebrafish
                      "org.EcK12.eg.db",	# Genome wide annotation for E coli strain K12
                      "org.EcSakai.eg.db",	# Genome wide annotation for E coli strain Sakai
                      "org.Gg.eg.db",	# Genome wide annotation for Chicken
                      "org.Hs.eg.db",	# Genome wide annotation for Human
                      "org.Mm.eg.db",	# Genome wide annotation for Mouse
                      "org.Mmu.eg.db",	# Genome wide annotation for Rhesus
                      #"org.Pf.plasmo.db",	# Genome wide annotation for Malaria
                      "org.Pt.eg.db",	# Genome wide annotation for Chimp
                      "org.Rn.eg.db",	# Genome wide annotation for Rat
                      "org.Sc.sgd.db",	# Genome wide annotation for Yeast
                      "org.Ss.eg.db",	# Genome wide annotation for Pig
                      "org.Xl.eg.db"	# Genome wide annotation for Xenopus
                      )

load_orgDb <- function(orgDb){

  if(!orgDb %in% installed.packages()[,"Package"]){
    source("https://bioconductor.org/biocLite.R")
    biocLite(orgDb)
    }
}

sapply(org.db.databases, load_orgDb, simplify = TRUE, USE.NAMES = TRUE)

lapply(org.db.databases, require, character.only = TRUE)


keytypes_list <- lapply(org.db.databases, function(x) NULL)
names(keytypes_list) <- paste(org.db.databases)

for (orgDb in org.db.databases){
  keytypes_list[[orgDb]] <- keytypes(get(orgDb))
}

library(rlist)

list.common(keytypes_list)


genes_per_organism <- data.frame(row.names = org.db.databases, number_entrez = rep(NA, length(org.db.databases)))

for (orgDb in org.db.databases){
  tryCatch(rm(genes))
  print(orgDb)
  tryCatch(genes <- keys(get(orgDb), keytype = "ENTREZID"))
  tryCatch(genes_per_organism[orgDb, 1] <- length(unique(genes)))
}


columns_list <- lapply(org.db.databases, function(x) NULL)
names(columns_list) <- paste(org.db.databases)

for (orgDb in org.db.databases){
  columns_list[[orgDb]] <- columns(get(orgDb))
}

list.common(columns_list)




##########################

library(AnnotationHub)
ah = AnnotationHub()

unique(ah$dataprovider)
unique(ah$species)
unique(ah$rdataclass)

query(ah, "OrgDb")
query(ah, c("OrgDb", "Takifugu"))
takifugu <- ah[["AH48147"]]
keytypes(takifugu)
columns(takifugu)
egid <- head(keys(takifugu, "ENTREZID"))
select(takifugu, egid, c("SYMBOL", "GENENAME"), "ENTREZID")

dm <- query(ah, c("ChainFile", "UCSC", "Drosophila melanogaster"))
dm

df <- mcols(dm)
