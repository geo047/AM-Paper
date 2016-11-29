# match am+ results to genes using map.txt file from Toni
# Last modified:  21/07/2016
# performed on my mac through RStudio


# Summary
# Results do not look very good. Some good genes but lots of noise as well. 
# How to move forward:  inlude PC into the analysis
snps <- c(
  "Gga_rs14707919",
 # "GGaluGA204479",
  "Gga_rs16469122",
  #"GGaluGA300550",
  "GGaluGA108350",
  "Gga_rs13595487",
  "GGaluGA283510",
  "Gga_rs14027776")
  

  
## map with genes file
map <- read.table("/Users/geo047/AM+/ChickenResults/map.txt", header=FALSE)
names(map) <- c("SNP", "Pos", "Chr", "dist_to_gene", "Gene")


##----------------
## Trait
##----------------
## which  genes have been found by am+
indx <- match(snps, map$SNP)
genes_found <- map$Gene[indx]

## other genes not found
genes_not_found <- unique(map$Gene)
indx <- match(genes_found, genes_not_found)
genes_not_found <- genes_not_found[-indx]  ## removing found genes
genes_not_found <- genes_not_found[-grep("LOC", genes_not_found)]
# write two lists to file for GO enrichment analysis
write.table(data.frame(genes_found), file="/Users/geo047/AM+/ChickenResults/genes_found.txt", col.names = F, row.names = F, quote = F)

write.table(data.frame(genes_not_found), file="/Users/geo047/AM+/ChickenResults/genes_not_found.txt", col.names = F, row.names = F, quote = F)


print(genes_found)

