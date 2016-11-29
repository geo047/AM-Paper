# trait read in from command line 
library(MWAM)
library(rcppMagmaSYEVD)


## function to accept command line arguments
args <- commandArgs()
#if (length(args)!=11) {cat("error: not enough input variables") break}
for (e in args) {
  ta = strsplit(e,"=",fixed=TRUE)
  if(! is.na(ta[[1]][2])) {
    temp = ta[[1]][2]
    if(substr(ta[[1]][1],nchar(ta[[1]][1]),nchar(ta[[1]][1])) == "I") {
      temp = as.integer(temp)
    }
    if(substr(ta[[1]][1],nchar(ta[[1]][1]),nchar(ta[[1]][1])) == "N") {
      temp = as.numeric(temp)
    }
    assign(substr(ta[[1]][1],2,nchar(ta[[1]][1])),temp)
    cat("assigned ",ta[[1]][1]," the value of |",temp,"|\n")
  } else {
    assign(substr(ta[[1]][1],2,nchar(ta[[1]][1])),TRUE)
    cat("assigned ",ta[[1]][1]," the value of TRUE\n")
  }
}



## read genotypes
DIR <- "./"    ## could use $MEMDIR but need to get this. 
geno <- read.genotypes(path=DIR,
                      bin_path=DIR , AA=0,  AB=1, BB=2,
               file_genotype="geno.txt",
                availmemGb=120)
## read map
map <- read.map(path = DIR , file_map = "map.txt", csv = FALSE)

## phenotypes 
pheno <- read.phenotypes(path = DIR , file_phenotype = "pheno.txt" ,
                         header = TRUE, csv = FALSE )

# remove any birds with NA's
pheno$CG <- as.factor(pheno$CG)
pheno[pheno==-9999] <- NA

## Perform AM+ analysis
## set args for multiple_locus_am function
argu <- list(numcores = 16, availmemGb = 120 , colname.trait = trait, 
          map = map , pheno = pheno, geno = geno,
            colname.feffects=c("CG"), 
           gpu=TRUE,
          alpha = 0.05, error_checking = FALSE , maxit = 20, verbose=TRUE)

## run AM+
am_res <- do.call( multiple_locus_am, argu)

save(am_res, "res.RData")

