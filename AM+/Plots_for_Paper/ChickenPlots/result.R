## forming results matrix for am+GPU by hand from
## /flush1/geo047/RealDataAnalysis/ChickenAnalysis/AM_GSCali
##
## Doing annotated manhattan plots (annotation are results of the other programs)


chr = "CHR"
bp = "BP"
p = "P"
snp = "SNP"
col = c("gray10", "gray60")
chrlabs = NULL
suggestiveline = -log10(0.00001) 
genomewideline = -log10(0.00000005)
highlight = NULL
logp = TRUE
trait="Ultra"







manhattan_mine <- function (x, chr = "CHR", bp = "BP", p = "P", snp = "SNP", col = c("gray10", 
                                                                   "gray60"), chrlabs = NULL, suggestiveline = -log10(0.00001), 
          genomewideline = -log10(0.00000005), highlight = NULL, logp = TRUE, cindx=NULL, trait="Ultra") 
{
  CHR = BP = P = index = NULL
  if (!(chr %in% names(x))) 
    stop(paste("Column", chr, "not found!"))
  if (!(bp %in% names(x))) 
    stop(paste("Column", bp, "not found!"))
  if (!(p %in% names(x))) 
    stop(paste("Column", p, "not found!"))
  if (!(snp %in% names(x))) 
    warning(paste("No SNP column found. OK unless you're trying to highlight."))
  if (!is.numeric(x[[chr]])) 
    stop(paste(chr, "column should be numeric. Do you have 'X', 'Y', 'MT', etc? If so change to numbers and try again."))
  if (!is.numeric(x[[bp]])) 
    stop(paste(bp, "column should be numeric."))
  if (!is.numeric(x[[p]])) 
    stop(paste(p, "column should be numeric."))
  d = data.frame(CHR = x[[chr]], BP = x[[bp]], P = x[[p]])
  if (!is.null(x[[snp]])) 
    d = transform(d, SNP = x[[snp]])
  d <- subset(d, (is.numeric(CHR) & is.numeric(BP) & is.numeric(P)))
  d <- d[order(d$CHR, d$BP), ]
  if (logp) {
    d$logp <- -log10(d$P)
  }  else {
    d$logp <- d$P
  }
  d$pos = NA
  d$index = NA
  ind = 0
  for (i in unique(d$CHR)) {
    ind = ind + 1
    d[d$CHR == i, ]$index = ind
  }
  nchr = length(unique(d$CHR))
  if (nchr == 1) {
    options(scipen = 999)
    d$pos = d$BP/1000000
    ticks = floor(length(d$pos))/2 + 1
    xlabel = paste("Chromosome", unique(d$CHR), "position(Mb)")
    labs = ticks
  }  else {
    lastbase = 0
    ticks = NULL
    for (i in unique(d$index)) {
      if (i == 1) {
        d[d$index == i, ]$pos = d[d$index == i, ]$BP
      }
      else {
        lastbase = lastbase + tail(subset(d, index == 
                                            i - 1)$BP, 1)
        d[d$index == i, ]$pos = d[d$index == i, ]$BP + 
          lastbase
      }
      ticks = c(ticks, (min(d[d$CHR == i, ]$pos) + max(d[d$CHR == 
                                                           i, ]$pos))/2 + 1)
    }
    xlabel = "Chromosome"
    labs <- unique(d$CHR)
  }
  xmax = ceiling(max(d$pos) * 1.03)
  xmin = floor(max(d$pos) * -0.03)
  def_args <- list(xaxt = "n", bty = "n", xaxs = "i", yaxs = "i", 
                   las = 1, pch = 20, xlim = c(xmin, xmax), ylim = c(0, 
                                                                     ceiling(max(d$logp))), xlab = xlabel, ylab = expression(-log[10](italic(p))))
  dotargs <- list()
  do.call("plot", c(NA, dotargs, def_args[!names(def_args) %in% 
                                            names(dotargs)]))
  if (!is.null(chrlabs)) {
    if (is.character(chrlabs)) {
      if (length(chrlabs) == length(labs)) {
        labs <- chrlabs
      }
      else {
        warning("You're trying to specify chromosome labels but the number of labels != number of chromosomes.")
      }
    } else {
      warning("If you're trying to specify chromosome labels, chrlabs must be a character vector")
    }
  }
  if (nchr == 1) {
    axis(1)
  } else {
    axis(1, at = ticks, labels = labs)
  }
  col = rep(col, max(d$CHR))
  if (nchr == 1) {
    with(d, points(pos, logp, pch = 20, col = col[1]))
  } else {
    icol = 1
    for (i in unique(d$index)) {
      with(d[d$index == unique(d$index)[i], ], points(pos, cex=2,
                                                      logp, col = col[icol], pch = 20))
      icol = icol + 1
    }
  }
  if (suggestiveline) 
    abline(h = suggestiveline, col = "blue")
  if (genomewideline) 
    abline(h = genomewideline, col = "red")
  
  m <- unique(res$method)
  
  for(ii in m)
  {
    snps <- res$SNP[res$method==ii & res$trait==trait]
      
    if (any(!(snps %in% d$SNP))) 
      warning("You're trying to highlight SNPs that don't exist in your results.")
    d.highlight = d[which(d$SNP %in% snps), ]
    if(ii=="am") {cex.size = 2; colm="red"}
    if(ii=="mlmm")  {cex.size = 3; colm="blue"}
    if(ii=="glmnet") {cex.size = 5; colm="green"}
    if(ii == "rf")  cex.size = 6
    if(ii == "bigRR")  cex.size = 7 
    
    
      with(d.highlight, points(pos, logp, pch = 1, cex=cex.size, col=colm,lwd=10))

} ## end for ii in m
    }  ## end function
 




res <- matrix(data=c(
"Ultra",  "Gga_rs14707919", "am",
"Ultra",   "GGaluGA204479"    , "am",
"Ultra",  "Gga_rs16469122"    , "am",
"Ultra",   "GGaluGA300550"    ,"am",
"Ultra",   "GGaluGA108350"    ,  "am",
"Ultra",  "Gga_rs13595487"    ,"am",
"Ultra",   "GGaluGA283510"    ,  "am",
"FCR",   "GGaluGA163800"      , "am",
"FCR",    "GGaluGA262999"     ,    "am", 
"WT1",   "Gga_rs14707919"     ,  "am",
"WT1",    "GGaluGA200018"     ,   "am",
"WT1",   "Gga_rs16549985"     ,"am",
"WT1",    "GGaluGA332912"     ,"am",
"WT1",    "GGaluGA163907"     ,"am",
"WT1",    "GGaluGA200017"     ,"am",
"FI",   "Gga_rs14707919"      ,"am",
"FI",   "Gga_rs15792386"      ,"am",
"FI",   "Gga_rs14239370"      , "am",
"FI",    "GGaluGA108750"      ,"am",
"FI",    "GGaluGA000581"      ,   "am",
"FI",    "GGaluGA308461"      ,   "am",
"FI",   "Gga_rs14657440"      ,   "am",
"GAIN",   "Gga_rs14707919"    ,   "am",
"GAIN",   "Gga_rs15021603"    ,   "am",
"GAIN",   "Gga_rs13763055"    ,   "am",
"GAIN",   "Gga_rs14657353"    ,   "am",
"GAIN",    "GGaluGA000581"    ,   "am",
"RFI",    "Gga_rs15792386"    ,   "am",
"RFI",    "GGaluGA092050"     ,   "am",
"RFI",    "GGaluGA308461"     ,   "am",
"RFI",    "GGaluGA176788"     ,   "am",
"RFI",    "GGaluGA126664"     ,   "am",
"RFI",   "Gga_rs13999493"     ,   "am",

"Ultra", "Gga_rs14707919",  "mlmm",
"Ultra",  "GGaluGA204479", "mlmm",
"Ultra", "Gga_rs16469122", "mlmm",
"Ultra",  "GGaluGA300550",  "mlmm",
"Ultra",  "GGaluGA108350", "mlmm",
"Ultra", "Gga_rs13595487", "mlmm",
"Ultra",  "GGaluGA283510", "mlmm",
"FCR", "GGaluGA163800", "mlmm",
"FCR", "GGaluGA262999", "mlmm",
"WT1", "Gga_rs14707919", "mlmm",
"WT1",  "GGaluGA200018", "mlmm",
"WT1", "Gga_rs16549985", "mlmm",
"WT1",  "GGaluGA332912", "mlmm",
"WT1",  "GGaluGA163907", "mlmm",
"WT1",  "GGaluGA200017", "mlmm",

"FI", "Gga_rs14707919", "mlmm",
"FI", "Gga_rs15792444", "mlmm",
"FI", "Gga_rs14239370", "mlmm",
"FI",  "GGaluGA108750", "mlmm",
"FI",  "GGaluGA000581", "mlmm",
"FI",  "GGaluGA308461", "mlmm",
"FI", "Gga_rs14657440", "mlmm",

"RFI", "Gga_rs15792386", "mlmm",
"RFI",  "GGaluGA092050", "mlmm",
"RFI",  "GGaluGA308461", "mlmm",
"RFI",  "GGaluGA176788", "mlmm",
"RFI",  "GGaluGA126664", "mlmm",
"RFI", "Gga_rs13999493", "mlmm",

"GAIN", "Gga_rs14707919", "mlmm",
"GAIN", "Gga_rs15021603", "mlmm",
"GAIN", "Gga_rs13763055", "mlmm",
"GAIN", "Gga_rs14657353", "mlmm",
"GAIN",  "GGaluGA000581", "mlmm",


# "Ultra", "Gga_rs14590013", "glmnet",   ## this looks wrong

"GAIN", "Gga_rs14707919", "glmnet",

"WT1", "Gga_rs14707919", "glmnet"



),
ncol=3, byrow=TRUE)
res <- as.data.frame(res)
names(res) <- c("trait", "SNP", "method")
res$trait <- with(res, as.character(trait))
res$SNP <- with(res, as.character(SNP))
res$method <- with(res, as.character(method))



##-------------------------------
##  Read map
##--------------------------------

filename <- "~/AM+/Plots_for_Paper/ChickenPlots/map.txt"
map <- read.table(filename, header=TRUE)  ## SNP Chr Pos
names(map) <- c("SNP","CHR","BP")

##-----------------------------
## Read p values for traits 
##---------------------------


pres <- list()
trait <- c("Ultra", "FCR", "WT1", "FI", "GAIN", "RFI")
DIR <- "~/AM+/Plots_for_Paper/ChickenPlots/"
for(ii in trait){
  filename <- paste(DIR,"pfinal_",ii,".RData", sep="")  
  load(filename)   ## loads p vector
  pres[[ii]] <- p
}


##----------------------
## Plots
##----------------------
library(qqman)

plotman <- function(tt="Ultra")
{
  ## create data frame for manhattan plots  
  df <- map
  df$P <- pres[[tt]]
  # remove na values
  indx <- with(df, which(is.na(P)))
  if(length(indx)>0)
    df <- df[-indx,]
  df$CHR <- with(df, as.factor(CHR))
  levels(df$CHR) <- with(df, 1:length(levels(CHR)))
  df$CHR <- with(df, as.numeric(as.character(CHR)))

  ## checking for p values of 0 which cause log issues
  if(with(df, min(P)==0)){
    indx <- with(df, which(P==min(P)))
    df$P[indx] <- 10**-14
  }

  
  manhattan_mine(df, suggestiveline = FALSE, genomewideline = FALSE, trait=tt)
  
  
  }



## lasso analyses could not be performed - exceeded memory limit

plotman(tt="RFI")

plotman(tt="Ultra")

plotman(tt="FCR")


stop()







plotman(tt="FCR", highlightpoints=FALSE)
plotman(tt="FCR", highlightpoints=TRUE)

plotman(tt="RFI", highlightpoints=FALSE)
plotman(tt="RFI", highlightpoints=TRUE)

plotman(tt="WT1", highlightpoints=FALSE)
plotman(tt="WT1", highlightpoints=TRUE)

plotman(tt="FI", highlightpoints=FALSE)
plotman(tt="FI", highlightpoints=TRUE)


plotman(tt="GAIN", highlightpoints=FALSE)
plotman(tt="GAIN", highlightpoints=TRUE)




