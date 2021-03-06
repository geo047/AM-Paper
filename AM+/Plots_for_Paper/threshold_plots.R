## plotting threshold and FDR for multi-locus mehtods and single-locus methods (fast, fastALL, gemma)
## Last revised: 21/12/2016
##
## input data resx_y.RData ftp'ed from home directory on bragg in
## /home/geo047/MWAM/SimStudy/Timing/Results/res$FAM_$indx.RData
## where $FAM is W S L A HS HL and $indx is the index 1, 2, 3, etc over the thresholds used in
## results.R on bragg 
## contains columns
## c("n_am", "nQTL_am",
##  "n_amGPU", "nQTL_amGPU",
##  "n_mlmm", "nQTL_mlmm",
##  "n_glmnet", "nQTL_glmnet",
##  "n_bigRR",  "nQTL_bigRR",
##  "n_ranger",  "nQTL_ranger", 
##  "n_lasso", "nQTL_lasso",
##   n_gemma, nQTL_gemma, 
##   n_fast, nQTL_fast, 
##   n_fastALL, nQTL_fastALL,
##  "nQTL_true")
## Note 
## moved from smoothed power curves to lines based on mean. Smoothed curves weird.
require(ggplot2)
library(GGally)
library(ggthemes)
library(gridExtra)
library(RColorBrewer)
library(extrafont)
DIR <- paste(getwd(),"/", sep="")

sizefn <- 16
thresh_indx <- 1:500


## vector initialisation
fam <- c("W", "S",  "L","HS","A","HL")
names.of.methods <- c("am", "mlmm","glmnet","lasso","r2VIM","bigRR", "gemma", "fastALL", "fast")


## list initialisation
FDR <- list()
dfres <- list()
recall <- list()

## threshold information is not actually contained in the files obtained from 
## bragg. It sits in the results.R file on /home/geo047/MWAM/SimStudy/Timing/RScripts/
pthresh  <- seq(0.01,0.99, length.out=500)
pthresh_r2VIM <- seq(0.05, 100, length.out=500)
#alpha <- 10**-seq(-log10(1e-35), -log10(1e-2), length.out=500)
alpha <- seq(-log10(1e-35), -log10(1e-2), length.out=500)



##----------------------------------------
## Forming list with FDR and recall(power)
## results over families and threshold indexes
##------------------------------------------
cat(" Forming FDR and power(recall) results ... \n")
## looping over families
for(ff in fam){
  FDR[[ff]] <- list()
  recall[[ff]] <- list()

  ## looing over threshold indexes
  for(indx in thresh_indx){
      FDR[[ff]][[indx]] <- list()
      recall[[ff]][[indx]] <- list()
      ## Load Data RData Objects
      filename <- paste(DIR,"res",ff,"_",indx, ".RData", sep="")
      load(filename)   #  # loads res_mat

  
 
    for(ii in names.of.methods){
      n_method <- paste("n_",ii, sep="")
      nQTL_method <- paste("nQTL_",ii, sep="")
      ## ---- Set Power (recall) and FDR 
      tmp <-  1 - (mat[, eval(nQTL_method)]/
                    mat[, eval(n_method)])
      tmp[is.nan(tmp)] <- 0
          FDR[[ff]][[indx]][[ii]] <- mean(tmp, na.rm=TRUE)
      # capturing case where there may not be any results 
     if(is.nan(FDR[[ff]][[indx]][[ii]]))
       FDR[[ff]][[indx]][[ii]] <- NA
          
      recall[[ff]][[indx]][[ii]] <- mean(mat[,eval(nQTL_method)]   / mat[, "nQTL_true"],
             na.rm=TRUE)
      if(is.nan(recall[[ff]][[indx]][[ii]]))
        recall[[ff]][[indx]][[ii]] <- NA
        }  ## end for ii
  
} ## end for  indx
} ## end for family



## form results structure
## method,  fam,  rep,  FDR
dfres <- data.frame()

## looping over families
for(ff in names(FDR))
{ 
   
  dftmp <- data.frame()
  cat(" Reading in family", ff, "\n")
  ## looping over threshold indexes
  for(indx in thresh_indx)
  {
    ## looping over methods
    for(mm in names(FDR[[1]][[1]])){
      #for(mm in "gemma"){
        if(mm=="glmnet"){
        df <- data.frame(method=mm, fam=ff, rep=1:length(FDR[[1]][[1]][[1]]),
                         FDR=FDR[[ff]][[indx]][[mm]] ,
                         threshold = pthresh[indx])    
        
      }

      if(mm=="LMM-Lasso"){
        df <- data.frame(method=mm, fam=ff, rep=1:length(FDR[[1]][[1]][[1]]),
                         FDR=FDR[[ff]][[indx]][[mm]] ,
                         threshold = pthresh[indx])
      }

      if(mm=="r2VIM"){
        df <- data.frame(method=mm, fam=ff, rep=1:length(FDR[[1]][[1]][[1]]),
                         FDR=FDR[[ff]][[indx]][[mm]] ,
                         threshold = pthresh_r2VIM[indx])
        
        
        
        
      }
      
      if(mm=="bigRR"){
        df <- data.frame(method=mm, fam=ff, rep=1:length(FDR[[1]][[1]][[1]]),
                         FDR=FDR[[ff]][[indx]][[mm]] ,
                         threshold = pthresh[indx])
        
      }
      
      if(mm=="gemma"){
        df <- data.frame(method=mm, fam=ff, rep=1:length(FDR[[1]][[1]][[1]]),
                         FDR=FDR[[ff]][[indx]][[mm]] ,
                         threshold = alpha[indx])
        
      }
      
      if(mm=="fast"){
        df <- data.frame(method=mm, fam=ff, rep=1:length(FDR[[1]][[1]][[1]]),
                         FDR=FDR[[ff]][[indx]][[mm]] ,
                         threshold = alpha[indx])
        
      }
        
      if(mm=="fastALL"){
        df <- data.frame(method=mm, fam=ff, rep=1:length(FDR[[1]][[1]][[1]]),
                         FDR=FDR[[ff]][[indx]][[mm]] ,
                         threshold = alpha[indx])
        
      }
        
      dftmp <- rbind.data.frame(dftmp , df)
      

  } ## end for thresh_indx  
  }  ## end for mm
  dfres <- rbind.data.frame(dfres, dftmp)
}  ## end for ff



## checking if all values are 0 for a curve
for(mm in levels(dfres$method)){
  for(ff in levels(dfres$fam)){
    indx <- which(dfres$method==mm & dfres$fam==ff) 
    if(sum(dfres$FDR[indx], na.rm=T)==0){
     print("in here")
       dfres$FDR[indx] <- NA
    }
  }
}



## change ordering of factor levels to change order of facet_wrap
dfres$method <- factor(dfres$method, levels=c("glmnet", "lasso",  "r2VIM",  "bigRR", "gemma", 
                                      "fastALL", "fast"))
levels(dfres$method) <- c("glmnet", "LMM-Lasso", "r2VIM", "bigRR", "GEMMA", "FaST-LMM^all", 
                      "FaST-LMM^few")

dfres$fam <- factor(dfres$fam, levels=c("W","L","S","HS","A","HL"))
levels(dfres$fam) <- c("150 x 5K", "350 x 500K", "1500 x 50K", "2000 x 500K", 
                       "4000 x 1.5M", "10000 x 1.5M")

## change family labels to simulation labels
#  W  150 x 5 K           750
#  S  350 x 400K         140,000
#  L  1500 x 50K          75,000
#  HS 2000 x 500K         1,000,000
#  A  4000 x 1.5M       600,000,000
#  HL 10000 x 1.5M      1.5*e10

# "am"      "mlmm"    "glmnet"  "lasso"   "r2VIM"   "bigRR"   "gemma"   "fastALL" "fast"  


 
p <- ggplot(data=dfres, aes(threshold, FDR, color=fam)) + geom_line(size=1)  +
  facet_wrap(~method, ncol=3,scales="free", labeller=label_parsed) + 
  theme(aspect.ratio = 1) # try with and without


## set theme
p <- p + theme_hc()

## increase spacing between facet plots
p <- p + theme(panel.spacing = unit(3, "lines"))

## specify xlab and ylab
p <- p  + ylab(bquote("False discovery rate")) + 
  xlab(bquote('Significance threshold'))




##  change x and y labels size and bold
p <- p + theme(axis.title.x = element_text(angle=0, vjust=1, size=14)) 
p <- p + theme(axis.title.y = element_text(angle=90, vjust=1, size=14))

# alter x and y axis labels 
p <- p + 
  theme(axis.text.x = element_text(size=11,  angle=0)) +
  theme(axis.text.y=element_text(size=11, hjust=0.5)) +
  theme(strip.text = element_text(size=14))

## increase font of lengend + remove legend title
p <- p +  theme(legend.text=element_text(size=12))
p <- p +  theme(legend.title=element_blank())
p <- p+ theme(legend.key.width=grid:::unit(1.5,"cm"))

#p + theme_base()
#p + theme_economist_white()
#p + theme_few()



postscript("~/Papers/AM-Paper/threshold.eps", width=10, height=10, fonts=c("sans", fonts()),
           horizontal=FALSE)
p
dev.off()


