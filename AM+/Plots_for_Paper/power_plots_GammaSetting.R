## plotting power and FDR for multi-locus mehtods and single-locus methods (fast, fastALL, gemma)
## Last revised:   29/11/2016  MAjor change. Plotting Power vs FDR 
## 
## Note: don't know why but this error would come up a lot when printing final plots. 
##
##       Error in grid.Call(L_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  : 
##       polygon edge not found
## 
##       Solution: just repeat the printing of the plot a couple of times and it will 
##                 correct itself. 
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
DIR <- "/Users/geo047/Papers/AM-Paper/AM+/Plots_for_Paper/"
sizefn <- 16
##thresh_indx <- 1:500
thresh_indx <- 1:2

## vector initialisation
fam <- c("W", "S",  "L","HS","A","HL")
#fam <- NULL
#for(ii in c(700,800,900,1000))
#{
#  for(jj in c(5,50,500))
#  {
#    nme <- paste0("X",ii,"new",jj)
#    fam <- c(fam, nme)
#  }
#  
#}


names.of.methods <- c("am", "mlmm","glmnet","lasso","r2VIM","bigRR", "gemma", "fastALL", "fast")


## list initialisation

FDR <- list()
recall <- list()  ## == power
dfres <- list()

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
  cat(" Reading in family", ff, "\n")
  ## looping over threshold indexes
  for(indx in thresh_indx)
  {
    ## looping over methods
    for(mm in names(FDR[[1]][[1]])){
    df <- data.frame(method=mm, fam=ff, rep=1:length(FDR[[1]][[1]][[1]]),
                     FDR=FDR[[ff]][[indx]][[mm]] ,
                     recall = recall[[ff]][[indx]][[mm]] )
    dfres <- rbind.data.frame(dfres , df)
    

  } ## end for thresh_indx  
  }  ## end for mm
}  ## end for ff




##----------------------
## FDR gives NaN when
## no QTL found. Set to 0
##-------------------------

dfres$FDR[which(is.nan(dfres$FDR))] <- 0
dfres$recall[which(is.nan(dfres$recall))] <- 0

## change ordering of factor levels to change order of facet_wrap
dfres$method <- factor(dfres$method, levels=c("am", "mlmm",   "glmnet", "lasso",  "r2VIM",  "bigRR", "gemma", 
                                      "fastALL", "fast"))
levels(dfres$method) <- c("AMplus", "MLMM", "glmnet", "LMM-Lasso", "r2VIM", "bigRR", "GEMMA", "FaST-LMM^all", 
                      "FaST-LMM^few")

####    levels(dfres$fam) <- c("150 x 5K", "350 x 500K", "1500 x 50K", "2000 x 500K", 
#####                       "4000 x 1.5M", "10000 x 1.5M")

## change family labels to simulation labels
#  W  150 x 5 K           750
#  S  350 x 400K         140,000
#  L  1500 x 50K          75,000
#  HS 2000 x 500K         1,000,000
#  A  4000 x 1.5M       600,000,000
#  HL 10000 x 1.5M      1.5*e10

# "am"      "mlmm"    "glmnet"  "lasso"   "r2VIM"   "bigRR"   "gemma"   "fastALL" "fast"  

##------------------------------------
## Draw plot for multi-locus methods
##-------------------------------------
## dropping MLMM recall values by a little bit because they are lower and so that we can see it in the plot
dfres[which(dfres$method=="MLMM"), "recall"] <-  dfres[which(dfres$method=="MLMM"), "recall"] * 0.95


df1 <- subset(subset(dfres, !(method=="AMplus" | method=="MLMM" | method=="GEMMA" | method=="FaST-LMM^few" | method=="FaST-LMM^all" )),
  !(fam=="4000 x 1.5M" | fam=="10000 x 1.5M"))

df2 <- subset(subset(dfres, method=="AMplus" | method=="MLMM"),  !(fam=="4000 x 1.5M" | fam=="10000 x 1.5M"))



p <- ggplot(data=df1, aes(FDR, recall, color=method)) + geom_line(size=1)  +
  geom_point(data=df2, aes(FDR, recall), size=2) +
  facet_wrap(~fam, ncol=2) + 
  theme(aspect.ratio = 1) # try with and without



p <- p + scale_color_manual(
  breaks=c("AMplus","MLMM","bigRR","glmnet","LMM-Lasso","r2VIM"),
values=brewer.pal(9, "Paired")[c(1,4,3,5,2,9)], 
guide = guide_legend(override.aes = list(
  linetype = c("blank", "blank", "solid", "solid", "solid", "solid"),
  shape=c(16, 16, NA, NA, NA, NA))))

p




                  
## set theme
p <- p + theme_hc()

## increase spacing between facet plots
p <- p + theme(panel.spacing = unit(3, "lines"))

## specify xlab and ylab
p <- p  + ylab(bquote("Power")) + 
  xlab(bquote('False discovery rate'))




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



postscript("~/Papers/AM-Paper/powerMultiple.eps", width=10, height=10, fonts=c("sans", fonts()),
           horizontal=FALSE)
p
dev.off()

p1 <- p

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## power vs fdr for single-locus models
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



df1 <- subset(dfres, (method=="GEMMA" | method=="FaST-LMM^few" | method=="FaST-LMM^all" ))

df2 <- subset(dfres, method=="AMplus" | method=="MLMM")



p <- ggplot(data=df1, aes(FDR, recall, color=method)) + geom_line(size=1)  +
  geom_point(data=df2, aes(FDR, recall), size=2) +
  facet_wrap(~fam, ncol=3) + 
  theme(aspect.ratio = 1) # try with and without


p <- p + scale_color_manual(
  breaks=c("AMplus","MLMM","FaST-LMM^all","FaST-LMM^few","GEMMA"),
  labels=c("AMplus", "MLMM", bquote("FaST-LMM"^all), bquote("FaST-LMM"^few), "GEMMA"),
  values=brewer.pal(12, "Paired")[c(1,7,6,12,2)], 
  guide = guide_legend(override.aes = list(
    linetype = c("blank", "blank", "solid", "solid", "solid"),
    shape=c(16, 16, NA, NA, NA))))



## set theme
p <- p + theme_hc()

## increase spacing between facet plots
p <- p + theme(panel.spacing = unit(3, "lines"))

## specify xlab and ylab
p <- p  + ylab(bquote("Power")) + 
  xlab(bquote('False discovery rate'))


p


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




postscript("~/Papers/AM-Paper/powerSingle.eps", width=10, height=10, fonts=c("sans", fonts()),
           horizontal=FALSE)
p
dev.off()


p2 <- p
