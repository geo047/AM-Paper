## plotting precision verse recall. 
## still have to code up ROC plot
## input data resmat.RData ftp'ed from home directory on bragg in
## /home/geo047/MWAM/SimStudy/Timing/Results/resmat$FAM.RData
## contains columns
## c("n_am", "nQTL_am",
##  "n_amGPU", "nQTL_amGPU",
##  "n_mlmm", "nQTL_mlmm",
##  "n_glmnet", "nQTL_glmnet",
##  "n_bigRR",  "nQTL_bigRR",
##  "n_rf",  "nQTL_rf", "n_lasso", "nQTL_lasso",
##  "n_piMASS", "nQTL_piMASS", "nQTL_true")

require(ggplot2)
library(GGally)
library(ggthemes)
library(gridExtra)
library(RColorBrewer)
DIR <- "~/AM+/Plots_for_Paper/"


## vector initialisation
fam <- c("W", "S",  "L","HS","A","HL")
names.of.methods <- c("am", "mlmm","glmnet","piMASS","lasso","rf","bigRR")


## list initialisation
precision <- list()
recall <- list()
h <- list()


for(ff in fam){
  precision[[ff]] <- list()
  recall[[ff]] <- list()
  h[[ff]] <- list()
  ## Load Data RData Objects
  filename <- paste(DIR,"res",ff,".RData", sep="")
  load(filename)   ## loads res_mat

  
  cat("forming Precision and REcall values \n")
  for(ii in names.of.methods){
    n_method <- paste("n_",ii, sep="")
    nQTL_method <- paste("nQTL_",ii, sep="")
    ## ---- Set PRec and Recall for AM+  -----
    precision[[ff]][[ii]] <- res_mat[, eval(nQTL_method)]/res_mat[, eval(n_method)]
    recall[[ff]][[ii]] <- res_mat[,eval(nQTL_method)] / res_mat[, "nQTL_true"]
  }  ## end for ii
  
} ## end for ff


## form results structure
## method,  fam,  rep,  precision
dfres <- data.frame()
for(ff in names(precision))
{
  for(mm in names(precision[[1]]))
  {
    df <- data.frame(method=mm, fam=ff, rep=1:length(precision[[1]][[1]]),
                     precision=precision[[ff]][[mm]],
                     recall = recall[[ff]][[mm]])
    dfres <- rbind.data.frame(dfres, df)
  }  ## end for mm
}  ## end for ff

##----------------------
## precision gives NaN when
## no QTL found. Set to 0
##-------------------------

dfres$precision[which(is.nan(dfres$precision))] <- 0


##---------------------------------------
## trying to get smoother to behave well
##---------------------------------------

indx <- with(dfres, which(precision==1 & fam=="W"))
dfres$precision[indx] <- 0.95





##-------------------------------
## smoother going outside bounds
##-------------------------------





## change family labels to simulation labels
#  W  150 x 5 K           750
#  S  350 x 400K         140,000
#  L  1500 x 50K          75,000
#  HS 2000 x 500K         1,000,000
#  A  4000 x 1.5M       600,000,000
#  HL 10000 x 1.5M      1.5*e10


c("W","S","L","HS","A","HL")
levels(dfres$fam) <-  c( "150 x 5K" , "350 x 400K",
            "1500 x 50K",
             "2000 x 500K",  
          "4000 x 1.5M",  "10000 x 1.5M")




p <- ggplot(dfres, aes(y=recall, x=as.numeric(fam), color=method)) + 
  geom_smooth(method="loess",span=0.5, se=FALSE, size=1.5, group=fam) +
  scale_color_manual(values = brewer.pal(9, "Set1"))
p


## set x axis labels
ts <-  c( "150 x 5K" ,  "350 x 400K" , "1500 x 50K",
           "2000 x 500K",  
          "4000 x 1.5M",  "10000 x 1.5M")
p <- p + scale_x_continuous(breaks=unique(as.numeric(dfres$fam)), labels=ts) 


# add theme
p <- p + theme_stata() + theme(plot.background=element_rect(fill="grey93")) 
p


#----------------------
# alter x axis labels
#----------------------
p <- p + 
  theme(axis.text.x = element_text(size=22,  angle=0, vjust=0.2)) +
  theme(axis.text.y=element_text(size=22, hjust=0.5))

## add x andy axis title
p <- p + xlab("\nscenario") + ylab("power\n")

  ##  change x and y labels size and bold
  p <- p + theme(axis.title.x = element_text(angle=0, vjust=1, size=22)) 
  p <- p + theme(axis.title.y = element_text(angle=90, vjust=1, size=22))
  p
  


  
## increase font of lengend + remove legend title
require(grid)
p <- p +  theme(legend.text=element_text(size=22))  ## font of labels
p <- p +  theme(legend.title=element_blank()) ## 
#p <- p + guides(colour = guide_legend(override.aes = list(size=3)))
p <- p + theme(legend.key.size=unit(1.5,"cm"))
p








