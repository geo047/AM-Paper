## plotting power and FDR for multi-locus mehtods and single-locus methods (fast, fastALL, gemma)
## 
## Note: don't know why but this error would come up a lot when printing final plots. 
##
##       Error in grid.Call(L_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  : 
##       polygon edge not found
## 
##       Solution: just repeat the printing of the plot a couple of times and it will 
##                 correct itself. 
##
## input data res?.RData ftp'ed from home directory on bragg in
## /home/geo047/MWAM/SimStudy/Timing/Results/res$FAM.RData
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
## moved from smoothed power curves to lines based on median. Smoothed curves weird.
require(ggplot2)
library(GGally)
library(ggthemes)
library(gridExtra)
library(RColorBrewer)
library(extrafont)
DIR <- paste(getwd(),"/", sep="")

sizefn <- 16



## vector initialisation
fam <- c("W", "S",  "L","HS","A","HL")
names.of.methods <- c("am", "mlmm","glmnet","lasso","r2VIM","bigRR", "gemma", "fastALL", "fast")


## list initialisation
FDR <- list()
recall <- list()  ## == power
FDRp <- list()  ## FDR points
recallp <- list()  ## recall points   == power
h <- list()


for(ff in fam){
  FDR[[ff]] <- list()
  recall[[ff]] <- list()
  FDRp[[ff]] <- list()
  recallp[[ff]] <- list()
  
  ## Load Data RData Objects
  filename <- paste(DIR,"res",ff,".RData", sep="")
  load(filename)   ## loads res_mat

  
  cat("forming FDR and REcall values \n")
  for(ii in names.of.methods){
    n_method <- paste("n_",ii, sep="")
    nQTL_method <- paste("nQTL_",ii, sep="")
    ## ---- Set Power (recall) and FDR 
    tmp <-  1 - (res_mat[, eval(nQTL_method)]/
                  res_mat[, eval(n_method)])
    tmp[is.nan(tmp)] <- 0
        FDR[[ff]][[ii]] <- median(tmp, na.rm=TRUE)
    
    recall[[ff]][[ii]] <- median(res_mat[,eval(nQTL_method)]   / res_mat[, "nQTL_true"],
           na.rm=TRUE)
      
    FDRp[[ff]][[ii]] <- 1 - 
      ((res_mat[, eval(nQTL_method)])/
        res_mat[, eval(n_method)])
    
    recallp[[ff]][[ii]] <- res_mat[,eval(nQTL_method)] / res_mat[, "nQTL_true"]
    
    ## remove NaNs
  FDRp[[ff]][[ii]][is.nan(FDRp[[ff]][[ii]])] <- 0
  recallp[[ff]][[ii]][is.nan(recallp[[ff]][[ii]])] <- 0  
        }  ## end for ii
  
} ## end for ff


## form results structure
## method,  fam,  rep,  FDR
dfres <- data.frame()
dfresp  <- data.frame()
for(ff in names(FDR))
{
  for(mm in names(FDR[[1]]))
  {
    df <- data.frame(method=mm, fam=ff, rep=1:length(FDR[[1]][[1]]),
                     FDR=FDR[[ff]][[mm]],
                     recall = recall[[ff]][[mm]])
    dfres <- rbind.data.frame(dfres, df)
    
    dfp <- data.frame(method=mm, fam=ff, rep=1:length(FDRp[[1]][[1]]),
                     FDRp=FDRp[[ff]][[mm]],
                     recallp = recallp[[ff]][[mm]])
    dfresp <- rbind.data.frame(dfresp, dfp)
    
  }  ## end for mm
}  ## end for ff


levels(dfres$fam) <-  c( "150 x 5K" , "350 x 400K",
                         "1500 x 50K",
                         "2000 x 500K",  
                         "4000 x 1.5M",  "10000 x 1.5M")

levels(dfresp$fam) <-  c( "150 x 5K" , "350 x 400K",
                         "1500 x 50K",
                         "2000 x 500K",  
                         "4000 x 1.5M",  "10000 x 1.5M")

# add number of genotypes
dfres$Pop <- NA
dfres$Pop[dfres$fam=="150 x 5K"] <- 150*5000
dfres$Pop[dfres$fam=="350 x 400K"] <- 350*400000
dfres$Pop[dfres$fam=="1500 x 50K"] <- 1500*50000
dfres$Pop[dfres$fam=="2000 x 500K"] <- 2000*500000
dfres$Pop[dfres$fam=="4000 x 1.5M"] <- 4000*1500000
dfres$Pop[dfres$fam=="10000 x 1.5M"] <- 10000*1500000

dfresp$Pop <- NA
dfresp$Pop[dfresp$fam=="150 x 5K"] <- 150*5000
dfresp$Pop[dfresp$fam=="350 x 400K"] <- 350*400000
dfresp$Pop[dfresp$fam=="1500 x 50K"] <- 1500*50000
dfresp$Pop[dfresp$fam=="2000 x 500K"] <- 2000*500000
dfresp$Pop[dfresp$fam=="4000 x 1.5M"] <- 4000*1500000
dfresp$Pop[dfresp$fam=="10000 x 1.5M"] <- 10000*1500000



## gitter am+ so that you can see lines
#dfres$recall[dfres$method=="am"] <- dfres$recall[dfres$method=="am"]*1.0
#dfres$FDR[dfres$method=="am"] <- dfres$FDR[dfres$method=="am"]*1.0


pps <- sort(unique(dfres$Pop))
## data frame for line segments
gsdf <- data.frame(x1=(pps), y1=rep(0,length(pps)), 
                   x2 = (pps), y2=c(0.95,0.9,0.95,0.9,0.95,0.9))

##----------------------
## FDR gives NaN when
## no QTL found. Set to 0
##-------------------------

dfres$FDR[which(is.nan(dfres$FDR))] <- 0
dfres$recall[which(is.nan(dfres$recall))] <- 0



## change family labels to simulation labels
#  W  150 x 5 K           750
#  S  350 x 400K         140,000
#  L  1500 x 50K          75,000
#  HS 2000 x 500K         1,000,000
#  A  4000 x 1.5M       600,000,000
#  HL 10000 x 1.5M      1.5*e10




##-------------------------------
## Recall = power and FDR plots
##--------------------------------

## to plot am relative to all the other plots, need to
##  1. remove am from the dfres data frame
##  2. create a new data frame with the am results repeated 8 times and each batch of results
##     indexed by the method

dfam <- subset(dfres, method=="am")
dfam <- do.call("rbind", replicate(8, dfam, simplify = FALSE))

## remove am from dfres
dfres <- subset(dfres, method!="am")
dfres$facet <- dfres$method


## add new method factor to dfam
dfam$facet <- rep(unique(as.character(dfres$method)), each=6)
dfam$method <- "AMplus"

## change method of dfres
dfres$method <- "other method"
dfres$method <- as.factor(as.character(dfres$method))

## combine dfres and dfam into a single df
df <- rbind.data.frame(dfres, dfam)
df$method <- as.factor(as.character(df$method))
df$facet <- as.factor(as.character(df$facet))


## change ordering of factor levels to change order of facet_wrap
df$facet <- factor(df$facet, levels=c("mlmm",   "glmnet", "lasso",  "r2VIM",  "bigRR", "gemma", 
                                      "fastALL", "fast"))
levels(df$facet) <- c("MLMM", "glmnet", "LMM-Lasso", "r2VIM", "bigRR", "GEMMA", "FaST-LMM^all", 
                      "FaST-LMM^few")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        Power plots
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p<- ggplot(df, aes(Pop, recall, color=method)) + geom_line(size=2) + geom_point() +
       facet_wrap(~facet, ncol=3, labeller=label_parsed) 

p  <- p + geom_segment(aes(x=7.5e5, y=0,xend=7.5e5,yend=0.99), colour="grey", cex=0.5) +
  geom_segment(aes(x=7.5e7, y=0,xend=7.5e7,yend=0.99), colour="grey", cex=0.5) +     
  geom_segment(aes(x=1.4e8, y=0,xend=1.4e8,yend=0.99), colour="grey", cex=0.5) +
  geom_segment(aes(x=1.0e9, y=0,xend=1.0e9,yend=0.99), colour="grey", cex=0.5) +
  geom_segment(aes(x=6.0e9, y=0,xend=6.0e9,yend=0.99), colour="grey", cex=0.5) +
  geom_segment(aes(x=1.5e10,y=0,xend=1.5e10,yend=0.99), colour="grey", cex=0.5) 

## log log scale
p  <- p + scale_x_continuous(trans="log10", limits=c(500000, 1e11),
                             breaks = scales::trans_breaks("log10", function(x) 10^x),
                             labels = scales::trans_format("log10", scales::math_format(10^.x))
)+  scale_y_continuous( limits=c(0, 1), breaks = seq(0,1,0.20))

##log tick marks
p <- p +annotation_logticks(scale=TRUE, side="b")

## setting theme
p <-  p + theme_tufte()

## specify xlab and ylab
p <- p  + ylab(bquote("Power")) + 
  xlab(bquote('\nNumber of genotypes'))

##  change x and y labels size and bold
p <- p + theme(axis.title.x = element_text(angle=0, vjust=1, size=18)) 
p <- p + theme(axis.title.y = element_text(angle=90, vjust=1, size=18))

# alter x and y axis labels 
p <- p + 
  theme(axis.text.x = element_text(size=14,  angle=0)) +
  theme(axis.text.y=element_text(size=14, hjust=0.5)) +
  theme(strip.text = element_text(size=16))

## positioning of  legend in vacant plot region
p <- p + theme(legend.position = c(0.8, 0.2)) 

## increase font of lengend + remove legend title
p <- p +  theme(legend.text=element_text(size=16))
p <- p +  theme(legend.title=element_blank())
p <- p+ theme(legend.key.width=grid:::unit(1.5,"cm"))

## to add text to plots, need to create a data fram
## and use geom_text(data=XXXX)
## example:
## http://stackoverflow.com/questions/11889625/annotating-text-on-individual-facet-in-ggplot2
##ann_text <- data.frame(mpg = 15,wt = 5,lab = "Text",
##                       cyl = factor(8,levels = c("4","6","8")))
##p + geom_text(data = ann_text,label = "Text")
##
dat <- data.frame(Pop=rep(pps[1]*1.50 , 3), 
                  recall = rep(0.98, 3),
                  labs = rep("150x5K",3),
                  method=rep("AMplus",3),
                  facet=c("MLMM","glmnet","LMM-Lasso"))
p <- p + geom_text(data=dat, aes(Pop, recall, label=labs, group=NULL), color="black", angle=90, hjust=1)

dat <- data.frame(Pop=rep(pps[3] * 1.50, 3), 
                  recall = rep(0.98, 3),
                  labs = rep("350x400K",3),
                  method=rep("AMplus",3),
                  facet=c("MLMM","glmnet","LMM-Lasso"))
p <- p + geom_text(data=dat, aes(Pop, recall, label=labs, group=NULL), color="black", angle=90, hjust=1)

dat <- data.frame(Pop=rep(pps[5]*0.70, 3), 
                  recall = rep(0.98, 3),
                  labs = rep("4000x1.5M",3),
                  method=rep("AMplus",3),
                  facet=c("MLMM","glmnet","LMM-Lasso"))
p <- p + geom_text(data=dat, aes(Pop, recall, label=labs, group=NULL), color="black", angle=90, hjust=1)

dat <- data.frame(Pop=rep(pps[2] * 0.70 , 3), 
                  recall = rep(0.98, 3),
                  labs = rep("1500x50K",3),
                  method=rep("AMplus",3),
                  facet=c("MLMM","glmnet","LMM-Lasso"))
p <- p + geom_text(data=dat, aes(Pop, recall, label=labs, group=NULL), color="black", angle=90, hjust=1)

dat <- data.frame(Pop=rep(pps[4]*0.70 , 3), 
                  recall = rep(0.98, 3),
                  labs = rep("2000x500K",3),
                  method=rep("AMplus",3),
                  facet=c("MLMM","glmnet","LMM-Lasso"))
p <- p + geom_text(data=dat, aes(Pop, recall, label=labs, group=NULL), color="black", angle=90, hjust=1)

dat <- data.frame(Pop=rep(pps[6]*1.5, 3), 
                  recall = rep(0.98, 3),
                  labs = rep("10000x1.5M",3),
                  method=rep("AMplus",3),
                  facet=c("MLMM","glmnet","LMM-Lasso"))
p <- p + geom_text(data=dat, aes(Pop, recall, label=labs, group=NULL), color="black", angle=90, hjust=1)

postscript("~/Papers/AM-Paper/power.eps", width=10, height=10, fonts=c("serif", fonts()),
           horizontal=FALSE)
p
dev.off()
powerplot <- p

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##     FDR plots
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p<- ggplot(df, aes(Pop, FDR, color=method)) + geom_line(size=2) + geom_point() +
  facet_wrap(~facet, ncol=3, labeller=label_parsed) 

p  <- p + geom_segment(aes(x=7.5e5, y=0,xend=7.5e5,yend=0.99), colour="grey", cex=0.5) +
  geom_segment(aes(x=7.5e7, y=0,xend=7.5e7,yend=0.99), colour="grey", cex=0.5) +     
  geom_segment(aes(x=1.4e8, y=0,xend=1.4e8,yend=0.99), colour="grey", cex=0.5) +
  geom_segment(aes(x=1.0e9, y=0,xend=1.0e9,yend=0.99), colour="grey", cex=0.5) +
  geom_segment(aes(x=6.0e9, y=0,xend=6.0e9,yend=0.99), colour="grey", cex=0.5) +
  geom_segment(aes(x=1.5e10,y=0,xend=1.5e10,yend=0.99), colour="grey", cex=0.5) 

## log log scale
p  <- p + scale_x_continuous(trans="log10", limits=c(500000, 1e11),
                             breaks = scales::trans_breaks("log10", function(x) 10^x),
                             labels = scales::trans_format("log10", scales::math_format(10^.x))
)+  scale_y_continuous( limits=c(0, 1), breaks = seq(0,1,0.20))

##log tick marks
p <- p +annotation_logticks(scale=TRUE, side="b")

## setting theme
p <-  p + theme_tufte()

## specify xlab and ylab
p <- p  + ylab(bquote("False discovery rate")) + 
  xlab(bquote('\nNumber of genotypes'))

##  change x and y labels size and bold
p <- p + theme(axis.title.x = element_text(angle=0, vjust=1, size=18)) 
p <- p + theme(axis.title.y = element_text(angle=90, vjust=1, size=18))

# alter x and y axis labels 
p <- p + 
  theme(axis.text.x = element_text(size=14,  angle=0)) +
  theme(axis.text.y=element_text(size=14, hjust=0.5)) +
  theme(strip.text = element_text(size=16))

## positioning of  legend in vacant plot region
p <- p + theme(legend.position = c(0.8, 0.2)) 

## increase font of lengend + remove legend title
p <- p +  theme(legend.text=element_text(size=16))
p <- p +  theme(legend.title=element_blank())
p <- p+ theme(legend.key.width=grid:::unit(1.5,"cm"))

## to add text to plots, need to create a data fram
## and use geom_text(data=XXXX)
## example:
## http://stackoverflow.com/questions/11889625/annotating-text-on-individual-facet-in-ggplot2
##ann_text <- data.frame(mpg = 15,wt = 5,lab = "Text",
##                       cyl = factor(8,levels = c("4","6","8")))
##p + geom_text(data = ann_text,label = "Text")
##
dat <- data.frame(Pop=rep(pps[1]*1.50 , 3), 
                  FDR = rep(0.98, 3),
                  labs = rep("150x5K",3),
                  method=rep("AMplus",3),
                  facet=c("MLMM","glmnet","LMM-Lasso"))
p <- p + geom_text(data=dat, aes(Pop, FDR, label=labs, group=NULL), color="black", angle=90, hjust=1)

dat <- data.frame(Pop=rep(pps[3] * 1.50, 3), 
                  FDR = rep(0.98, 3),
                  labs = rep("350x400K",3),
                  method=rep("AMplus",3),
                  facet=c("MLMM","glmnet","LMM-Lasso"))
p <- p + geom_text(data=dat, aes(Pop, FDR, label=labs, group=NULL), color="black", angle=90, hjust=1)

dat <- data.frame(Pop=rep(pps[5]*0.70, 3), 
                  FDR = rep(0.98, 3),
                  labs = rep("4000x1.5M",3),
                  method=rep("AMplus",3),
                  facet=c("MLMM","glmnet","LMM-Lasso"))
p <- p + geom_text(data=dat, aes(Pop, FDR, label=labs, group=NULL), color="black", angle=90, hjust=1)

dat <- data.frame(Pop=rep(pps[2] * 0.70 , 3), 
                  FDR = rep(0.98, 3),
                  labs = rep("1500x50K",3),
                  method=rep("AMplus",3),
                  facet=c("MLMM","glmnet","LMM-Lasso"))
p <- p + geom_text(data=dat, aes(Pop, FDR, label=labs, group=NULL), color="black", angle=90, hjust=1)

dat <- data.frame(Pop=rep(pps[4]*0.70 , 3), 
                  FDR = rep(0.98, 3),
                  labs = rep("2000x500K",3),
                  method=rep("AMplus",3),
                  facet=c("MLMM","glmnet","LMM-Lasso"))
p <- p + geom_text(data=dat, aes(Pop, FDR, label=labs, group=NULL), color="black", angle=90, hjust=1)

dat <- data.frame(Pop=rep(pps[6]*1.5, 3), 
                  FDR = rep(0.98, 3),
                  labs = rep("10000x1.5M",3),
                  method=rep("AMplus",3),
                  facet=c("MLMM","glmnet","LMM-Lasso"))
p <- p + geom_text(data=dat, aes(Pop, FDR, label=labs, group=NULL), color="black", angle=90, hjust=1)

postscript("~/Papers/AM-Paper/FDR.eps", width=10, height=10, fonts=c("serif", fonts()),
           horizontal=FALSE)
p
dev.off()



