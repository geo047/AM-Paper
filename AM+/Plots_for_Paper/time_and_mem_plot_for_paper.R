## Notes to self
# 1. this script cannot be run from source. The plots do not write to file.
# 2. this script sometimes gives plotting errors. Just keep running it with "run" until success.
# 3. the runtime plots are printed separately. They then need to be read in to powerpoint a long with a 
#     screen grab of the latex produced table.  This is then saved as a jpeg file to produce Figure 1 in paper.






## Results are obtained from bragg
## input matrices cputimes.dat and memory.dat, and abstimes.dat
## Input obtained from /home/geo047/MWAM/SimStudy/Timing/Results  on Bragg cluster. 
## See README file on Bragg for how these input files were created. 
##
## Last modified:  19/07/2016

##-----------------
##  Main Program 
##-------------------

require(ggplot2)
library(GGally)
library(ggthemes)
library(gridExtra)
library("reshape2")
library(RColorBrewer)
##----------------------
##  CPU times 
##-----------------------

## raw plots and logdf plots do not look that good. am+ is better but not by much.
## going to try am+ as benchmark and comparing everything against it. -ve values mean 
## other approach is better. 

setwd("/Users/geo047/Papers/AM-Paper/AM+/Plots_for_Paper")
df <- read.table(file="cputimes.dat", header=TRUE)

sizefn <- 12

df<- melt(df)
names(df) <- c("Pop", "method","time")

df$time[df$time<1] <- 1  ## setting times less than 1 to 1
levels(df$Pop) <- c(10000*1500000, 150*5000, 1500*50000,2000*500000, 350*400000,  4000*1500000)
df$Pop <- as.numeric(as.character(df$Pop))

pps <- sort(unique(df$Pop))
names(df) <- c("Pop", "method","time")
##--------------------
## times - times
##--------------------

## data frame for line segments
gsdf <- data.frame(x1=(pps), y1=rep(1,length(pps)), 
                   x2 = (pps), y2=c(700,500,700,500,700,500))


##-------------------
##  Main plot
##--------------------

## maybe draw vertical lines first to get around problem.

p <- ggplot(df, aes(x=Pop, y=time, group=method,shape=method, colour=method)) + 
  geom_line(size=2) + geom_point(aes(colour=method),size=4) +
  scale_shape_manual(breaks=c("am.", "mlmm","glmnet","lasso","rf_ranger","bigRR"), 
                     labels=c("Eagle", "MLMM","glmnet", "LMM-Lasso",
                              "r2VIM","bigRR"),
                     values=c(rep(19,6)))+
  scale_colour_manual(breaks=c("am.", "mlmm","glmnet","lasso","rf_ranger","bigRR"),
                      labels=c("Eagle", "MLMM","glmnet", "LMM-Lasso",
                               "r2VIM","bigRR"),
                      values=brewer.pal(10, "Paired")[c(2,1,3, 4,5, 6)]) + 
  scale_size(guide = 'none') 
p


## suppressed extra legend for size
## the reordered colors is complicated but it is based on the original colors 
## being of the order 1,2,5,3,4 (don't know why ggplot2 would do it this way) so
## I had to put color 2 in position 1, 
##                    3  ->         2,
##                    4  ->         5,
##                    5  ->         3,
##                    6  ->         4,
## and the rest dont matter because they are not used.




p  <- p +  geom_segment(aes(x=7.5e5, y=0,xend=7.5e5,yend=6000), colour="grey", cex=0.5) +
  geom_segment(aes(x=7.5e7, y=0,xend=7.5e7,yend=4000), colour="grey", cex=0.5) +     
  geom_segment(aes(x=1.4e8, y=0,xend=1.4e8,yend=6000), colour="grey", cex=0.5) +
  geom_segment(aes(x=1.0e9, y=0,xend=1.0e9,yend=4000), colour="grey", cex=0.5) +
  geom_segment(aes(x=6.0e9, y=0,xend=6.0e9,yend=6000), colour="grey", cex=0.5) +
  geom_segment(aes(x=1.5e10,y=0,xend=1.5e10,yend=4000), colour="grey", cex=0.5) 



## log log scale
p  <- p + scale_x_continuous(trans="log10", limits=c(500000, 1e11),
                             breaks = scales::trans_breaks("log10", function(x) 10^x),
                             labels = scales::trans_format("log10", scales::math_format(10^.x))
)+
  scale_y_continuous(trans="log10", limits=c(1, 10000), 
                     breaks = scales::trans_breaks("log10", function(x) 10^x),
                     labels = scales::trans_format("log10", scales::math_format(10^.x)))


##log tick marks
 p <- p +annotation_logticks(scale=TRUE, side="bl", size=2, short=unit(0.3, "cm"),mid=unit(0.6, "cm"), 
                             long=unit(0.9, "cm") )



## make more room around plot
p <- p + theme(plot.margin = grid::unit(c(1,0,4,1), "lines"))

## set theme
p <-  p + theme_classic()

## specify xlab and ylab
p <- p  + ylab(bquote("Median of elapse times (in minutes)\n")) + 
  xlab(bquote('Number of genotypes'))


##  change x and y labels size and bold
#p <- p + theme(axis.title.x = element_text(angle=0, vjust=1, size=12)) 
#p <- p + theme(axis.title.y = element_text(angle=90, vjust=1, size=12))

# alter x and y axis labels 
p <- p + 
  theme(axis.text.x = element_text(size=12,  angle=0)) +
  theme(axis.text.y=element_text(size=sizefn, hjust=0.5))

## increase font of lengend + remove legend title
p <- p +  theme(legend.text=element_text(size=sizefn))
p <- p +  theme(legend.title=element_blank())
p <- p+ theme(legend.key.width=grid:::unit(1.5,"cm"))




## add text to plots
p <- p +  annotate("text", x=pps, y=rep(9000,length(pps)), label= c("150 x 5K" , 
                                                                    "",
                                                                    "350 x 400K" , 
                                                                    "",  
                                                                    "4000 x 1.5M",  
                                                                    ""),
                   size=11)


p <- p +  annotate("text", x=pps, y=rep(5000,length(pps)), label= c("" , 
                                                                    "1500 x 50K",
                                                                    "" ,  
                                                                    "2000 x 500K", 
                                                                    "",  
                                                                    "10000 x 1.5M"),
                   size=11)





p <- p + theme(legend.position = c(0.82, 0.8), 
               legend.justification = c(0, 1))

# remove x axis labels
 #p <- p + theme(axis.text.x=element_blank(),axis.title.x=element_blank())
 
 ## putting some space around the plot
 p <- p + theme(plot.margin = grid::unit(c(1,5,2,1), "lines"))
 

## increase font size of labels
 p <- p +theme(axis.title.y=element_text(size=38),
          axis.title.x=element_text(size=38))
 
## increase width of axis lines
p <-  p + theme(axis.line.x = element_line(size = 1.5),
           axis.line.y = element_line(size = 1.5))
 

## increase font of x and y tick labels
p <-  p + theme(axis.text.x = element_text(size=38),
                axis.text.y = element_text(size=38))


## increase spacing between items in legend
p <- p  + theme(legend.key.height=unit(4,"line"), legend.text = element_text(size=34))


## increase font legened
# p <- p + theme(legend.text = element_text(size=22))
 
 
 
p_time <- p

p_time

## Export figure with specs : 1500 x 1000




  
  
##----------------------------------------------------------
## Absolute times for fast, fastall, gemma, am+

##----------------------------------------------------------
  
  df <- read.table(file="abscputimes.dat", header=TRUE)
 # remove column for am.GPU
 indx <- which(colnames(df)=="am.GPU")
 df <- df[, -indx]
  sizefn <- 12
  indx <- which(df[,2:ncol(df)]<1, arr.ind=TRUE) 
  # add on first col
  indx[,"col"] <- indx[, "col"] + 1
  df[indx] <- 1
  
  
  
  #df[df<1] <- 1  ## so that it will plot on the log scale
  
  df<- melt(df)
  
  levels(df$Pop) <- c(10000*1500000, 150*5000, 1500*50000,2000*500000, 350*400000,  4000*1500000)
  df$Pop <- as.numeric(as.character(df$Pop))
  

  pps <- sort(unique(df$Pop))
  names(df) <- c("Pop", "method","time")
  ##--------------------
  
  
  p <- ggplot(df, aes(x=Pop, y=time, group=method,shape=method, colour=method)) +
    geom_line(size=2) + geom_point(aes(colour=method),size=5) +
    scale_shape_manual(breaks=c("am.", "gemma", "fastALL", "fast"), 
                                labels=c("Eagle",
                                "GEMMA",
                                bquote('FaST-LMM'^all),
                                bquote('FaST-LMM'^few)),
                       values=c(rep(19,1),rep(17,3)))+
    scale_colour_manual(breaks=c("am.", "gemma", "fastALL", "fast"),
      labels=c("Eagle",
                                 "GEMMA",
                                 bquote('FaST-LMM'^all),
                                 bquote('FaST-LMM'^few)),
                        values=brewer.pal(10, "Paired")[c(2,7,9,10)]) + 
    scale_size(guide = 'none') ## suppressed extra legend for size
  
  
  p  <- p +  geom_segment(aes(x=7.5e5, y=0,xend=7.5e5,yend=6000), colour="grey", cex=0.5) +
    geom_segment(aes(x=7.5e7, y=0,xend=7.5e7,yend=4000), colour="grey", cex=0.5) +     
    geom_segment(aes(x=1.4e8, y=0,xend=1.4e8,yend=6000), colour="grey", cex=0.5) +
    geom_segment(aes(x=1.0e9, y=0,xend=1.0e9,yend=4000), colour="grey", cex=0.5) +
    geom_segment(aes(x=6.0e9, y=0,xend=6.0e9,yend=6000), colour="grey", cex=0.5) +
    geom_segment(aes(x=1.5e10,y=0,xend=1.5e10,yend=4000), colour="grey", cex=0.5) 
  
  
  
  ## log log scale
  p  <- p + scale_x_continuous(trans="log10", limits=c(500000, 1e11),
                               breaks = scales::trans_breaks("log10", function(x) 10^x),
                               labels = scales::trans_format("log10", scales::math_format(10^.x))
  )+
    scale_y_continuous(trans="log10", limits=c(1, 10000), 
                       breaks = scales::trans_breaks("log10", function(x) 10^x),
                       labels = scales::trans_format("log10", scales::math_format(10^.x)))
  
  
  ##log tick marks
  
  p <- p +annotation_logticks(scale=TRUE, side="bl", size=2, short=unit(0.3, "cm"),mid=unit(0.6, "cm"), 
                              long=unit(0.9, "cm") )
  
  
  
  ## make more room around plot
  p <- p + theme(plot.margin = grid::unit(c(1,0,4,1), "lines"))
  
  ## set theme
  p <-  p + theme_classic()
  
  ## specify xlab and ylab
  p <- p  + ylab(bquote("Median of elapse times (in minutes)\n")) + 
    xlab(bquote('Number of genotypes'))
  
  
  ##  change x and y labels size and bold
  #p <- p + theme(axis.title.x = element_text(angle=0, vjust=1, size=12)) 
  #p <- p + theme(axis.title.y = element_text(angle=90, vjust=1, size=12))
  
  # alter x and y axis labels 
  p <- p + 
    theme(axis.text.x = element_text(size=12,  angle=0)) +
    theme(axis.text.y=element_text(size=sizefn, hjust=0.5))
  
  ## increase font of lengend + remove legend title
  p <- p +  theme(legend.text=element_text(size=sizefn))
  p <- p +  theme(legend.title=element_blank())
  p <- p+ theme(legend.key.width=grid:::unit(1.5,"cm"))
  
  
  
  
  ## add text to plots
  p <- p +  annotate("text", x=pps, y=rep(9000,length(pps)), label= c("150 x 5K" , 
                                                                      "",
                                                                      "350 x 400K" , 
                                                                      "",  
                                                                      "4000 x 1.5M",  
                                                                      ""),
                     size=11)
  
  
  p <- p +  annotate("text", x=pps, y=rep(5000,length(pps)), label= c("" , 
                                                                      "1500 x 50K",
                                                                      "" ,  
                                                                      "2000 x 500K", 
                                                                      "",  
                                                                      "10000 x 1.5M"),
                     size=11)
  
  
  
  
  
  p <- p + theme(legend.position = c(0.82, 0.8), 
                 legend.justification = c(0, 1))
  
  # remove x axis labels
  #p <- p + theme(axis.text.x=element_blank(),axis.title.x=element_blank())
  
  ## putting some space around the plot
  p <- p + theme(plot.margin = grid::unit(c(1,5,2,1), "lines"))
  
  
  ## increase font size of labels
  p <- p +theme(axis.title.y=element_text(size=38),
                axis.title.x=element_text(size=38))
  
  ## increase width of axis lines
  p <-  p + theme(axis.line.x = element_line(size = 1.5),
                  axis.line.y = element_line(size = 1.5))
  
  
  ## increase font of x and y tick labels
  p <-  p + theme(axis.text.x = element_text(size=38),
                  axis.text.y = element_text(size=38))
  
  
  ## increase spacing between items in legend
  p <- p  + theme(legend.key.height=unit(4,"line"), legend.text = element_text(size=34))
  
  
  ## increase font legened
  # p <- p + theme(legend.text = element_text(size=22))
  
  
  
  
  
  p_abstime <- p
  
  p_abstime

  ## Export figure : JPEG   dims 1700 x `1000`
  # copy into powerpoint for figure for paper.
  
  
stop()
  
  
##-----------------------------
## Multiple plots on single page
##------------------------------
  
  library(grid)
  
  postscript("~/Papers/AM-Paper/time1.eps", width=10, height=10, 
             horizontal=FALSE)
  # square plot
  p_time + coord_fixed() 
  dev.off()
  
  postscript("~/Papers/AM-Paper/time2.eps", width=10, height=10, 
             horizontal=FALSE)
  # square plot
  p_abstime + coord_fixed() 
  dev.off()
  
  
#  postscript("~/Papers/AM-Paper/time.eps", width=10, height=10, 
#             horizontal=FALSE)
# grid.newpage()
#  grid.draw(rbind(ggplotGrob(p_time), ggplotGrob(p_abstime), size = "last"))
#  
#dev.off()
#  stop()
  
  

  
  
  
##-----------------------------
## Memory plot
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##==============================  
  
  setwd("/Users/geo047/Papers/AM-Paper/AM+/Plots_for_Paper")
  df <- read.table(file="memory.dat", header=TRUE)
  sizefn <- 14
  
  df<- melt(df)
  df <- df[, -2]
  
  names(df) <- c("Pop", "method","mem")
  indx <- which(df$mem > 120)
  df[indx, "mem"] <- 119
  
  #levels(df$Pop)
  # "A"  "HL" "HS" "L"  "S"  "W" 
  # 
  levels(df$Pop) <- c(4000*1500000, 10000*1500000, 2000*500000,350*400000, 1500*50000,150*5000  )
  
  df$Pop <- as.numeric(as.character(df$Pop))
  
  pps <- sort(unique(df$Pop))
  
  ##--------------------
  ## Memory
  ##--------------------
  
  ## data frame for line segments
  gsdf <- data.frame(x1=(pps), y1=rep(1,length(pps)), 
                     x2 = (pps), y2=c(700,500,700,500,700,500))
  
  
  
  p <- ggplot(df, aes(x=Pop, y=mem, group=method, colour=method)) + 
    geom_line(size=2) + geom_point(aes(colour=method),size=5) +
   # scale_shape_manual(values=1:nlevels(df$method)) +
    scale_colour_manual(breaks=c("am.", "bigRR", "glmnet", "lasso", "mlmm", "r2VIM", "gemma", "fastALL", "fast"),
                       labels=c("Eagle","bigRR", "glmnet", "LMM-Lasso", "MLMM", "r2VIM", "GEMMA", bquote("FaST-LMM"^all), 
                                bquote("FaST-LMM"^few)),
                       values=brewer.pal(9, "Paired")) +
    
    scale_shape_manual(breaks=c("am.", "bigRR", "glmnet", "lasso", "mlmm", "r2VIM", "gemma", "fastALL", "fast"),
                       labels=c("AMplus","bigRR", "glmnet", "LMM-Lasso", "MLMM", "r2VIM", "GEMMA", bquote("FaST-LMM"^all), 
                                bquote("FaST-LMM"^few)),
                       values = c(19,19,19,19,19,17,17,17,19)) +

    scale_size(guide = 'none')  + scale_y_continuous(limits = c(0, 130))
  
  
  p  <- p +  geom_segment(aes(x=7.5e5, y=0,xend=7.5e5,yend=127), colour="grey", cex=0.5) +
    geom_segment(aes(x=7.5e7, y=0,xend=7.5e7,yend=120), colour="grey", cex=0.5) +     
    geom_segment(aes(x=1.4e8, y=0,xend=1.4e8,yend=127), colour="grey", cex=0.5) +
    geom_segment(aes(x=1.0e9, y=0,xend=1.0e9,yend=120), colour="grey", cex=0.5) +
    geom_segment(aes(x=6.0e9, y=0,xend=6.0e9,yend=127), colour="grey", cex=0.5) +
    geom_segment(aes(x=1.5e10,y=0,xend=1.5e10,yend=120), colour="grey", cex=0.5) 
  
  
  ## log log scale
  p  <- p + scale_x_continuous(trans="log10", limits=c(500000, 1e11),
                               breaks = scales::trans_breaks("log10", function(x) 10^x),
                               labels = scales::trans_format("log10", scales::math_format(10^.x))
  )
  ##log tick marks
  p <- p +annotation_logticks(scale=TRUE, side="b")
  
  
  
  ## make more room around plot
  p <- p + theme(plot.margin = grid::unit(c(1,0,4,1), "lines"))
  
  ## set theme
  p <-  p + theme_classic()
  
  ## specify xlab and ylab
  p <- p  + ylab(bquote("Memory usage (in Gbytes)")) + 
    xlab(bquote('\nNumber of genotypes'))
  
  
  ##  change x and y labels size and bold
  p <- p + theme(axis.title.x = element_text(angle=0, vjust=1, size=16)) 
  p <- p + theme(axis.title.y = element_text(angle=90, vjust=1, size=16))
  
  # alter x and y axis labels 
  p <- p + 
    theme(axis.text.x = element_text(size=14,  angle=0)) +
    theme(axis.text.y=element_text(size=sizefn, hjust=0.5))
  
  ## increase font of lengend + remove legend title
  p <- p +  theme(legend.text=element_text(size=sizefn))
  p <- p +  theme(legend.title=element_blank())
  p <- p+ theme(legend.key.width=grid:::unit(1.5,"cm"))
  
  
  
  ## add text to plots
  p <- p +  annotate("text", x=pps, y=rep(129,length(pps)), label= c("150 x 5K" , 
                                                                      "",
                                                                      "350 x 400K" , 
                                                                      "",  
                                                                      "4000 x 1.5M",  
                                                                      ""),
                     size=5)
  
  
  p <- p +  annotate("text", x=pps, y=rep(123,length(pps)), label= c("" , 
                                                                     "1500 x 50K",
                                                                     "" ,  
                                                                     "2000 x 500K", 
                                                                     "",  
                                                                     "10000 x 1.5M"),
                     size=5)
  p <- p + theme(legend.position = c(0.84, 0.6), 
                 legend.justification = c(0, 1))
  
  # remove x axis labels
  #p <- p + theme(axis.text.x=element_blank(),axis.title.x=element_blank())
  
  ## putting some space around the plot
  p <- p + theme(plot.margin = grid::unit(c(1,5,1,1), "lines"))
  
  ## square plot
  p <- p + theme(aspect.ratio=1)
  
  
  p_mem <- p
  p_mem
  
  
  postscript("~/Papers/AM-Paper/mem.eps", width=10, height=10, 
             horizontal=FALSE)
  p_mem
  dev.off()
  
  
  
  
  
