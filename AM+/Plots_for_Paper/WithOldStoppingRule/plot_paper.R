## Results are obtained from bragg
## input matrices cputimes.dat and memory.dat
## Input obtained from /home/geo047/MWAM/SimStudy/Timing/Results  on Bragg cluster. 
## See README file on Bragg for how these input files were created. 
##
## amGPU just mention these results in the paper. Doesn't make sense on the figure. Doesn't add anything. 
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

setwd("/Users/geo047/AM+/Plots_for_Paper")
df <- read.table(file="cputimes.dat", header=TRUE)
sizefn <- 16

## times - ratios

mlmm <- with(df, mlmm)
piMASS <- with(df, piMASS)
glmnet <- with(df, glmnet)
rf <- with(df, rf)
bigRR <- with(df, bigRR)
lasso <- with(df, lasso)

dfnew <- data.frame("mlmm"=mlmm, "piMASS"=piMASS, "glmnet"=glmnet, "rf"=rf,  
                    "bigRR"=bigRR, "lasso"=lasso, check.names=FALSE)

dfnew$Pop <- df$Pop
#dfnew$Pop <- factor(dfnew$Pop,levels(dfnew$Pop)[c(2,3,5,4,6,1)])
dfnew$Pop <- factor(dfnew$Pop,levels(dfnew$Pop)[c(6,4,5,3,1,2)])
dfnew$Pop <- log10(c(150*5, 1500*50, 350*400, 2000*500, 4000*1500, 10000*1500))
dfnew$size <- 2

##-------------------
##  Main plot
##--------------------
p1 <- ggplot(dfnew, aes(Pop, group=1, size=size)) + 
  geom_line(aes(y = mlmm, colour = "mlmm")) +
  geom_line(aes(y = piMASS, colour = "piMASS")) + 
  geom_line(aes(y = lasso, colour = "lmm-lasso")) +
  geom_line(aes(y = glmnet, colour = "glmnet")) +
  geom_line(aes(y = rf, colour = "randfor")) + 
  geom_line(aes(y = bigRR, colour = "bigRR"))  +
  scale_color_manual(values = brewer.pal(6, "Set1"))


##------------------
## Labels for X axis
##------------------
ts <-  c( "150 x 5K\n2.8" , "1500 x 50K\n4.9",
          "350 x 400K\n5.1" ,  "2000 x 500K\n6.0",  
          "4000 x 1.5M\n6.8",  "10000 x 1.5M\n7.2")

p1 <- p1 + scale_x_continuous(breaks=dfnew$Pop, labels=ts, limits=c(2.5,7.5)) 
p1 <- p1 + scale_size(range=c(2, 2), guide=FALSE)


##-----------------
## Set theme
##------------------
p1 <- p1 + theme_stata() + theme(plot.background=element_rect(fill="grey93")) 




#----------------------
# alter x axis labels
#----------------------
p1 <- p1 + 
  theme(axis.text.x = element_text(size=12,  angle=60, vjust=c(0.5,0.5,0.5,0.5,0.5,0.5))) +
  theme(axis.text.y=element_text(size=sizefn, hjust=0.5))




p1 <- p1  + ylab(bquote("median ratio of elapse times (comparison to am+)")) + 
  xlab(bquote('simulation scenario and its ' ~log[10]~ 'number of genotypes'))


## set y limits
p1 <- p1 + ylim(0,180)
       

## add title
p1 <- p1 + ggtitle("Computing Time")
p1 <- p1 + theme(plot.title = element_text(size=20, face="bold", vjust=2))

#  increase tick marker lenght (x and y)
p1 <- p1 + theme(axis.ticks.length=grid::unit(0.65,"cm"))

##  change x and y labels size and bold
p1 <- p1 + theme(axis.title.x = element_text(angle=0, vjust=1, size=sizefn)) 
p1 <- p1 + theme(axis.title.y = element_text(angle=90, vjust=1, size=sizefn))


# change size of x and y tick labels
#p1 <- p1 + 
#  theme(axis.text.x = element_text(size=12, angle=30)) +
#  theme(axis.text.y=element_text(size=sizefn, hjust=0.5))
p1
# change size of y label
p1 <- p1 + theme(axis.title.y=element_text(angle=90, size=sizefn))





## increase font of lengend + remove legend title
p1 <- p1 +  theme(legend.text=element_text(size=sizefn))
p1 <- p1 +  theme(legend.title=element_blank())
p1 <- p1 + guides(colour = guide_legend(override.aes = list(size=3)))

## add annotations to plot
p1 <- p1 + annotate("text", x=6.0, y=dfnew[["lasso"]][4]-2, label="M") +
           annotate("text", x=6.0+0.1, y=dfnew[["bigRR"]][4], label="C") + 
           annotate("text", x= 6.0+0.1, y=dfnew[["rf"]][4], label="M") +
          annotate("text", x=6.8, y=dfnew[["mlmm"]][5], label="M") +
          annotate("text", x=6,0, y=df[["glmnet"]][4], label="M")


## adding a table of results to the plot
mytable <- data.frame(c1=0.19, c2=1.8, c3=1.4, c4=33.7, c5=131.3, c6=739.8)
names(mytable) <- c("150 x 5K", "1500 x 50K", "350 x 400K", "2000 x 500K" ,"4000 x 1.5M", 
                    "10000 x 1.5M")
rownames(mytable) <- "median elapse \ntime of am+ (mins)"


p1 <- p1 + 
     annotation_custom(tableGrob(mytable), xmin=2.5, xmax=7.5, ymin=160, ymax=180)

p1







#######---------------------------------------
#####  Memory plot for suplementary materials
#######-------------------------------------



setwd("/Users/geo047/AM+/Plots_for_Paper")
df <- read.table(file="memory.dat", header=TRUE, check.names=FALSE)
sizefn <- 16
names(df)[2] <- "am"

df$Pop <- factor(df$Pop,levels(df$Pop)[c(6,4,5,3,1,2)])
numgenos <- c(150*5, 1500*50, 350*400, 2000*500, 4000*1500, 10000*1500)
df$Pop <- log10(numgenos)
df$size <- 2

df[df>120] <- 119

## cannot see mlmm memory usage because it is same as am+ just increased by 5% to see
df$mlmm <- df$mlmm * 1.06

p1 <- ggplot(df, aes(Pop, group=1, size=size)) + 
  geom_line(aes(y = mlmm, colour = "mlmm")) +
  geom_line(aes(y = piMASS, colour = "piMASS")) + 
  geom_line(aes(y = lasso, colour = "lmm-lasso")) +
  geom_line(aes(y = glmnet, colour = "glmnet")) +
  geom_line(aes(y = rf, colour = "randfor")) + 
  geom_line(aes(y = bigRR, colour = "bigRR")) +
  geom_line(aes(y=am, colour="am+")) +
  scale_color_manual(values = brewer.pal(7, "Set1"))

ts <-  c( "150 x 5K\n2.8" , "1500 x 50K\n4.9",
          "350 x 400K\n5.1" ,  "2000 x 500K\n6.0",  
          "4000 x 1.5M\n6.8",  "10000 x 1.5M\n7.2")




p1 <- p1 + scale_x_continuous(breaks=unique(df$Pop), labels=ts, limits=c(2.5,7.5)) 
p1 <- p1 + scale_size(range=c(2, 2), guide=FALSE)


##-----------------
## Set theme
##------------------
p1 <- p1 + theme_stata() + theme(plot.background=element_rect(fill="grey93")) 



#----------------------
# alter x axis labels
#----------------------
p1 <- p1 + 
  theme(axis.text.x = element_text(size=12,  angle=60, vjust=c(0.5,0.5,0.5,0.5,0.5,0.5))) +
  theme(axis.text.y=element_text(size=sizefn, hjust=0.5))



p1 <- p1  + ylab(bquote("memory usage (in Gbytes)")) + 
  xlab(bquote('simulation scenario and its ' ~log[10]~ 'number of genotypes'))


## set y limits
p1 <- p1 + ylim(0,125)


## add title
p1 <- p1 + ggtitle("Memory Usage")
p1 <- p1 + theme(plot.title = element_text(size=20, face="bold", vjust=2))

#  increase tick marker lenght (x and y)
p1 <- p1 + theme(axis.ticks.length=grid::unit(0.65,"cm"))

##  change x and y labels size and bold
p1 <- p1 + theme(axis.title.x = element_text(angle=0, vjust=1, size=sizefn)) 
p1 <- p1 + theme(axis.title.y = element_text(angle=90, vjust=1, size=sizefn))

# change size of y label
p1 <- p1 + theme(axis.title.y=element_text(angle=90, size=sizefn))





## increase font of lengend + remove legend title
p1 <- p1 +  theme(legend.text=element_text(size=sizefn))
p1 <- p1 +  theme(legend.title=element_blank())
p1 <- p1 + guides(colour = guide_legend(override.aes = list(size=3)))

## add annotations to plot
p1 <- p1 + annotate("text", x=6.0, y=df[["lasso"]][4], label="M") +
  annotate("text", x=6.0+0.1, y=df[["bigRR"]][4], label="C") + 
  annotate("text", x= 6.0+0.1, y=df[["rf"]][4], label="M") +
  annotate("text", x=6.8, y=df[["mlmm"]][5], label="M")+
  annotate("text", x=6,0, y=df[["glmnet"]][4], label="M")
  
## Add line for size of data set
M <- numgenos*8/1000000  ## read in as double precision 


p1 <- p1 + geom_line(aes(y=M, x=df$Pop), linetype="dashed" )
  
p1













