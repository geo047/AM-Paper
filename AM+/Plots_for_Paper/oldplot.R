require(ggplot2)
library(GGally)
library(ggthemes)
library(gridExtra)
library(reshape)
library(cowplot)
library(gtable)


##--------------------
## ggplot2 plots
##-----------------

setwd("/Users/geo047/AM+/Plots_for_Paper")
df1 <- read.table(file="cputimes.dat", header=TRUE)

startc  <- 2
finishc <- 7

# log10 scale
nms <- names(df)
logdf <- log10(df1[,startc:finishc])
logdf[logdf<1] <- 0   ## initializing values to 0 (less than a minute)
logdf <- cbind.data.frame(df[,1:1], logdf)
names(logdf) <- nms
names(logdf) <- c("Method", "150.x.5K",    "1500.x.50K",  "350.x.400K", 
                 "2000.x.500K" ,"4000.x.1.5M", "10000 x 1.5M")

# Generate basic parallel coordinate plot
p1 <- ggparcoord(data = logdf,                 
                # Which columns to use in the plot
                columns = startc:finishc,
                # Which column to use for coloring data
                groupColumn = 1,                 
                # Do not show points
                showPoints = TRUE,                
                # Turn on alpha blending for dense plots
                alphaLines = 0.6,                
                # Turn off box shading range
                shadeBox = NULL,                
                # Will normalize each column's values to [0, 1]
                scale = "globalminmax"  # try "std" also
                
) 


p1 <- p1 + theme_hc()   ## nice but no x axes




## add y labels  
p1 <- p1 + ylab(expression( bold(log[10] ( Computing~Time) )))


## manually label x axis values
p1 <- p1 + scale_y_continuous(breaks = c(0,1,2,3,log10(100*60)), labels=c("0", "1","2","3",
                                       "beyond \n machine \n capacity"))

#  increase tick marker lenght (x and y)
p1 <- p1 + theme(axis.ticks.length=grid::unit(0.3,"cm"))

##  change y labels size and bold
p1 <- p1 + theme(axis.title.y = element_text(angle=90, vjust=1, size=14))

# change size of x and y tick labels
p1 <- p1 + theme(axis.text.y=element_text(size=14, hjust=0.5))

## thicken line
p1 <- p1  + geom_line(size=2)




      ##----------------------------
      ##   Memory Usage
      ##----------------------------

df2 <- read.table(file="memory.dat", header=TRUE)


startc  <- 2
finishc <- 7

p2 <- ggparcoord(data = df2, columns = startc:finishc, groupColumn = 1,                 
                showPoints = TRUE,alphaLines = 0.6, shadeBox = NULL,                
                scale = "globalminmax") 

p2 <- p2 + theme_hc() 

## add y labels  
p2 <- p2  + ylab("Memory usage (in Gigabytes)")
p2 <- p2 + xlab(expression(bold(paste("Study type", ",", 
                                      " nind by nSNP", ",",
                                      " ngenotypes (x 10", {}^6, ")"))))


## manually label x axis values
p2 <- p2  + scale_x_discrete(labels=
                               c("Crops (unsequenced)\n150 x 5K\n0.75", 
                                 "Livestock\n1500 x 50K\n7.5", 
                                 "Sorgum\n350 x 400K\n90",
                                 "Humans (small)\n2000 x 500K\n1000",
                                 "Arabidopsis \n 4000 x 1.5M\n6000",
                                 "Humans (large) \n 10000 x 1.5M\n15000"))

p2 <- p2 + scale_y_continuous(breaks = c(0,50,100, 126), labels=c("0", "50","100", 
                  "beyond \n machine \n capacity"))


# change theme

#p <- p + theme_gdocs()
#p <- p + theme_stata()
#p <- p + theme_hc()   ## nice but no x axes
# p <- p + theme_gdocs()  ## this looks good
#p <- p + theme_wsj()  ## no
#p <- p + theme_economist()  ## no
#p <- p + theme_pander()  ## no
#p <- p + theme_solarized()  ## no
#p <- p + theme_fivethirtyeight()

##  change x and y TITLES  size and bold
p2 <- p2 + theme(axis.title.x = element_text(angle=0, vjust=1, size=14)) 
p2 <- p2 + theme(axis.title.y = element_text(angle=90, vjust=1, size=14))

# change size of x and y labels
p2 <- p2 + theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
  theme(axis.text.y=element_text(size=14, hjust=0.5))
p2 <- p2 + theme(axis.title.x = element_text(vjust=-0.45))

## thicken line
p2 <- p2  + geom_line(size=2)

## increase font of lengend + remove lengend title
p2 <- p2 +  theme(legend.text=element_text(size=14))
p2 <- p2 +  theme(legend.title=element_blank())


#  increase tick marker lenght (x and y)
p2 <- p2 + theme(axis.ticks.length=grid::unit(0.3,"cm"))




##======================
##    Multiple Plots
##======================

# remove legend from first plot
p1 <- p1 + theme(legend.position="none")

# remove labels from first plot
p1 <- p1 + theme(axis.text.x = element_blank(), axis.title.x = element_blank() )

gt1 <- ggplot_gtable(ggplot_build(p1))
gt2 <- ggplot_gtable(ggplot_build(p2))
maxWidth = unit.pmax(gt1$widths[2:3], gt2$widths[2:3])
maxWidth
gt1$widths[2:3] <- maxWidth
gt2$widths[2:3] <- maxWidth


gt <- gtable(widths = unit(c(2), "null"), height = unit(c(1.5, 2), "null"))
#gt <- gtable_add_grob(gt, gt1[, -5], 2, 1)
gt <- gtable_add_grob(gt, gt1, 1, 1)
gt <- gtable_add_grob(gt, gt2, 2, 1)
grid.newpage()
grid.draw(gt)

