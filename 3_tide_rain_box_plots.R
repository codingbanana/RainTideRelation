# plotting results in box plots
## choose which dataset you want to plot:
## both dataset shares same object names, so they cannot be load concurrently
#load("tide_stats.Rdata")
load("tide_stats_detrend.Rdata")

library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(plotly)
# for multiplot function
source("C:\\Users\\hao.zhang\\Documents\\Rprojects\\HZtoolbox\\R\\multiplot_func.R")

t <- rain_lf
# remove NA records in stats
t <- t[complete.cases(t),]
# d is the melt data of t
d <- gather(t[,c(1,6:8)],stat,level,-start,na.rm = T)

# all-in-one ts plot w/ LOESS regression
p.TS.all<- ggplot(d)+
    geom_point(aes(start,level,colour=stat),alpha=0.5, shape=21)+
    geom_smooth(data=t,aes(x=start, y=t[,7]),size=1.5, fill="blue")+
    labs(x="Year", y="Event-Mean tide Elevation (ft, NAVD88)",
         title="Event-based Tide Stats Time Series")+
    theme(axis.title = element_text(size = rel(1.2)),
          axis.text=element_text(size = rel(1.2)))

# mean TS plot w/ regression
p.TS.mean <- ggplot(t,aes(start,t[,7]))+
    geom_point(alpha=0.2, color="blue")+
    geom_smooth(size=1.1, color="red")+
    labs(x="Year", y="Event-Mean tide Elevation (ft, NAVD88)",
         title="Event-Mean Tide Time Series")+
    theme(axis.title = element_text(size = rel(1.2)),
          axis.text=element_text(size = rel(1.2)))

# calculate cdf for all with shades between mean cdf
cdf.all <- lapply(t[,6:8], FUN = function(x,n=1){
    ranks <- rank(x, ties.method='max')
    cdf <- (ranks / (max(ranks)+n)) * 100
})
## flatten the matrix into a vector that contains only cdfs
cdf.out <- unlist(cdf.all)
## remove NAs
cdf.out <- cdf.out[!is.na(cdf.out)]
## create a tide-cdf paired dataset
d.cdf <- data.frame(d,cdf=cdf.out)
##
tide.cdf <- cbind(t[,6:8],cdf.all)
tide.min <-setNames(tide.cdf[,c(4,1)],c('x','y'))
tide.min <- tide.min[order(tide.min$y,na.last = NA),]
tide.max <-setNames(tide.cdf[,c(6,3)],c('x','y'))
tide.max <- tide.max[order(tide.max$y,na.last = NA,decreasing = T),]

poly_df <- rbind(tide.min, tide.max)
p.cdf.all<- ggplot(d.cdf)+
    geom_polygon(data = poly_df,aes(x = x,y = y),
                 fill = "lightgray",alpha = 0.8)+
    geom_line(aes(y=level,x=cdf,group=stat,color=stat))+
    labs(x="CDF,% equal or less",y="Event-Mean tide Elevation (ft, NAVD88)",title="Event-based Tide Stats")+
    geom_hline(yintercept = c(2.47,-4.22),size=0.2,linetype='dashed')+
    annotate(geom="text", label="MHHW 1901 = 2.47 feet NAVD88",x=0,y=2.47,vjust=-1, hjust=0)+
    annotate(geom="text", label="MLLW 1901 = -4.22 feet NAVD88",x=0,y=-4.22,vjust=-1,hjust=0)+
    theme(axis.title = element_text(size = rel(1.2)),
          axis.text=element_text(size = rel(1.2)),
          legend.justification=c(1,0), legend.position=c(1,0))

# duration vs tide
p.tide.vs.duration <- ggplot(t, aes(x=duration,y=t[,7]))+
    geom_point(alpha=0.2)+
    geom_smooth(size=2, fill="red")+
    labs(x="rainfall duration (hr)",
         y="mean tide (ft, NAVD88)",
         title=" Duration vs. Tide")+
    theme(axis.title = element_text(size = rel(1.2)),
          axis.text=element_text(size = rel(1.2)),
          legend.position="none") +
    theme_bw()

# categorized CDF by depth
## method 1: range (0~1,1~2,2~3,3~Inf)
depth.factor <- cut(t$depth,breaks = c(0:3,Inf))
n_count <- summary(depth.factor)

p.box.by.Rdepth1 <- ggplot(t, aes(y=t[,7],x=depth.factor))+
    geom_boxplot(aes(color=depth.factor))+
    labs(x="Rainfall depth category (in)",y="Event-Mean tide Elevation (ft, NAVD88)",title="Categorized tide box-whisker plot, method 1")+
    geom_hline(yintercept = c(2.47,-4.22),size=0.2,linetype='dashed')+
    annotate(geom="text", label="MHHW 1901 = 2.47 feet NAVD88",x=0.5,y=2.47,vjust=-1,hjust=0)+
    annotate(geom="text", label="MLLW 1901 = -4.22 feet NAVD88",x=0.5,y=-4.22,vjust=-1,hjust=0)+
    annotate(geom="text",label=paste0("n = ",n_count),x = 1:length(n_count),y = 4,size=4)+
    theme(axis.title = element_text(size = rel(1.2)),
          axis.text=element_text(size = rel(1.2)),
          legend.position="none")

## method 2: range(0~Inf,1~Inf,2~Inf,3~Inf)
tide <- lapply(0:3, function(x) {subset(t, t$depth>x)[,7]})
tide.melt <- data.frame(
    index = rep(paste0(">",0:3), sapply(tide, length)),
    tide = unlist(tide))
n_count <- summary(tide.melt$index)
p.box.by.Rdepth2 <- ggplot(tide.melt,aes(y=tide,x=index)) +
    geom_boxplot(aes(color=index)) +
    labs(x="Rainfall depth category (in)",y="Event-Mean tide Elevation (ft, NAVD88)",title="Categorized tide box-whisker plot, method 2")+
    geom_hline(yintercept = c(2.47,-4.22),size=0.2,linetype='dashed')+
    annotate(geom="text", label="MHHW 1901 = 2.47 feet NAVD88",x=0.5,y=2.47,vjust=-1,hjust=0)+
    annotate(geom="text", label="MLLW 1901 = -4.22 feet NAVD88",x=0.5,y=-4.22,vjust=-1,hjust=0)+
    annotate(geom="text",label=paste0("n = ",n_count),x = 1:length(n_count),y = 4,size=4)+
    theme(axis.title = element_text(size = rel(1.2)),
          axis.text=element_text(size = rel(1.2)),
          legend.position="none")

# visualize plots in one figure
multiplot(p.cdf.all,p.box.by.Rdepth1,p.TS.mean,p.box.by.Rdepth2,cols=2)
# save all plots in a Rdata file
# deparse(substitute()): returns the object name as a string
save(list=grep("(^p\\.)",ls(),value=T),file ="ggplots_lf_box.Rdata")

# export plots
sapply(grep("(^p\\.)",ls(),value=T),function (x) {
    ggsave(filename = paste0(x,".png"),plot = get(x),device = "png",
           path = file.path(getwd(),"exportPNG"),scale = 0.7,
           width = 12,height = 8,units = 'in',dpi = 300)
})

# rm()
# # same way for high frequency data
# make_plots(rain_hf)
# # besides using global variable, save the objects in a separate file
# save(list=grep("(^p\\.)",ls(),value=T),file = "ggplots_hf_box.Rdata")
