# plotting results
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

make_plots <- function(dataset){
    t <- dataset
    # d is the melt data of t
    d <- gather(t[,c(1,6:8)],stat,level,-start,na.rm = T)

    # all-in-one ts plot w/ LOESS regression
    # <<- assign variable in global scope
    p.TS.all<- ggplot(d)+
        geom_point(data=d,aes(start,level,colour=stat),alpha=0.5, shape=21)+
        geom_smooth(data=t,aes(x=start, y=t[,7]),size=2, fill="blue")

    # mean TS plot w/ regression
    t <- t[complete.cases(t),]
    p.TS.mean <<- ggplot(t,aes(start,t[,7]))+
        geom_point(alpha=0.2, color="blue")+
        geom_smooth(size=1.1, color="red")+
        labs(x="Year", y="mean tide (ft, NAVD88)",
             title="Mean Mean Tide Time Series")+
        theme(axis.title = element_text(size = rel(1.2)),
              axis.text=element_text(size = rel(1.2)))

    # cdfs for tide stats
#     add_cdf <- function(data=t,col,n=1){
#         d <- data[,col]
#         ranks <- rank(d, ties.method='max',na.last = 'keep')
#         cdf <- (ranks / (max(ranks, na.rm=TRUE)+n)) * 100
#     }
#     cdf_out <- mapply(FUN=add_cdf,col=as.list(colnames(t)[6:8]))
#     cdf_out.vc <- as.vector(cdf_out)
#     cdf_out.vc <- subset(cdf_out.vc,complete.cases(cdf_out.vc))
#     d.cdf <- data.frame(d,cdf=cdf_out.vc)
#
#     p.cdf.all<<- ggplot(d.cdf,aes(y=level,x=cdf,group=stat,color=stat))+
#         geom_step()+
#         labs(x="cdf",y="mean tide (ft, NAVD88)",title="All events")+
#         theme(axis.title = element_text(size = rel(1.2)),
#               axis.text=element_text(size = rel(1.2)))

    # alternative: with shades between mean cdf
    ## calculate cdf for each tide, contains NA
    cdf.all <- lapply(t[,6:8], FUN = function(x,n=1){
        ranks <- rank(x, ties.method='max',na.last = 'keep')
        cdf <- (ranks / (max(ranks, na.rm=TRUE)+n)) * 100
    })
    ## flatten the matrix into a vector that contains only cdfs
    cdf.out <- unlist(cdf.all)
    ## remove NAs
    cdf.out <- cdf.out[!is.na(cdf.out)]
    ## create a tide-cdf paired dataset
    d.cdf <- data.frame(d,cdf=cdf.out)

    tide.cdf <- cbind(t[,6:8],cdf.all)
    tide.min <-setNames(tide.cdf[,c(4,1)],c('x','y'))
    tide.min <- tide.min[order(tide.min$y,na.last = NA),]
    tide.max <-setNames(tide.cdf[,c(6,3)],c('x','y'))
    tide.max <- tide.max[order(tide.max$y,na.last = NA,decreasing = T),]

    poly_df <- rbind(tide.min, tide.max)
    p.cdf.all<<- ggplot(d.cdf)+
        geom_polygon(data = poly_df,aes(x = x,y = y),
                     fill = "lightgray",alpha = 0.8)+
        geom_line(aes(y=level,x=cdf,group=stat,color=stat))+
        labs(x="cdf",y="mean tide (ft, NAVD88)",title="All events")+
        geom_hline(yintercept = c(2.47,-4.22),size=0.2,linetype='dashed')+
        annotate(geom="text", label="MHHW 1901 = 2.47 feet NAVD88",x=15,y=2.47,vjust=-1)+
        annotate(geom="text", label="MLLW 1901 = -4.22 feet NAVD88",x=15,y=-4.22,vjust=-1)+
        theme(axis.title = element_text(size = rel(1.2)),
              axis.text=element_text(size = rel(1.2)))

    # duration vs tide
    p.tide.vs.duration <<- ggplot(t, aes(x=duration,y=t[,7]))+
        geom_point(alpha=0.2)+
        geom_smooth(size=2, fill="red")+
        labs(x="rainfall duration (hr)",
             y="mean tide (ft, NAVD88)",
             title=" Duration vs. Tide")+
        theme(axis.title = element_text(size = rel(1.2)),
              axis.text=element_text(size = rel(1.2))) +
        theme_bw()

    # categorized CDF by depth
    ## method 1: range (0~1,1~2,2~3,3~Inf)
    depth.factor <- cut(t$depth,breaks = c(0:3,Inf))
    # add multiplicator to scale
    mult_format <- function() {
        function(x) format(100*x,digits = 2)
    }
    p.cdf.by.Rdepth1 <<- ggplot(t, aes(x=t[,7],group=depth.factor))+
        stat_ecdf(aes(color=depth.factor))+
        labs(y="cdf",x="mean tide (ft, NAVD88)",title="Categorized tide, method 1")+
        scale_y_continuous(labels = mult_format())+
        geom_vline(xintercept = c(2.47,-4.22),size=0.2,linetype='dashed')+
        annotate(geom="text", label="MHHW 1901 = 2.47 feet NAVD88",y=0.15,x=2.47,vjust=-1)+
        annotate(geom="text", label="MLLW 1901 = -4.22 feet NAVD88",y=0.15,x=-4.22,vjust=-1)+
        theme(axis.title = element_text(size = rel(1.2)),
              axis.text=element_text(size = rel(1.2))) +
        coord_flip()

    ## method 2: range(0~Inf,1~Inf,2~Inf,3~Inf)
    tide <- lapply(0:3, function(x) {subset(t, t$depth>x)[,7]})
    tide.melt <- data.frame(
        Index = rep(paste0(">",0:3), sapply(tide, length)),
        tide = unlist(tide))
    tide.cdf <- lapply(tide, FUN = function(x,n=1){
        ranks <- rank(x, ties.method='max',na.last = 'keep')
        cdf <- (ranks / (max(ranks, na.rm=TRUE)+n)) * 100
    })
    tide.combine <- cbind(tide.melt,cdf=unlist(tide.cdf))
    tide.combine <- tide.combine[complete.cases(tide.combine),]
    p.cdf.by.Rdepth2 <<- ggplot(tide.combine) +
        geom_line(aes(y=tide,x=cdf,color=Index, group=Index)) +
        labs(x="cdf",y="mean tide (ft, NAVD88)",title="Categorized tide, method 2")+
        geom_hline(yintercept = c(2.47,-4.22),size=0.2,linetype='dashed')+
        annotate(geom="text", label="MHHW 1901 = 2.47 feet NAVD88",x=15,y=2.47,vjust=-1)+
        annotate(geom="text", label="MLLW 1901 = -4.22 feet NAVD88",x=15,y=-4.22,vjust=-1)+
        theme(axis.title = element_text(size = rel(1.2)),
              axis.text=element_text(size = rel(1.2)))

    # visualize plots
    multiplot(p.cdf.all,p.cdf.by.Rdepth1,p.TS.mean,p.cdf.by.Rdepth2,cols=2)
}
make_plots(rain_lf)
# save all plots in a Rdata file
# deparse(substitute()): returns the object name as a string
save(list=grep("(^p\\.)",ls(),value=T),file ="ggplots_lf.Rdata")
# rm()
# # same way for high frequency data
# make_plots(rain_hf)
# # besides using global variable, save the objects in a separate file
# save(list=grep("(^p\\.)",ls(),value=T),file = "ggplots_hf.Rdata")
