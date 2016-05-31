# add a Dry/Wet factor to tide time series (1 for Wet, 0 for Dry)
load(file = "tide_rain_detrend.Rdata")

DW_box<- function(ts){
    library(lubridate)
    #get event boundaries (add 1hr for cut function)
    event_boundary <-data.frame(rain[,'start'],rain[,'start']+
                                    hours(rain[,'duration']+1))

    #convert boundaries to a POSIXct vector (connects head to tail)
    breaks <- ymd_hms(c(t(event_boundary)),tz="EST")
    #create a factor based on datetime of the time series
    f <- cut(ts$dt,breaks,right=F)
    levels(f) <- rep(x = c("Wet","Dry"),len=length(levels(f)))
    d <- data.frame(ts,D_W=f)

    library(ggplot2)
    p <- ggplot(d)+
        geom_boxplot(aes(y=tide,x='Overall',color='Overall'),size=1.2)+   # overall
        # Note: 'Overall' will show as the legend name, can be any char
        geom_boxplot(aes(y = tide,x=D_W, color=D_W))+     # dry, wet only
        labs(y="Tide Elev, ft NAVD88",x="Dry/Wet",title="Dry vs. Wet Periods box-wishker plot")+
        geom_hline(yintercept = c(2.47,-4.22),size=0.2,linetype='dashed')+
        annotate(geom="text", label="MHHW detrend at 1901 = 2.47 feet",y=2.47,x=0,vjust=-1,hjust=0,size=4)+
        annotate(geom="text", label="MLLW detrend at 1901 = -4.22 feet",y=-4.22,x=0,vjust=1,hjust=0,size=4)+           #hjust=0: left align
        theme(axis.title = element_text(size = rel(1.2)),
              axis.text=element_text(size = rel(1.2)))

        sample_n <- c(sum(d$D_W=='Dry',na.rm = T),sum(complete.cases(d),na.rm = T),sum(d$D_W=='Wet',na.rm = T))
        p <- p + annotate(geom="text",label=paste0("n = ",sample_n),x = 1:3,y = 7,size=4)
        return(p)
}
# add tide stats (hf-high frequency, lf-low frequency)
p.tide.lf.box <- DW_box(ts=tide.low)
p.tide.hf.box <- DW_box(ts=tide.high)
save(p.tide.lf.cdf,p.tide.hf.cdf,file="Dry_Wet_tide_box.Rdata")
