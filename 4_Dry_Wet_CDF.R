# add a Dry/Wet factor to tide time series (1 for Wet, 0 for Dry)
load(file = "tide_rain_detrend.Rdata")

add_DW_factor<- function(ts,ref,tag,yrange){
    library(lubridate)
    #get event boundaries (add 1hr for cut function)
    event_boundary <-data.frame(ref[,'start'],ref[,'start']+
                                    hours(ref[,'duration']+1))

    #convert boundaries to a POSIXct vector (connects head to tail)
    breaks <- ymd_hms(c(t(event_boundary)),tz="EST")
    #create a factor based on datetime of the time series
    f <- cut(ts$dt,breaks,right=F)
    levels(f) <- rep(x = c("Wet","Dry"),len=length(levels(f)))
    d <- data.frame(ts,D_W=f)

    library(ggplot2)
    # add multiplicator to scale
    mult_format <- function() {
        function(x) format(100*x,digits = 2)
    }
    p <- ggplot(d)+
        stat_ecdf(aes(x=tide,color='Overall'),size=1.2)+   # overall
        # Note: 'Overall' will show as the legend name, can be any char
        stat_ecdf(aes(x = tide,color=D_W))+     # dry, wet only
        labs(y="cdf",x="Tide Elev. (ft, NAVD88)",title="Dry vs. Wet Periods CDF")+
        scale_y_continuous(labels = mult_format())+
        scale_x_continuous(limits =yrange)+
        geom_vline(xintercept = c(2.47,-4.22),size=0.2,linetype='dashed')+
        annotate(geom="text", label="MHHW 1901 = 2.47 feet NAVD88",y=0.15,x=2.47,vjust=-1)+
        annotate(geom="text", label="MLLW 1901 = -4.22 feet NAVD88",y=0.15,x=-4.22,vjust=-1)+
        #geom_hline(yintercept = 0.5,size=0.2,aes(linetype='b'))+
        theme(axis.title = element_text(size = rel(1.2)),
              axis.text=element_text(size = rel(1.2))) +
        coord_flip()
    return(p)
}
# add tide stats (hf-high frequency, lf-low frequency)
p.tide.lf.cdf <- add_DW_factor(ts=tide.low,ref=rain,yrange = c(-6,6))
p.tide.lf.cdf.zoom <- add_DW_factor(ts=tide.low,ref=rain,yrange = c(-4,4))
p.tide.hf.cdf <- add_DW_factor(ts=tide.high,ref=rain,yrange=c(-8,8))
save(p.tide.lf.cdf,p.tide.hf.cdf,file="Dry_Wet_tide_CDF.Rdata")
