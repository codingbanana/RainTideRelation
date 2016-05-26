# load data trend/detrend
load(file = "tide_rain.Rdata")

# pipe operator hotkey: Ctrl+Shift+M

# # method 1: FOR loop. works but too slow.
# t1 <- Sys.time()
# library(zoo)
# tide.zoo <- zoo(tide.low$tide,tide.low$dt)
# for (i in 1:length(rain$start)){
#     tmp <- window(tide.zoo,start = rain$start[i],end = rain$end[i])
#     if (length(tmp)>0){
#         rain[i,'td.max.lf'] <- max(tmp)
#         rain[i,'td.min.lf'] <- min(tmp)
#         rain[i,'td.mean.lf'] <- mean(tmp)
#     }
# }
# t2 <- Sys.time()
# # write.csv(rain,"rain_td_hf.csv",row.names = F,quote = F)
#
# # # method2 - dplyr
# t3 <- Sys.time()
# library(dplyr)
# cal_stats <- function(start_dtime,end_dtime) {
#     d_event <- tide.low %>%
#         filter(dt >= start_dtime & dt <= end_dtime)
#     s <- data.frame(max(d_event$tide),min(d_event$tide),mean(d_event$tide))
#     return(s)
# }
#
# d_stats2_list <-mapply(FUN = cal_stats, rain$start, rain$end, SIMPLIFY = FALSE)
# d_stats2 <- data.frame('Event' = seq(1,nrow(rain),1), matrix(unlist(d_stats2_list), nrow=nrow(rain), byrow=T),stringsAsFactors=FALSE)
# names(d_stats2)[-1] <- c('max', 'min', 'mean' )
# t4 <- Sys.time()

## method3:  tapply+cut
# prototype:
#     library(lubridate)
#     tide.low. <- tide.low
#     tide.low. <- tide.low.[order(tide.low.$dt),]
#     range <- rain[,c(1,5)]
#     breaks <- c(t(range))
#     breaks <- ymd_hms(breaks,tz="EST")
#     td <- data.frame(tide.low,cut(tide.low$dt,breaks))
#
#     factor <- list(cut(tide.low.$dt,breaks))
#     td <- tapply(tide.low.$tide,factor,mean)
#     td <- data.frame(start=names(td),td.mean.lf=as.vector(td))
#     ## aggregate removes empty levels, this method is abandoned
#     #td <- aggregate(tide.low.$tide, factor,mean,na.rm=F)
#     td2 <- td[seq(2,nrow(td),2),]
#     tt <- merge(rain,td2)

# function implementation:

## add tide stats to rain (ref) table
# t5 <- Sys.time()
add_tide_stats <- function(ts,ref,tag){
    library(lubridate)
    #get event boundaries (add 1hr for cut function)
    event_boundary <-data.frame(ref[,'start'],ref[,'start']+
                                    hours(ref[,'duration']+1))
    #convert boundaries to a POSIXct vector (connects head to tail)
    breaks <- ymd_hms(c(t(event_boundary)),tz="EST")
    #create a factor based on datetime of the time series
    f <- list(cut(ts$dt,breaks,right=F))
    #sub-function to calculate the stats
    stats <- function(x,f,func,...){
        # apply stats(max, min, mean) of tide (ts$tide) by factor
        o <- tapply(x,f,func,...)
        # extract only the event groups(odd elements)
        o <- o[seq(1,length(o),2)]
        return(o)
    }
    # create a stats data frame w/ name
    p <- data.frame(stats(ts$tide,f,min),
                    stats(ts$tide,f,mean),
                    stats(ts$tide,f,max))
    colnames(p) <- c(paste0('td.min.',tag),
                     paste0('td.mean.',tag),
                     paste0('td.max.',tag))
    #append stats to rain (ref) table
    p <- cbind(ref,p)
    return(p)
}
# add tide stats (hf-high frequency, lf-low frequency)
rain_lf <- add_tide_stats(ts=tide.low,ref=rain,tag='lf')
# write.csv(rain_lf,file = "rain_lf.csv",quote = F,row.names = F)
# t6 <- Sys.time()
rain_hf <- add_tide_stats(tide.high,rain,'hf')
save(rain_lf, rain_hf,file = "tide_stats.Rdata")

# do the same procedure for detrend data
load(file = "tide_rain_detrend.Rdata")
# add tide stats (hf-high frequency, lf-low frequency)
rain_lf <- add_tide_stats(ts=tide.low,ref=rain,tag='lf')
# write.csv(rain_lf,file = "rain_lf.csv",quote = F,row.names = F)
# t6 <- Sys.time()
rain_hf <- add_tide_stats(tide.high,rain,'hf')
save(rain_lf, rain_hf,file = "tide_stats_detrend.Rdata")

# # method4 : from Eddie
# t7 <- Sys.time()
#     event_count <- nrow(rain)
#     # with apply
#     cal_stats <- function(start_dtime,end_dtime) {
#         d_event <- subset(x=tide.low, dt >= start_dtime &
#                               dt <= end_dtime)
#         s <- data.frame(max(d_event$tide),min(d_event$tide),mean(d_event$tide))
#         return(s)
#     }
#
#     d_stats2_list <-mapply(FUN = cal_stats, rain$start, rain$end,SIMPLIFY = FALSE)
#     d_stats2 <- data.frame('Event' = seq(1,event_count,1),matrix(unlist(d_stats2_list), nrow=event_count, byrow=T),stringsAsFactors=FALSE)
#     names(d_stats2)[-1] <- c('max', 'min', 'mean')
#
# # Profile the methods
# t8 <- Sys.time()
# paste("method1: ", round(difftime(t2,t1,units = "s"),0), "s")
# paste("method2: ", round(difftime(t4,t3,units = "s"),0), "s")
# paste("method3: ", round(difftime(t6,t5,units = "s"),0), "s")
# paste("method4: ", round(difftime(t8,t7,units = "s"),0), "s")

# method1: 1203s
# method2: 328s
# method3: 2s (!winner!)
# method4: 489s

# results from method 3 and 4 are compared, QA passed.
