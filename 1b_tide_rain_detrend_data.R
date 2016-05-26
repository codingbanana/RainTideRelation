# clip tide data based on rainfall periods, and calculate stats

# tide-high frequency
raw <- read.csv("PhilSL-Adj09-detrendedto1901_1901-2015_NAVD88-EST.csv",header = F,stringsAsFactors = F)
tide.high <- data.frame(dt = ISOdatetime(raw[[1]], raw[[2]],raw[[3]],raw[[4]],0,0,tz = 'EST'),tide = raw[[5]])
# the parsed data are not in chronic order
tide.high <- tide.high[order(tide.high$dt),]

# tide-low frequency
raw <- read.csv("PhilLPFilt-Adj09-detrendedto1901_1901-2015_NAVD88-EST.csv",header = F,stringsAsFactors = F)
tide.low <- data.frame(dt = ISOdatetime(raw[[1]], raw[[2]],raw[[3]],raw[[4]],0,0,tz = 'EST'),tide = raw[[5]])

# rain-event definition
raw <- read.csv("NetSTORM_PHLRainfallEvents.csv",header = T,stringsAsFactors = F)
rain <- data.frame(start = lubridate::mdy_hm(raw$Date, tz = 'EST'), duration = raw$EventHrs, depth=raw$Depth,peak=raw$Maximum)
rain$end <- rain$start+lubridate::hours(rain$duration)

#destory raw
rm(raw)
save(tide.low,tide.high,rain,file = "tide_rain_detrend.Rdata")
