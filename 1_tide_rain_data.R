# clip tide data based on rainfall periods, and calculate stats

# tide-high frequency
raw <- read.csv("Tide_Phila_obs_P11adj0.09.csv",header = T,stringsAsFactors = F)
tide.high <- data.frame(dt = ISOdatetime(raw$Y, raw$M,raw$D,raw$H,0,0,tz = 'EST'),tide = raw$Water.Level.FT.NAVD88)

# tide-low frequency
raw <- read.csv("Tide_PhilSL_LPFilt_01_2015.csv",header = T,stringsAsFactors = F)
tide.low <- data.frame(dt = ISOdatetime(raw$Y, raw$M,raw$D,raw$H,0,0,tz = 'EST'),tide = raw$Low.Pass..feet.NAVD88.)

# rain-event definition
raw <- read.csv("NetSTORM_PHLRainfallEvents.csv",header = T,stringsAsFactors = F)
rain <- data.frame(start = lubridate::mdy_hm(raw$Date, tz = 'EST'), duration = raw$EventHrs, depth=raw$Depth,peak=raw$Maximum)
rain$end <- rain$start+lubridate::hours(rain$duration)

#destory raw
rm(raw)
save(tide.low,tide.high,rain.def,file = "tide_rain.Rdata")