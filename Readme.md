Title: Rain & tide coincidence analysis
Owner: Hao Zhang 
Created: 5/31/2016

Raw data:
tblPHLhour_forNetSTORM.csv: Rainfall Time Series, used for NetStorm analysis
NetSTORM_PHLRainfallEvents.csv: Rainfall Events definition, produced by NetStorm

PhilLPFilt-Adj09-detrendedto1901_1901-2015_NAVD88-EST.csv: Low frequency, Detrended, pivot at 1900, Tide Time Series
PhilSL-Adj09-detrendedto1901_1901-2015_NAVD88-EST.csv: High frequency, Detrended, pivot at 1900, Tide Time Series
Tide_Phila_obs_P11adj0.09.csv: High frequency Tide Time Series
Tide_PhilSL_LPFilt_01_2015.csv: Low frequency Tide Time Series

Not in use:
PhilSL_1901_2015_FtNAVDLSTPier11Adj.csv: High freqency, Filled, Tide Time Series
MaxTidePerEventLevel_Version2_20141208_VR.xlsx: event-based max tide by VR
PhilLPFilt-Adj09-detrended_1901-2015_NAVD88-EST-pivot1950.csv: Low frequency, Detrend, pivot at 1950, Tide Time Series
PhilSL-Adj09-detrended_1901-2015_NAVD88-EST-pivot1950.csv: High frequency, Detrend, pivot at 1950, Tide Time Series


Scripts:
1_tide_rain_data.R : read high & low frequency tide data (1901-2015, hourly, unfilled) and rain definitions, save data in tide_rain.Rdata

1_tide_rain_detrend_data.R : read high & low frequency tide data (detrended to 1901) and rain definitions, save data in tide_rain_detrend.Rdata

2_tide_rain_event_extraction.R : calcualte event-based tide stats (mean, max, min), multiple methods were compared while the fastest one is uncommented for production runs. save data in tide_stats.Rdata and tide_stats_detrend.Rdata

3_tide_rain_plots.R : create time-series, cdf (overall and categorized),tide vs. duration plots, save data in ggplots_lf.Rdata and ggplots_hf.Rdata. This file has been depreciated, use the 3_tide_rain_box_plots.R instead.

3_tide_rain_box_plots.R : create time-series, ovrall cdf, tide vs. duration, and categorized box-whisker plots, save data in ggplots_lf_box.Rdata, also export plots in exportPNG folder.

4_Dry_Wet_CDF.R : categorize each tide data point as either 'dry' or 'wet' based on the rain event definition, then make a CDF plot for the dry and wet categories. save data in Dry_Wet_tide_CDF.Rdata. This file has been depreciated, use the 4_Dry_Wet_box.R instead.

4_Dry_Wet_box.R : categorize each tide data point as either 'dry' or 'wet' based on the rain event definition, then make a box-whisker plots for the dry and wet categories. save data in Dry_Wet_tide_box.Rdata
