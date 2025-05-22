# Temp_eco_curve_comparison


## Script - 01_data_collation_and_format.R

Formats and joins bio and all temp data

Data needed 
Bio data - ignore/Bio/SMC_bioassessment2.csv 
TempEst - ignore/modelledTemp/biosites_TempMetrics.csv
Rogers et al - ignore/modelledTemp/baseline_stream_temp.RData

## TempEst variable descriptions

temp.mod is modeled daily mean temperature
temp.doy is modeled day-of-year mean temperature
temp.anom is modeled temperature anomaly relative to day-of-year mean
temp.plus is modeled maximum temperature relative to day-of-year mean
temp.max is modeled maximum daily temperature, adjusted up by 10%

## TempEst metrics descriptions

lst_max7rav	      the max value of rolling 7-day average for lst
humidity_max7rav	the max value of rolling 7-day average for humidity
temp.anom_max7rav	the max value of rolling 7-day average for temp.anom
temp.plus_max7rav	the max value of rolling 7-day average for temp.plus
temp.doy_max7rav	the max value of rolling 7-day average for temp.doy
tmod_min7rmn	    the minimum value of a rolling 7-day minimum for temp.mod
tmax_min7rmn	    the minimum value of a rolling 7-day minimum for temp.max
tmod_max7rmx	    the maximum value of a rolling 7-day maximum for temp.mod
tmax_max7rmx	    the maximum value of a rolling 7-day maximum for temp.max
tmod_max7rav	    the max value of rolling 7-day average for temp.mod
tmax_max7rav	    the max value of rolling 7-day average for temp.max
tmod_maxdiff	    max difference between rolling 7-day min and max for temp.mod
tmax_maxdiff	    max difference between rolling 7-day min and max for temp.max
tmod_avdiff	      average difference between rolling 7-day min and max for temp.mod
tmax_avdiff	      average difference between rolling 7-day min and max for temp.max
tmod_ab30count	  the number of 7-day rolling averages that are over 30 for temp.mod
tmax_ab30count	  the number of 7-day rolling averages that are over 30 for temp.max