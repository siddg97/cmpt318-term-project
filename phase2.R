library(lubridate)

ds <- data
test <- na.omit(read.csv('U:/cmpt318/test_data/test1.txt'));

# ## [TRAINING DATA] Weekly minimums for wednesday evenings for GAP, V and GI
# week.mins <- aggregate(
#                  list(GAP = ds$Global_active_power, V = ds$Voltage, GI = ds$Global_intensity), 
#                  by= list(Week = week(as.Date(ds$Date, format='%d/%m/%Y'))), 
#                  FUN = min
#              )
# 
# ## [TRAINING DATA] Weekly maximums for wednesday evenings for GAP, V and GI
# week.maxs <- aggregate(
#                  list(GAP = ds$Global_active_power, V = ds$Voltage, GI = ds$Global_intensity), 
#                  by= list(Week = week(as.Date(ds$Date, format='%d/%m/%Y'))), 
#                  FUN = max
#              )

## [TRAINING DATA] Monthly minimums for wednesday evenings for GAP, V and GI
month.mins <- aggregate(
                  list(GAP = ds$Global_active_power, V = ds$Voltage, GI = ds$Global_intensity), 
                  by= list(Month = month(as.Date(ds$Date, format='%d/%m/%Y'))), 
                  FUN = min
              )

## [TRAINING DATA] Monthly maximums for wednesday evenings for GAP, V and GI
month.maxs <- aggregate(
                  list(GAP = ds$Global_active_power, V = ds$Voltage, GI = ds$Global_intensity), 
                  by= list(Month = month(as.Date(ds$Date, format='%d/%m/%Y'))), 
                  FUN = max
              )

## plot point-anomalies for each month GAP
for (i in 1:12) {
  m <- test[month(test$Date)==i, ]
  
  ## Find points below and their indices
  below <- m$Global_active_power[m$Global_active_power < month.mins$GAP[i]]
  below.idx <- which(m$Global_active_power < month.mins$GAP[i])
  
  ## Find points above and their indices
  above <- m$Global_active_power[m$Global_active_power > month.maxs$GAP[i]]
  above.idx <- which(m$Global_active_power > month.maxs$GAP[i])
  
  ## Plot them mofos
  layout(1)
  plot(m$Global_active_power, main=as.character(i))
  points(below.idx,below, col='red', lwd=2)
  points(above.idx,above, col='red', lwd=2)
  abline(month.maxs$GAP[i],0,col='blue',lwd=2)
  abline(month.mins$GAP[i],0,col='blue',lwd=2)
}

