library(lubridate)

ds <- data

## [TRAINING DATA] Weekly minimums for wednesday evenings for GAP, V and GI
week.mins <- aggregate(
                 list(GAP = ds$Global_active_power, V = ds$Voltage, GI = ds$Global_intensity), 
                 by= list(Week = week(as.Date(ds$Date, format='%d/%m/%Y'))), 
                 FUN = min
             )

## [TRAINING DATA] Weekly maximums for wednesday evenings for GAP, V and GI
week.maxs <- aggregate(
                 list(GAP = ds$Global_active_power, V = ds$Voltage, GI = ds$Global_intensity), 
                 by= list(Week = week(as.Date(ds$Date, format='%d/%m/%Y'))), 
                 FUN = max
)

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

