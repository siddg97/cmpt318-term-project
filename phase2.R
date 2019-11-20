library(lubridate)
library(TTR)

ds <- data
test <- na.omit(
            read.csv('U:/cmpt318/test_data/test1.txt')
          # read.csv('~/git/cmpt318-term-project/TrainData.txt')
        );

###########################################
###########     CHARACTERISTIC 1 ##########
###########################################

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
month.name <- function(n) {
  ifelse(n==1,'January',
    ifelse(n==2, 'Feburary', 
           ifelse(n==3,'March', 
                  ifelse(n==4, 'April', 
                         ifelse(n==5, 'May', 
                                ifelse(n==6, 'June', 
                                       ifelse(n==7, 'July', 
                                              ifelse(n==8, 'August', 
                                                     ifelse(n==9, 'September',
                                                            ifelse(n==10, 'October',
                                                                   ifelse(n==11, 'November', 'December')
                                                            )
                                                    )
                                              )
                                      )
                              )
                        )
                  )
            )
    )
  )
}

## plot point-anomalies for each month GAP
for (i in 1:12) {
  ## Get raw month data
  m <- test[month(test$Date)==i, ]
  
  ## Find points below min and their indices
  below <- m$Global_active_power[m$Global_active_power < month.mins$GAP[i]]
  below.idx <- which(m$Global_active_power < month.mins$GAP[i])
  
  ## Find points above max and their indices
  above <- m$Global_active_power[m$Global_active_power > month.maxs$GAP[i]]
  above.idx <- which(m$Global_active_power > month.maxs$GAP[i])
  
  ## Plot them mofos
  layout(1)
  plot(
    m$Global_active_power, main=paste('Anomalies for',month.name(i), sep=' '), 
    col='black', lwd=1, xlab='Days ->', xaxt='n', ylab='Global Active Power',
    panel.first=grid(NULL,NULL,lwd=1,col='gray'), pch=20
  )
  points(below.idx,below, col='red', pch=20, lwd=2)
  points(above.idx,above, col='chocolate2', pch=20, lwd=2)
  abline(month.maxs$GAP[i],0,col='green',lwd=3)
  abline(month.mins$GAP[i],0,col='green',lwd=3)
  legend('topright',c('Above','Normal','Below'),cex=.8,col=c('chocolate2','black','red'),pch=c(20,20,20))
}

###########################################
###########     CHARACTERISTIC 2 ##########
###########################################

z <- 3
# The z score threshold
# A data value is an anomaly if its mean is not in [-z*SD, +z*SD]
# Can be changed to modify sensitivity to point anomalies

## get raw data
move.GAP <- data.frame(Raw = test$Global_active_power)

## run the moving average
move.GAP$Mean <- runMean(move.GAP$Raw, n=20)

## run the moving SD
move.GAP$SD <- runSD(move.GAP$Raw, n=20)

## find anomalies
anomaly.GAP <- move.GAP$Raw[( (move.GAP$Raw < move.GAP$Mean - z*move.GAP$SD) | (move.GAP$Raw > move.GAP$Mean + z*move.GAP$SD) & !is.na(move.GAP$Mean) )]
anomaly.GAP <- anomaly.GAP[!is.na(anomaly.GAP)]
## accumulate indices
anomaly.GAP.idx <- which(move.GAP$Raw < move.GAP$Mean - z*move.GAP$SD | move.GAP$Raw > move.GAP$Mean + z*move.GAP$SD )

## Plot them mofos
plot(move.GAP$Raw)
points(anomaly.GAP.idx, anomaly.GAP,col='red', lwd=2)
