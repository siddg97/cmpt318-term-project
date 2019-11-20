library(lubridate)
library(TTR)

ds <- data
test <- na.omit(
          #  read.csv('U:/cmpt318/test_data/test1.txt')
          read.csv('~/git/cmpt318-term-project/test_data/test1.txt')
        );

### APROACH 1

###########################################
###########   CHARACTERISTIC 1   ##########
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
                                                                   ifelse(n==11, 'November', 'December')))))))))))
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
    col='black', lwd=1, xlab=paste('Days in', month.name(i), sep=' '), xaxt='n', ylab='Global Active Power',
    panel.first=grid(NULL,NULL,lwd=1,col='gray'), pch=20
  )
  points(below.idx,below, col='red', pch=20, lwd=2)
  points(above.idx,above, col='chocolate2', pch=20, lwd=2)
  abline(month.maxs$GAP[i],0,col='blue',lwd=4)
  abline(month.mins$GAP[i],0,col='blue',lwd=4)
  legend('topleft', c('Above','Normal','Below'), cex=0.7, col=c('chocolate2','black','red'),pch=c(20,20,20))
}

############################################
###########   CHARACTERISTIC 2   ###########
############################################

z <- 3
# The z score threshold
# A data value is an anomaly if its mean is not in [-z*SD, +z*SD]
# Can be changed to modify sensitivity to point anomalies

## get raw data
move.GAP <- data.frame(Raw = test$Global_active_power)
move.GRP <- data.frame(Raw = test$Global_reactive_power)
move.V   <- data.frame(Raw = test$Voltage)
move.GI  <- data.frame(Raw = test$Global_intensity)

## run the moving average
move.GAP$Mean <- runMean(move.GAP$Raw, n=20)
move.GRP$Mean <- runMean(move.GRP$Raw, n=20)
move.V$Mean   <- runMean(move.V$Raw, n=20)
move.GI$Mean  <- runMean(move.GI$Raw, n=20)

## run the moving SD
move.GAP$SD <- runSD(move.GAP$Raw, n=20)
move.GRP$SD <- runSD(move.GRP$Raw, n=20)
move.V$SD <- runSD(move.V$Raw, n=20)
move.GI$SD <- runSD(move.GI$Raw, n=20)

## find anomalies

#### Global Active Power anomalies
anomaly.GAP <- move.GAP$Raw[( ((move.GAP$Raw < move.GAP$Mean - z*move.GAP$SD) | (move.GAP$Raw > move.GAP$Mean + z*move.GAP$SD)) & !is.na(move.GAP$Mean) )]
anomaly.GAP <- anomaly.GAP[!is.na(anomaly.GAP)]
## accumulate indices
anomaly.GAP.idx <- which((move.GAP$Raw < move.GAP$Mean - z*move.GAP$SD | move.GAP$Raw > move.GAP$Mean + z*move.GAP$SD) & !is.na(move.GAP$Mean) )

#### Global Reactive Power anomalies
anomaly.GRP <- move.GRP$Raw[( ((move.GRP$Raw < move.GRP$Mean - z*move.GRP$SD) | (move.GRP$Raw > move.GRP$Mean + z*move.GRP$SD)) & !is.na(move.GRP$Mean) )]
anomaly.GRP <- anomaly.GRP[!is.na(anomaly.GRP)]
## accumulate indices
anomaly.GRP.idx <- which((move.GRP$Raw < move.GRP$Mean - z*move.GRP$SD | move.GRP$Raw > move.GRP$Mean + z*move.GRP$SD) & !is.na(move.GRP$Mean) )

#### Voltage anomalies
anomaly.V <- move.V$Raw[( ((move.V$Raw < move.V$Mean - z*move.V$SD) | (move.V$Raw > move.V$Mean + z*move.V$SD)) & !is.na(move.V$Mean) )]
anomaly.V <- anomaly.V[!is.na(anomaly.V)]
## accumulate indices
anomaly.V.idx <- which((move.V$Raw < move.V$Mean - z*move.V$SD | move.V$Raw > move.V$Mean + z*move.V$SD) & !is.na(move.V$Mean) )

#### Global Intensity anomalies
anomaly.GI <- move.GI$Raw[( ((move.GI$Raw < move.GI$Mean - z*move.GI$SD) | (move.GI$Raw > move.GI$Mean + z*move.GI$SD)) & !is.na(move.GI$Mean) )]
anomaly.GI <- anomaly.GI[!is.na(anomaly.GI)]
## accumulate indices
anomaly.GI.idx <- which((move.GI$Raw < move.GI$Mean - z*move.GI$SD | move.GI$Raw > move.GI$Mean + z*move.GI$SD) & !is.na(move.GI$Mean) )


## Plot them mofos
#### GAP
layout(1)
plot(
  move.GAP$Raw, main='Moving average: Global Active Power Anomalies', col='black', pch=20,
  lwd=1, xlab='Time', xaxt='n', ylab='Global Active Power', 
  panel.first = grid(NULL, NULL, lwd=1, col='gray')
)
points(anomaly.GAP.idx, anomaly.GAP, col='dodgerblue2', pch=20, lwd=1)
legend('topleft', c('Anomalies','Normal'), cex=0.7, pch=c(20,20), col=c('dodgerblue2','black'))

#### GRP
layout(1)
plot(
  move.GRP$Raw, main='Moving average: Global Reactive Power Anomalies', col='black', pch=20,
  lwd=1, xlab='Time', xaxt='n', ylab='Global Reactive Power', 
  panel.first = grid(NULL, NULL, lwd=1, col='gray')
)
points(anomaly.GRP.idx, anomaly.GRP, col='red', pch=20, lwd=1)
legend('topleft', c('Anomalies','Normal'), cex=0.7, pch=c(20,20), col=c('red','black'))

#### V
layout(1)
plot(
  move.V$Raw, main='Moving average: Voltage Anomalies', col='black', pch=20,
  lwd=1, xlab='Time', xaxt='n', ylab='Voltage', 
  panel.first = grid(NULL, NULL, lwd=1, col='gray')
)
points(anomaly.V.idx, anomaly.V, col='darkorange3', pch=20, lwd=1)
legend('bottomleft', c('Anomalies','Normal'), cex=0.7, pch=c(20,20), col=c('darkorange3','black'))

#### GI
layout(1)
plot(
  move.GI$Raw, main='Moving average: Global Intensity Anomalies', col='black', pch=20,
  lwd=1, xlab='Time', xaxt='n', ylab='Global Intensity', 
  panel.first = grid(NULL, NULL, lwd=1, col='gray')
)
points(anomaly.GI.idx, anomaly.GI, col='mediumvioletred', pch=20, lwd=1)
legend('topleft', c('Anomalies','Normal'), cex=0.7, pch=c(20,20), col=c('mediumvioletred','black'))

