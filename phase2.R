library(lubridate)
library(TTR)

ds <- data
test <- na.omit(
          read.csv("C:/Users/Sidd/Desktop/github/cmpt318-term-project/test_data/test1.txt")
          #  read.csv('U:/cmpt318/test_data/test1.txt')
          #  read.csv('~/git/cmpt318-term-project/test_data/test1.txt')
        );

### APROACH 1

###########################################
###########   CHARACTERISTIC 1   ##########
###########################################

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
  m <- test[month(test$Date)==i & test$Global_active_power >= 0, ]
  
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
    col='black', lwd=1, xlab=paste('Minutes of ',month.name(i), sep=' '), xaxt='n', ylab='Global Active Power',
    panel.first=grid(NULL,NULL,lwd=1,col='gray'), pch=20
  )
  points(below.idx,below, col='red', pch=20, lwd=1)
  points(above.idx,above, col='chocolate2', pch=20, lwd=2)
  legend('topleft', c('Above','Normal','Below'), cex=0.7, col=c('chocolate2','black','red'),pch=c(20,20,20))
}

############################################
###########   CHARACTERISTIC 2   ###########
############################################

z <- 3
# The z score threshold
# A data value is an anomaly if its mean is not in [mean-z*SD, mean+z*SD]
# Can be changed to modify sensitivity to point anomalies

for (i in 1:12) {
  # get raw data for month `i`
  raw.data <- data.frame(Raw = test$Global_active_power[test$Global_active_power>=0 & month(test$Date)==i])
  
  # find moving averages
  raw.data$Mean <- runMean(raw.data$Raw, n=20)
  
  # find the moving SD's
  raw.data$SD <- runSD(raw.data$Raw, n=20)
  
  # find anomalies
  anomalies <- raw.data$Raw[ ((raw.data$Raw < raw.data$Mean - z*raw.data$SD) | (raw.data$Raw > raw.data$Mean + z*raw.data$SD)) & !is.na(raw.data$Mean)]
  anomalies <- anomalies[!is.na(anomalies)]
  
  # find anomalous indices
  anomalies.idx <- which(((raw.data$Raw < raw.data$Mean - z*raw.data$SD) | (raw.data$Raw > raw.data$Mean + z*raw.data$SD)) & !is.na(raw.data$Mean))
  
  # plot the moving average curve
  plot(
    raw.data$Mean, type='l', lwd=2, col='red', xlab='Minutes', ylab='Global Active Power', 
    main=paste('Moving average curve:', month.name(i), sep=' '),
    panel.first= grid(NULL,NULL, lwd=1,col='gray')
  )
  
  # plot the moving SD curve
  plot(
    raw.data$SD, type='l', lwd=2, col='chartreuse4', xlab='Minutes', ylab='Global Active Power',
    main=paste('Moving SD curve:', month.name(i), sep=' '),
    panel.first= grid(NULL,NULL, lwd=1,col='gray')
  )
  
  # plot them anomalies
  plot(
    raw.data$Raw, main=paste('Moving average Anomalies:',month.name(i), sep=' '), col='black', pch=20,
    lwd=1, xlab='Minutes', xaxt='n', ylab='Global Active Power', 
    panel.first = grid(NULL, NULL, lwd=1, col='gray')
  )
  points(anomalies.idx, anomalies, col='chocolate2', lwd=2)
  legend('topleft', c('Anomalies','Normal'), cex=0.65, pch=c(20,20), col=c('red','black'))
  
}
