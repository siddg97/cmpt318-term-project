library(lubridate)
library(depmixS4)
library(TTR)


## function to set up test data set 1 for `day` from time `start` to `end`
init.t1 <- function(day, start, end) {
  ### TEST SET 1
  raw <- read.csv("C:/Users/Sidd/Desktop/github/cmpt318-term-project/test_data/test1.txt")
         #  read.csv('U:/cmpt318/test_data/test1.txt')
         #  read.csv('~/git/cmpt318-term-project/test_data/test1.txt')
  
  ## FILTER FOR `NA`
  td <- raw[!is.na(raw$Global_active_power) & !is.na(raw$Global_reactive_power) 
            & !is.na(raw$Global_intensity) & !is.na(raw$Voltage),]
  
  ## FILTER FOR "day" from "start" to "end" times
  doi <- td[(as.POSIXlt(td$Date, format='%d/%m/%Y')$wday==day 
             & hour(as.POSIXlt(td$Time, format='%H:%M:%S'))>=start 
             & hour(as.POSIXlt(td$Time, format='%H:%M:%S'))<=end),]
  return(doi)
}

## function to set up test data set 2 for `day` from time `start` to `end`
init.t2 <- function(day, start, end){
  ### TEST SET 2
  raw <- read.csv("C:/Users/Sidd/Desktop/github/cmpt318-term-project/test_data/test2.txt")
  #  read.csv('U:/cmpt318/test_data/test2.txt')
  #  read.csv('~/git/cmpt318-term-project/test_data/test2.txt')
  
  ## FILTER FOR `NA`
  td <- raw[!is.na(raw$Global_active_power) & !is.na(raw$Global_reactive_power) 
            & !is.na(raw$Global_intensity) & !is.na(raw$Voltage),]
  
  ## FILTER FOR "day" from "start" to "end" times
  doi <- td[(as.POSIXlt(td$Date, format='%d/%m/%Y')$wday==day 
             & hour(as.POSIXlt(td$Time, format='%H:%M:%S'))>=start 
             & hour(as.POSIXlt(td$Time, format='%H:%M:%S'))<=end),]
 return(doi) 
}

## function to set up test data set 3 for `day` from time `start` to `end`
init.t3 <- function(day, start, end){
  ### TEST SET 3
  raw <- read.csv("C:/Users/Sidd/Desktop/github/cmpt318-term-project/test_data/test3.txt")
  #  read.csv('U:/cmpt318/test_data/test3.txt')
  #  read.csv('~/git/cmpt318-term-project/test_data/test3.txt')
  
  ## FILTER FOR `NA`
  td <- raw[!is.na(raw$Global_active_power) & !is.na(raw$Global_reactive_power) 
            & !is.na(raw$Global_intensity) & !is.na(raw$Voltage),]
  
  ## FILTER FOR "day" from "start" to "end" times
  doi <- td[(as.POSIXlt(td$Date, format='%d/%m/%Y')$wday==day 
                & hour(as.POSIXlt(td$Time, format='%H:%M:%S'))>=start 
                & hour(as.POSIXlt(td$Time, format='%H:%M:%S'))<=end),]
  return(doi)
}

## function to set up test data set 4 for `day` from time `start` to `end`
init.t4 <- function(day, start, end){ 
  ### TEST SET 4
  raw <- read.csv("C:/Users/Sidd/Desktop/github/cmpt318-term-project/test_data/test4.txt")
  #  read.csv('U:/cmpt318/test_data/test4.txt')
  #  read.csv('~/git/cmpt318-term-project/test_data/test4.txt')
  
  ## FILTER FOR `NA`
  td <- raw[!is.na(raw$Global_active_power) & !is.na(raw$Global_reactive_power) 
            & !is.na(raw$Global_intensity) & !is.na(raw$Voltage),]
  
  ## FILTER FOR "day" from "start" to "end" times
  doi <- td[(as.POSIXlt(td$Date, format='%d/%m/%Y')$wday==day 
                & hour(as.POSIXlt(td$Time, format='%H:%M:%S'))>=start 
                & hour(as.POSIXlt(td$Time, format='%H:%M:%S'))<=end),]
  return(doi)
}

## function to set up test data set 5 for `day` from time `start` to `end`
init.t5 <- function(day, start, end){
  ### TEST SET 5
  raw <- read.csv("C:/Users/Sidd/Desktop/github/cmpt318-term-project/test_data/test5.txt")
  #  read.csv('U:/cmpt318/test_data/test5.txt')
  #  read.csv('~/git/cmpt318-term-project/test_data/test5.txt')
  
  ## FILTER FOR `NA`
  td <- raw[!is.na(raw$Global_active_power) & !is.na(raw$Global_reactive_power) 
            & !is.na(raw$Global_intensity) & !is.na(raw$Voltage),]
  
  ## FILTER FOR "day" from "start" to "end" times
  doi <- td[(as.POSIXlt(td$Date, format='%d/%m/%Y')$wday==day 
                & hour(as.POSIXlt(td$Time, format='%H:%M:%S'))>=start 
                & hour(as.POSIXlt(td$Time, format='%H:%M:%S'))<=end),]
  return(doi)
}


train.data <- data

test1 <- init.t1(3,16,18)
test2 <- init.t2(3,16,18)
test3 <- init.t3(3,16,18)
test4 <- init.t4(3,16,18)
test5 <- init.t5(3,16,18)



#           _____  _____  _____   ____          _____ _    _       __ 
#     /\   |  __ \|  __ \|  __ \ / __ \   /\   / ____| |  | |  _  /_ |
#    /  \  | |__) | |__) | |__) | |  | | /  \ | |    | |__| | (_)  | |
#   / /\ \ |  ___/|  ___/|  _  /| |  | |/ /\ \| |    |  __  |      | |
#  / ____ \| |    | |    | | \ \| |__| / ____ \ |____| |  | |  _   | |
# /_/    \_\_|    |_|    |_|  \_\\____/_/    \_\_____|_|  |_| (_)  |_|


###########################################
###########   CHARACTERISTIC 1   ##########
###########################################

## [TRAINING DATA] Monthly minimums for wednesday evenings for GAP, V and GI
month.mins <- aggregate(
                  list(GAP = train.data$Global_active_power, V = train.data$Voltage, GI = train.data$Global_intensity), 
                  by= list(Month = month(as.Date(train.data$Date, format='%d/%m/%Y'))), 
                  FUN = min
              )

## [TRAINING DATA] Monthly maximums for wednesday evenings for GAP, V and GI
month.maxs <- aggregate(
                  list(GAP = train.data$Global_active_power, V = train.data$Voltage, GI = train.data$Global_intensity), 
                  by= list(Month = month(as.Date(train.data$Date, format='%d/%m/%Y'))), 
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


## plot point-anomalies for each month GAP for test set `test1`
for (i in 1:12) {
  ## Get raw month data
  m <- test1[month(test1$Date)==i & test1$Global_active_power >= 0, ]
  
  ## Find points below min and their indices
  below <- m$Global_active_power[m$Global_active_power < month.mins$GAP[i]]
  below.idx <- which(m$Global_active_power < month.mins$GAP[i])
  
  ## Find points above max and their indices
  above <- m$Global_active_power[m$Global_active_power > month.maxs$GAP[i]]
  above.idx <- which(m$Global_active_power > month.maxs$GAP[i])
  
  ## Plot them mofos
  layout(1)
  plot(
    m$Global_active_power, main=paste('Anomalies for Testing Set #1:',month.name(i), sep=' '),
    col='#4363d8', lwd=2, xlab=paste('Minutes of ',month.name(i), sep=' '), xaxt='n', ylab='Global Active Power',
    panel.first=grid(NULL,NULL,lwd=1,col='gray')
  )
  lines(below.idx,below, col='#800000', lwd=2, type='b')
  lines(above.idx,above, col='#E6194B', lwd=2, type='b')
  legend('topleft', c('Above','Normal','Below'), cex=0.5, col=c('#E6194B','#4363d8','#800000'), lwd=2)
}


## plot point-anomalies for each month GAP for test set `test2`
for (i in 1:12) {
  ## Get raw month data
  m <- test2[month(test2$Date)==i & test2$Global_active_power >= 0, ]
  
  ## Find points below min and their indices
  below <- m$Global_active_power[m$Global_active_power < month.mins$GAP[i]]
  below.idx <- which(m$Global_active_power < month.mins$GAP[i])
  
  ## Find points above max and their indices
  above <- m$Global_active_power[m$Global_active_power > month.maxs$GAP[i]]
  above.idx <- which(m$Global_active_power > month.maxs$GAP[i])
  
  ## Plot them mofos
  layout(1)
  plot(
    m$Global_active_power, main=paste('Anomalies for Testing Set #2:',month.name(i), sep=' '),
    col='#4363d8', lwd=2, xlab=paste('Minutes of ',month.name(i), sep=' '), xaxt='n', ylab='Global Active Power',
    panel.first=grid(NULL,NULL,lwd=1,col='gray')
  )
  lines(below.idx,below, col='#800000', lwd=2, type='b')
  lines(above.idx,above, col='#E6194B', lwd=2, type='b')
  legend('topleft', c('Above','Normal','Below'), cex=0.5, col=c('#E6194B','#4363d8','#800000'), lwd=2)
}


## plot point-anomalies for each month GAP for test set `test3`
for (i in 1:12) {
  ## Get raw month data
  m <- test3[month(test3$Date)==i & test3$Global_active_power >= 0, ]
  
  ## Find points below min and their indices
  below <- m$Global_active_power[m$Global_active_power < month.mins$GAP[i]]
  below.idx <- which(m$Global_active_power < month.mins$GAP[i])
  
  ## Find points above max and their indices
  above <- m$Global_active_power[m$Global_active_power > month.maxs$GAP[i]]
  above.idx <- which(m$Global_active_power > month.maxs$GAP[i])
  
  ## Plot them mofos
  layout(1)
  plot(
    m$Global_active_power, main=paste('Anomalies for Testing Set #3:',month.name(i), sep=' '),
    col='#4363d8', lwd=2, xlab=paste('Minutes of ',month.name(i), sep=' '), xaxt='n', ylab='Global Active Power',
    panel.first=grid(NULL,NULL,lwd=1,col='gray')
  )
  lines(below.idx,below, col='#800000', lwd=2, type='b')
  lines(above.idx,above, col='#E6194B', lwd=2, type='b')
  legend('topleft', c('Above','Normal','Below'), cex=0.5, col=c('#E6194B','#4363d8','#800000'), lwd=2)
}


## plot point-anomalies for each month GAP for test set `test4`
for (i in 1:12) {
  ## Get raw month data
  m <- test4[month(test4$Date)==i & test4$Global_active_power >= 0, ]
  
  ## Find points below min and their indices
  below <- m$Global_active_power[m$Global_active_power < month.mins$GAP[i]]
  below.idx <- which(m$Global_active_power < month.mins$GAP[i])
  
  ## Find points above max and their indices
  above <- m$Global_active_power[m$Global_active_power > month.maxs$GAP[i]]
  above.idx <- which(m$Global_active_power > month.maxs$GAP[i])
  
  ## Plot them mofos
  layout(1)
  plot(
    m$Global_active_power, main=paste('Anomalies for Testing Set #4:',month.name(i), sep=' '),
    col='#4363d8', lwd=2, xlab=paste('Minutes of ',month.name(i), sep=' '), xaxt='n', ylab='Global Active Power',
    panel.first=grid(NULL,NULL,lwd=1,col='gray')
  )
  lines(below.idx,below, col='#800000', lwd=2, type='b')
  lines(above.idx,above, col='#E6194B', lwd=2, type='b')
  legend('topleft', c('Above','Normal','Below'), cex=0.5, col=c('#E6194B','#4363d8','#800000'), lwd=2)
}


## plot point-anomalies for each month GAP for test set `test5`
for (i in 1:12) {
  ## Get raw month data
  m <- test5[month(test5$Date)==i & test5$Global_active_power >= 0, ]
  
  ## Find points below min and their indices
  below <- m$Global_active_power[m$Global_active_power < month.mins$GAP[i]]
  below.idx <- which(m$Global_active_power < month.mins$GAP[i])
  
  ## Find points above max and their indices
  above <- m$Global_active_power[m$Global_active_power > month.maxs$GAP[i]]
  above.idx <- which(m$Global_active_power > month.maxs$GAP[i])
  
  ## Plot them mofos
  layout(1)
  plot(
    m$Global_active_power, main=paste('Anomalies for Testing Set #5:',month.name(i), sep=' '),
    col='#4363d8', lwd=2, xlab=paste('Minutes of ',month.name(i), sep=' '), xaxt='n', ylab='Global Active Power',
    panel.first=grid(NULL,NULL,lwd=1,col='gray')
  )
  lines(below.idx,below, col='#800000', lwd=2, type='b')
  lines(above.idx,above, col='#E6194B', lwd=2, type='b')
  legend('topleft', c('Above','Normal','Below'), cex=0.5, col=c('#E6194B','#4363d8','#800000'), lwd=2)
}


############################################
###########   CHARACTERISTIC 2   ###########
############################################

z <- 3
# The z score threshold
# A data value is an anomaly if its mean is not in [mean-z*SD, mean+z*SD]
# Can be changed to modify sensitivity to point anomalies


## plot moving average anomalies for `test1`
for (i in 1:12) {
  # get raw data for month `i`
  raw.data <- data.frame(Raw = test1$Global_active_power[test1$Global_active_power>=0 & month(test1$Date)==i])
  
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
    main=paste('Test Set #1: Moving average curve -', month.name(i), sep=' '),
    panel.first= grid(NULL,NULL, lwd=1,col='gray')
  )
  
  # plot the moving SD curve
  plot(
    raw.data$SD, type='l', lwd=2, col='chartreuse4', xlab='Minutes', ylab='Global Active Power',
    main=paste('Test Set #1: Moving SD curve -', month.name(i), sep=' '),
    panel.first= grid(NULL,NULL, lwd=1,col='gray')
  )
  
  # plot them anomalies
  plot(
    raw.data$Raw, main=paste('Test Set #1: Moving average Anomalies -',month.name(i), sep=' '), 
    col='#F58231', pch=20, lwd=1, xlab='Minutes', xaxt='n', ylab='Global Active Power', 
    panel.first = grid(NULL, NULL, lwd=1, col='gray')
  )
  lines(anomalies.idx, anomalies, col='black', lwd=2, type='b')
  legend('topleft', c('Normal','Anomalies'), cex=0.5, pch=c(20,20), col=c('#F58231','black'))
}


## plot moving average anomalies for `test2`
for (i in 1:12) {
  # get raw data for month `i`
  raw.data <- data.frame(Raw = test2$Global_active_power[test2$Global_active_power>=0 & month(test2$Date)==i])
  
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
    main=paste('Test Set #2: Moving average curve -', month.name(i), sep=' '),
    panel.first= grid(NULL,NULL, lwd=1,col='gray')
  )
  
  # plot the moving SD curve
  plot(
    raw.data$SD, type='l', lwd=2, col='chartreuse4', xlab='Minutes', ylab='Global Active Power',
    main=paste('Test Set #2: Moving SD curve -', month.name(i), sep=' '),
    panel.first= grid(NULL,NULL, lwd=1,col='gray')
  )
  
  # plot them anomalies
  plot(
    raw.data$Raw, main=paste('Test Set #2: Moving average Anomalies -',month.name(i), sep=' '), col='#F58231', pch=20,
    lwd=1, xlab='Minutes', xaxt='n', ylab='Global Active Power', 
    panel.first = grid(NULL, NULL, lwd=1, col='gray')
  )
  lines(anomalies.idx, anomalies, col='black', lwd=2, type='b')
  legend('topleft', c('Normal','Anomalies'), cex=0.5, pch=c(20,20), col=c('#F58231','black'))
}


## plot moving average anomalies for `test3`
for (i in 1:12) {
  # get raw data for month `i`
  raw.data <- data.frame(Raw = test3$Global_active_power[test3$Global_active_power>=0 & month(test3$Date)==i])
  
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
    main=paste('Test Set #3: Moving average curve -', month.name(i), sep=' '),
    panel.first= grid(NULL,NULL, lwd=1,col='gray')
  )
  
  # plot the moving SD curve
  plot(
    raw.data$SD, type='l', lwd=2, col='chartreuse4', xlab='Minutes', ylab='Global Active Power',
    main=paste('Test Set #3: Moving SD curve -', month.name(i), sep=' '),
    panel.first= grid(NULL,NULL, lwd=1,col='gray')
  )
  
  # plot them anomalies
  plot(
    raw.data$Raw, main=paste('Test Set #3: Moving average Anomalies -',month.name(i), sep=' '), col='#F58231', pch=20,
    lwd=1, xlab='Minutes', xaxt='n', ylab='Global Active Power', 
    panel.first = grid(NULL, NULL, lwd=1, col='gray')
  )
  lines(anomalies.idx, anomalies, col='black', lwd=2, type='b')
  legend('topleft', c('Normal','Anomalies'), cex=0.5, pch=c(20,20), col=c('#F58231','black'))
}


## plot moving average anomalies for `test4`
for (i in 1:12) {
  # get raw data for month `i`
  raw.data <- data.frame(Raw = test4$Global_active_power[test4$Global_active_power>=0 & month(test4$Date)==i])
  
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
    main=paste('Test Set #4: Moving average curve -', month.name(i), sep=' '),
    panel.first= grid(NULL,NULL, lwd=1,col='gray')
  )
  
  # plot the moving SD curve
  plot(
    raw.data$SD, type='l', lwd=2, col='chartreuse4', xlab='Minutes', ylab='Global Active Power',
    main=paste('Test Set #4: Moving SD curve -', month.name(i), sep=' '),
    panel.first= grid(NULL,NULL, lwd=1,col='gray')
  )
  
  # plot them anomalies
  plot(
    raw.data$Raw, main=paste('Test Set #4: Moving average Anomalies -',month.name(i), sep=' '), col='#F58231', pch=20,
    lwd=1, xlab='Minutes', xaxt='n', ylab='Global Active Power', 
    panel.first = grid(NULL, NULL, lwd=1, col='gray')
  )
  lines(anomalies.idx, anomalies, col='black', lwd=2, type='b')
  legend('topleft', c('Normal','Anomalies'), cex=0.5, pch=c(20,20), col=c('#F58231','black'))
}


## plot moving average anomalies for `test5`
for (i in 1:12) {
  # get raw data for month `i`
  raw.data <- data.frame(Raw = test5$Global_active_power[test5$Global_active_power>=0 & month(test5$Date)==i])
  
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
    main=paste('Test Set #5: Moving average curve -', month.name(i), sep=' '),
    panel.first= grid(NULL,NULL, lwd=1,col='gray')
  )
  
  # plot the moving SD curve
  plot(
    raw.data$SD, type='l', lwd=2, col='chartreuse4', xlab='Minutes', ylab='Global Active Power',
    main=paste('Test Set #5: Moving SD curve -', month.name(i), sep=' '),
    panel.first= grid(NULL,NULL, lwd=1,col='gray')
  )
  
  # plot them anomalies
  plot(
    raw.data$Raw, main=paste('Test Set #5: Moving average Anomalies -',month.name(i), sep=' '), col='#F58231', pch=20,
    lwd=1, xlab='Minutes', xaxt='n', ylab='Global Active Power', 
    panel.first = grid(NULL, NULL, lwd=1, col='gray')
  )
  lines(anomalies.idx, anomalies, col='black', lwd=2, type='b')
  legend('topleft', c('Normal','Anomalies'), cex=0.5, pch=c(20,20), col=c('#F58231','black'))
}



#           _____  _____  _____   ____          _____ _    _       ___  
#     /\   |  __ \|  __ \|  __ \ / __ \   /\   / ____| |  | |  _  |__ \ 
#    /  \  | |__) | |__) | |__) | |  | | /  \ | |    | |__| | (_)    ) |
#   / /\ \ |  ___/|  ___/|  _  /| |  | |/ /\ \| |    |  __  |       / / 
#  / ____ \| |    | |    | | \ \| |__| / ____ \ |____| |  | |  _   / /_ 
# /_/    \_\_|    |_|    |_|  \_\\____/_/    \_\_____|_|  |_| (_) |____|


###############################
######### DATA VARS:###########
###############################
## +> train.data <- data
## +> test1 <- init.t1(3,16,18)
## +> test2 <- init.t2(3,16,18)
## +> test3 <- init.t3(3,16,18)
## +> test4 <- init.t4(3,16,18)
## +> test5 <- init.t5(3,16,18)

# Train the initial model and fit it
train.model <- function(model.data,n){
  # get the all global active power and date
  training.entries <- model.data[c("Date","Global_active_power")]
  # get count of entries according year and day 
  num.entries <- aggregate(training.entries$Date, by=list(Year=year(training.entries$Date), Day=day(training.entries$Date)), FUN=length)
  # order the entries according to year
  num.entries <- num.entries[order(num.entries$Year),]
  # set count column name
  colnames(num.entries) <- c('Year','Day','Count')
  # extract the count column
  num.entries <- num.entries[,3]
  
  set.seed(1)
  model = depmix(
            response=Global_active_power~1, data=model.data, 
            nstates=n, family=gaussian("identity"), ntimes=num.entries
          )
  fit.m = fit(model)
  return(fit.m)
}

# Train the intial multivariate model and fit it
train.multi.model <- function(model.data,n) {
  # get the all global active power and date
  training.entries <- model.data[c("Date","Global_active_power")]
  # get count of entries according year and day 
  num.entries <- aggregate(training.entries$Date, by=list(Year=year(training.entries$Date), Day=day(training.entries$Date)), FUN=length)
  # order the entries according to year
  num.entries <- num.entries[order(num.entries$Year),]
  # set count column name
  colnames(num.entries) <- c('Year','Day','Count')
  # extract the count column
  num.entries <- num.entries[,3]
  
  set.seed(1)
  model = depmix(
    response=list(Global_active_power~1, Global_intensity~1), 
    data=model.data, nstates=n, ntimes=num.entries,
    family=list(gaussian("identity"), gaussian("identity"))
  )
  fit.m = fit(model)
  return(fit.m)
}


# Test the trained model for the given test data set
test.model <- function(trained.model, test.data, n) {
  # get all global active power and date
  testing.entries <- test.data[c("Date","Global_active_power")]
  # get count of entries according to year and day
  num.entries <- aggregate(
                    testing.entries$Date, 
                    by=list(Year=year(testing.entries$Date), Day=day(testing.entries$Date)), 
                    FUN=length
                 )
  # order the entries according to year
  num.entries <- num.entries[order(num.entries$Year),]
  # set count column name
  colnames(num.entries) <- c("Year","Day","Count")
  # extract the count column
  num.entries <- num.entries[,3]
  
  set.seed(1)
  model <- depmix(
              response=Global_active_power~1, data=test.data,
              nstates=n, ntime=num.entries, family=gaussian("identity")
           )
  model <- setpars(model, getpars(trained.model))
  fb <- forwardbackward(model)
  
  return(fb$logLike)
}

# Test the trained multivariate model for the given test data set
test.multi.model <- function(trained.model, test.data, n) {
  # get all global active power and date
  testing.entries <- test.data[c("Date","Global_active_power")]
  # get count of entries according to year and day
  num.entries <- aggregate(
    testing.entries$Date, 
    by=list(Year=year(testing.entries$Date), Day=day(testing.entries$Date)), 
    FUN=length
  )
  # order the entries according to year
  num.entries <- num.entries[order(num.entries$Year),]
  # set count column name
  colnames(num.entries) <- c("Year","Day","Count")
  # extract the count column
  num.entries <- num.entries[,3]
  
  set.seed(1)
  model <- depmix(
    response=list(Global_active_power~1, Global_intensity~1), 
    family=list(gaussian("identity"), gaussian("identity")), 
    data=test.data, nstates=n, ntime=num.entries
  )
  model <- setpars(model, getpars(trained.model))
  fb <- forwardbackward(model)
  
  return(fb$logLike)
}


## Find optimal nstates and plot
plot.bic.state <- function(model.data, min.s, max.s, is.multi) {
  bic <- c()
  ll <- c()
  index <- 1
  model <- NULL
  
  for (i in min.s:max.s) {
    if(is.multi){
      model <- train.model(model.data, i)
    } else {
      model <- train.multi.model(model.data, i)
    }
    bic[index] <- BIC(model)
    ll[index] <- logLik(model)
    index <- index+1
  }
  ## Now, bic and ll have values indexed by number of states.
  ## We plot them to analyse the optimal nstates for our training model
  layout(1)
  plot( #### BIC PLOT
    min.s:max.s, bic, type='l', col='#911EB4', lwd=3,
    xlab='# of states [nstates]', ylab='BIC',
    main=ifelse(is.multi,'BIC vs. nstates - Multivariate','BIC vs. nstates - Univariate'),
    panel.first= grid(NULL,NULL,lwd=1, col='gray') 
  )
  layout(1)
  plot( #### LL PLOT
    min.s:max.s, ll, type='l', col='#000075', lwd=3,
    xlab='# of states [nstates]', ylab='Log-likelihood', 
    main=ifelse(is.multi,'Log-likelihood vs. nstates - Multivariate','Lol-likelihood vs. nstates - Univariate'),
    panel.first=grid(NULL,NULL,lwd=1,col='gray')
  )
  return(data.frame(BIC=bic, LL=ll, N=min.s:max.s))
}




## get plot of BIC and logLike vs nstates for univariate HMM
ns.df       <- plot.bic.state(train.data, 2, 30, is.multi=FALSE)
## get plot of BIC and logLike vs nstates for multivariate HMM
ns.df.multi <- plot.bic.state(train.data, 2, 30, is.multi=TRUE)

### NEED TO SET THIS ACCORDING TO plots obtained 
### from the above 2 calls to plot.bic.state() 
op.states <- 10

## Train model using the optimal nstate value
trained.mod       <- train.model(train.data, n=op.states)
## Train multivariate using the optimal nstate value
trained.multi.mod <- train.multi.model(train.data, n=op.states)

## test all 5 test data sets for univariate
t1.ll <- test.model(trained.mod, test1, op.states)
t2.ll <- test.model(trained.mod, test2, op.states)
t3.ll <- test.model(trained.mod, test3, op.states)
t4.ll <- test.model(trained.mod, test4, op.states)
t5.ll <- test.model(trained.mod, test5, op.states)

## test all 5 test data sets for multivariate
t1.m.ll <- test.multi.model(trained.multi.mod, test1, op.states)
t2.m.ll <- test.multi.model(trained.multi.mod, test2, op.states)
t3.m.ll <- test.multi.model(trained.multi.mod, test3, op.states)
t4.m.ll <- test.multi.model(trained.multi.mod, test4, op.states)
t5.m.ll <- test.multi.model(trained.multi.mod, test5, op.states)
