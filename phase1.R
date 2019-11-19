library(lubridate);

#### function to set up training data. filter for NAs and wednedsday time intervals
init <- function(day, start, end) {
  raw <- read.csv("~/git/cmpt318-term-project/TrainData.txt")
  # read.csv("C:/Users/sga94/Desktop/cmpt318-term-project-master/TrainData.txt")
  ## FILTER FOR `NA`
  td <- raw[!is.na(raw$Global_active_power) & !is.na(raw$Global_reactive_power) 
             & !is.na(raw$Global_intensity) & !is.na(raw$Voltage),]
  
  ## FILTER FOR "day" from "start" to "end" times
  doi <- td[(as.POSIXlt(td$Date, format='%d/%m/%Y')$wday==day 
             & hour(as.POSIXlt(td$Time, format='%H:%M:%S'))>=start 
             & hour(as.POSIXlt(td$Time, format='%H:%M:%S'))<=end),]
  
  return(doi)
}


# Converts dates to the season they are in
season <- function(dates) {
  winterEnd <- 79 # March 20
  springEnd <- 171 # June 21
  summerEnd <- 266 # September 23
  fallEnd   <- 355 # December 21
  
  days <- yday(as.POSIXlt(dates, format="%d/%m/%Y"))
  
  ifelse(days < winterEnd, "Winter",
         ifelse(days < springEnd, "Spring",
                ifelse(days < summerEnd, "Summer",
                       ifelse(days < fallEnd, "Fall", "Winter")
                )
          )
  )
}


## function to convert numeric time to clock time
time.to.str <- function(time.num) {
  h <- time.num %/% 60
  m <- time.num %% 60
  if(0<=m & m<=9) {
    return(paste(h,paste('0',m,sep=''),sep=':'));
  } else {
    return(paste(h,m,sep=':'));
  }
}

#### function to get average GAP plot for each minute
plot.minute <- function(data,s,e) {
  avg.min <- c()
  times   <- c()
  for (i in s:e) {
    t <- data[as.numeric(data$Time)==i,]
    avg.min <- c(avg.min, mean(t$Global_active_power))
  }
  for (i in seq(s,e,10)) {
    times <- c(times, time.to.str(i))
  }
  layout(1)
  plot(
    s:e, avg.min, col='darkred', type='o', xlab='Time [24 hour clock]', xaxt='n',
    ylab='Global Active Power', main='Average for each minute',
    lwd=2, panel.first = grid(NULL,NULL,lwd=1,col='gray')
  )
  axis(side=1, at=seq(s,e,10), labels=times)
  return(avg.min)
}


#### function to get stuff for each week and plot
explore.week <- function(data) {
  ## FOR MIN, MAX AND MEAN
  mean.week <- aggregate(data$Global_active_power, by=list(week(as.Date(data$Date, format='%d/%m/%Y'))), mean)
  names(mean.week) <- c("Week", "Global_active_power")
  
  ## FOR S.D.
  sd.week <- aggregate(data$Global_active_power, by=list(week(as.Date(data$Date, format='%d/%m/%Y'))), sd)
  
  ## PLOTS
  layout(1:2)
  plot(
    mean.week, type='o', col='red3', lwd=3, 
    main='Average for each week', xlab='Week', ylab='Global Active Power',
    panel.first = grid(NULL,NULL,lwd=1,col='gray'), xaxt='n'
  )
  axis(side=1, at=1:53)
  plot(
    sd.week, type='o', col='chartreuse4', lwd=3, 
    main='Standard Deviation for each week', xlab='Week', ylab='Standard deviation',
    panel.first = grid(NULL,NULL,lwd=1,col='gray'), xaxt='n'
  )
  axis(side=1, at=1:53)
  return(c(max(mean.week$Global_active_power), min(mean.week$Global_active_power)))
}


#### function to get stuff for each month and plot
explore.month <- function(data) {
  ## FOR MIN, MAX AND MEAN
  mean.month <- aggregate(data$Global_active_power, by=list(month(as.Date(data$Date, format='%d/%m/%Y'))), mean)
  names(mean.month) <- c('Month','Global_active_power')
  
  ## FOR S.D.
  sd.month <- aggregate(data$Global_active_power, by=list(month(as.Date(data$Date, format='%d/%m/%Y'))), sd)
  
  ## PLOTS
  layout(1:2)
  plot(
    mean.month, type='o', col='royalblue3', lwd=3,
    main='Average for each month', xlab='Month', ylab='Global Active Power',
    panel.first = grid(NULL,NULL,lwd=1,col='gray'), xaxt='n'
  )
  axis(side=1, at=1:12, labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sept','Oct','Nov','Dec'))
  plot(
    sd.month, type='o', col='chartreuse4', lwd=3,
    main='Standard Deviation for each month', xlab='Month', ylab='Standard deviation',
    panel.first = grid(NULL,NULL,lwd=1,col='gray'), xaxt='n'
  )
  axis(side=1, at=1:12, labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sept','Oct','Nov','Dec'))

  return(c(max(mean.month$Global_active_power), min(mean.month$Global_active_power)))
}


#### function to get stuff for each season and plot
explore.season <- function(data) {
  ## FOR MIN, MAX AND MEAN
  mean.season <- aggregate(data$Global_active_power, by=list(season(data$Date)), mean)
  names(mean.season) <- c('Season','Global_active_power')
  
  ## FOR S.D.
  sd.season <- aggregate(data$Global_active_power, by=list(season(data$Date)), sd)
  names(sd.season) <- c('Season', 'Sd')
  ## PLOTS
  layout(1:2)
  plot(mean.season$Global_active_power, type='o', col='slateblue4', lwd=3, ylab='Global Active Power',
       xlab='',main='Average for each season', xaxt='n',
       panel.first = grid(NULL,NULL,lwd=1,col='gray') 
  )
  axis(side=1, at=1:4, labels=c('Fall','Spring','Summer','Winter'))
  plot(sd.season$Sd, type='o', col='chartreuse4', lwd=3, ylab='Standard deviation',
       xlab='',main='Standard Deviation for each season', xaxt='n',
       panel.first = grid(NULL,NULL,lwd=1,col='gray')
  )
  axis(side=1, at=1:4, labels=c('Fall','Spring','Summer','Winter'))
  
  return(c(max(mean.season$Global_active_power), min(mean.season$Global_active_power)))
}



##### Function to perform phase1 characteristic1 tasks
c1 <- function(data){
  
  ## plot minute avg
  minute.data <- plot.minute(data,960,1080)          # plot.minute(data, start_minutes, end_minutes)
  
  ## plot and find week data
  week.data   <- explore.week(data)   # week.data   = list(max,min)
  
  ## plot and find month data
  month.data  <- explore.month(data)  # month.data  = list(max,min)
  
  ## plot and find season data
  season.data <- explore.season(data) # season.data = list(max,min)
  x <- matrix(c(week.data, month.data, season.data), nrow=3, byrow=TRUE)
  rownames(x) <- c('Week', 'Month', 'Season')
  colnames(x) <- c('Max', 'Min')
  return(x)
  
}

##### Function to perform phase1 characteristic2 tasks
c2 <- function(data) {
  independent.features <- data[c(3,4,5,6)]
  mat_c <- cor(independent.features)
  ## extract cors
  gap <- c(mat_c[1,2], mat_c[1,3], mat_c[1,4])
  grp <- c(mat_c[2,1], mat_c[2,3], mat_c[2,4])
  v   <- c(mat_c[3,1], mat_c[3,2], mat_c[3,4])
  gi  <- c(mat_c[4,1], mat_c[4,2], mat_c[4,3])
  
  ## plot tcorrs for each feature
  layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))
  plot(
    1:3, gap, lwd=2, type='o', col='red3', xlab='', ylab='Correlation', 
    xaxt='n', main='Global Active Power Correlation', panel.first = grid(NULL,NULL,lwd=1,col='gray')
  )
  axis(side=1, at=1:3, labels= c('Global Reactive Power','Voltage','Global Intensity'))
  
  plot(
    1:3, grp, lwd=2, type='o', col='dodgerblue2', xlab='', ylab='Correlation', 
    xaxt='n', main='Global Reactive Power Correlation', panel.first = grid(NULL,NULL,lwd=1,col='gray')
  )
  axis(side=1, at=1:3, labels= c('Global Active Power','Voltage', 'Global Intensity'))
  
  plot(
    1:3, v, lwd=2, type='o', col='slateblue4', xlab='', ylab='Correlation', 
    xaxt='n', main='Voltage Correlation', panel.first = grid(NULL,NULL,lwd=1,col='gray')
  )
  axis(side=1, at=1:3, labels= c('Global Active Power','Global Reactive Power', 'Global Intensity'))
  
  plot(
    1:3, gi, lwd=2, type='o', col='chartreuse4', xlab='', ylab='Correlation', 
    xaxt='n', main='Global Intensity Correlation', panel.first = grid(NULL,NULL,lwd=1,col='gray')
  )
  axis(side=1, at=1:3, labels= c('Global Active Power', 'Global Reactive Power', 'Voltage'))
  return(mat_c)
}

## Import data
data <- init(3,16,18)               # init(day_number, start_time, end_time)
## perform c1 tasks
c1.result <- c1(data)
## perform c2 tasks
c2.result <- c2(data)
