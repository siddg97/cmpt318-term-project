library(lubridate);

t1 <- read.csv("U:/cmpt318/TestData/test1.txt")

wed.evenings <- t1[(as.POSIXlt(t1$Date, format='%d/%m/%Y')$wday==3) & 
				   (hour(as.POSIXlt(t1$Time, format='%H:%M:%S'))>=18) & 
				   	(hour(as.POSIXlt(t1$Time, format='%H:%M:%S'))<21), ]

# average for every minute
avg.wed.evenings <- aggregate(Global_active_power~Time, wed.evenings[,c(2,3)], mean)

get.season <- function (dates) {
	w.end <- 79
	s.end <- 171
	sum.end <- 266
	f.end <- 355
	
	days <- yday(as.POSIXlt(dates, format='%d/%m/%Y'))
	if(daya<w.end){
		return('Winter')
	} else if(days<s.end){
		return('Spring')
	} else if(days<sum.end){
		return('Summer')
	} else if(days<f.end){
		return('Fall')
	} else {
		return('Winter')
	}
}


# average of all weeks
avg.week.evening <- aggregate(wed.evenings$Global_active_power, by=list(week(as.Date(wed.evenings$Date, format='%d/%m/%Y'))), mean)
names(avg.week.evening) <- c('Week','Global_active_power')

# store min and max
max.week <- max(avg.week.evening$Global_active_power)
min.week <- min(avg.week.evening$Global_active_power)



# average of all months
avg.month.evening <- aggregate(wed.evenings$Global_active_power, by=list(month(as.Date(wed.evenings$Date, format='%d/%m/%Y'))), mean)
names(avg.month.evening) <- c('Month','Global_active_power')

# store min and max
max.month <- max(avg.month.evening$Global_active_power)
min.month <- min(avg.month.evening$Global_active_power)



# average of all seasons
avg.season.evening <- aggregate(wed.evenings$Global_active_power, by=list(get.season(as.Date(wed.evenings$Date, format='%d/%m/%Y'))), mean)
names(avg.season.evening) <- c('Season','Global_active_power')

# store min and max
max.season <- max(avg.season.evening$Global_active_power)
min.season <- min(avg.season.evening$Global_active_power)