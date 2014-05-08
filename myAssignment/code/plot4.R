require(sqldf)
require(lubridate)

# function to read file
get.data = function(){
  
  # get file from web and store in my.temp
  my.temp = tempfile()
  my.url = "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  download.file(my.url, my.temp)
  
  # use sql query to extract relevent data for project
  my.sql = "SELECT * from file WHERE Date == '1/2/2007' OR Date == '2/2/2007'"
  data = read.csv.sql(unzip(my.temp, "household_power_consumption.txt"), sql = my.sql, sep = ";")
  data$Date = as.Date(data$Date, "%d/%m/%Y")
  
  # tidy up and return data
  unlink(my.temp)
  return (data)
}

# function to compute indicies and day names of points in time when day changes
get.day.changes = function(date){
  date = as.POSIXct(date, format = "%d/%m/%Y")      # format
  date = c(date, date[length(date)] + days(1))      # make last element next day 
  ind = c(1, which(diff(date) > 0) + 1)             # find indices where day changes (include index 1)
  day = format(date[ind], "%a")                     # format days change time-dates as short day name
  return(data.frame(ind, day))
}

my.data = get.data()

# write plot to png
png(filename = "../figure/plot4.png", width = 480, height = 480, units = "px")

# create plot
with(my.data, {
  
    par(mfcol = c(2,2))
    my.ticks = get.day.changes(Date)  # get indicies and names of axis lables
  
    # plot 1
    plot(Global_active_power, type = "l", xaxt = "n", 
       ylab = "Global Active Power",
       xlab = "")
    axis(side = 1, at = my.ticks$ind, labels = my.ticks$day)
  
    # plot 2
    plot(Sub_metering_1, 
       type = "l", col = 1, xaxt = "n", 
       ylab = "Energy sub metering",
       xlab = "")
    lines(Sub_metering_2, col = 2)
    lines(Sub_metering_3, col = 4)
    legend("topright", 
         lty = 1,
         col = c(1,2,4),
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    axis(side = 1, at = my.ticks$ind, labels = my.ticks$day)
  
    # plot 3
    plot(Voltage, 
       type = "l", col = 1, xaxt = "n", 
       xlab = "datetime")
    axis(side = 1, at = my.ticks$ind, labels = my.ticks$day)
  
    # plot 4
    plot(Global_reactive_power, 
       type = "l", col = 1, xaxt = "n", 
       xlab = "datetime")
    axis(side = 1, at = my.ticks$ind, labels = my.ticks$day)
  }
)
dev.off()