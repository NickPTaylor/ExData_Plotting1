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
png(filename = "../figure/plot2.png", width = 480, height = 480, units = "px")

# create plot
with(my.data, {
    plot(Global_active_power, type = "l", xaxt = "n", 
       ylab = "Global Active Power (kilowatts)",
       xlab = "")
  
    # write axis lables
    my.ticks = get.day.changes(Date)
    axis(side = 1, at = my.ticks$ind, labels = my.ticks$day)        
  } 
)
dev.off()