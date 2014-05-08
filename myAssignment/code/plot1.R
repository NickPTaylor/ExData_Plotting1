require(sqldf)

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

# read data
my.data = get.data()

# write plot to png
png(filename = "../figure/plot1.png", width = 480, height = 480, units = "px")

# create plot
with(my.data, 
     hist(Global_active_power,
          col = "red",
          main = "Global Active Power", 
          xlab = "Global Active Power (kilowatts)")
)
dev.off()