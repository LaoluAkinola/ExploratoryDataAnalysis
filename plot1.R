library(dplyr)
library(readr)
library(lubridate)

data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)
data_tbl <- tbl_df(data)
data_tbl <- mutate(data_tbl, Date = as.Date(as.character(data_tbl[["Date"]]), format = "%d/%m/%Y"))

startDate <- as.Date("2007/02/01", format = "%Y/%m/%d")
endDate <- as.Date("2007/02/02", format = "%Y/%m/%d")
data_tbl <- filter(data_tbl, (Date >= startDate & Date <= endDate))


times <- as.character(data_tbl[["Time"]])
dates <- as.character(data_tbl[["Date"]])

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

data_tbl <- mutate(data_tbl, Time = times)
data_tbl <- mutate_cond(data_tbl, Global_active_power == "?", Global_active_power = NA)
data_tbl <- mutate_cond(data_tbl, Global_reactive_power == "?", Global_reactive_power = NA)
data_tbl <- mutate_cond(data_tbl, Voltage == "?", Voltage = NA)
data_tbl <- mutate_cond(data_tbl, Global_intensity == "?", Global_intensity = NA)
data_tbl <- mutate_cond(data_tbl, Sub_metering_1 == "?", Sub_metering_1 = NA)
data_tbl <- mutate_cond(data_tbl, Sub_metering_2 == "?", Sub_metering_2 = NA)
data_tbl <- mutate_cond(data_tbl, Sub_metering_3 == "?", Sub_metering_3 = NA)

data_tbl <- mutate(data_tbl, Global_active_power = parse_number(as.character(Global_active_power)))
data_tbl <- mutate(data_tbl, Global_reactive_power = parse_number(as.character(Global_reactive_power)))
data_tbl <- mutate(data_tbl, Voltage = parse_number(as.character(Voltage)))
data_tbl <- mutate(data_tbl, Global_intensity = parse_number(as.character(Global_intensity)))
data_tbl <- mutate(data_tbl, Sub_metering_1 = parse_number(as.character(Sub_metering_1)))
data_tbl <- mutate(data_tbl, Sub_metering_2 = parse_number(as.character(Sub_metering_2)))
data_tbl <- mutate(data_tbl, Sub_metering_3 = parse_number(as.character(Sub_metering_3)))
data_tbl <- mutate(data_tbl, Day = day(Date))

# Plot 1
png("plot1.png")
hist(data_tbl[["Global_active_power"]], col = "red", 
     xlab = "Global Active Power (kilowatts)", 
     ylab = "Frequency", main = "Global Active Power")
dev.off()