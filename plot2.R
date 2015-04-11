# function to get the data
# Please ensure that the text file is placed in Your working directory
# and is named appropriately as "household_power_consumption.txt"

get_data <- function(){
        input <- read.table(paste(getwd(),"household_power_consumption.txt", sep = "/"),
                            sep = ";",header = T, na.strings = "?", nrows = 2075259)
        
        input$Date = as.Date(input$Date ,"%d/%m/%Y")
        input <- input[input$Date >=as.Date("2007-02-01","%Y-%m-%d")
                       & input$Date < as.Date("2007-02-03","%Y-%m-%d"),]
        
        # Convert Date and Time into a string
        input$DateTime <- paste(input$Date, input$Time, sep = " ")
        # COnvert DateTime into POSIXlt
        input$DateTime <- strptime(input$DateTime,
                                   format = "%Y-%m-%d %H:%M:%S")
        
        input <- input[,c(-1,-2)]
        colnames(input)
        input <- input[,c("DateTime","Global_active_power",
                          "Global_reactive_power","Voltage",
                          "Global_intensity","Sub_metering_1",
                          "Sub_metering_2","Sub_metering_3")]
        return(input)
}


# second plot
plot2 <- function(input){
        png(file = "plot2.png",width = 480, height = 480) 
        par(cex.lab = 0.8, cex.axis = 0.7,
            oma = c(0,0,0,0), mgp =c(2,1,0))
        with(input, plot(DateTime, Global_active_power,
                         type = "l", xlab = character(0),
                         ylab = "Global Active Power (kilowatts)" ))
        dev.off()
        return(paste("please check ",getwd()," for plot.2"))
}

# run both functions
input <- get_data()
plot2(input)
