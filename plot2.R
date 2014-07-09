##plot2(file)
##
## argument     : file
##              :  indicates filename described by character
##              :  assume the file locates on the working directory
##
## return       : explicitly no returns
##              : implicitly save the plot into plot2.png on the working directory
##

plot2<- function(file){
        ## read file  seperation ";" , variables in all the column are assigned to character instead of factor   
        DF <- read.csv2(file, header = TRUE, sep = ";",stringsAsFactors=FALSE)
        
        ##  using data from the dates 2007-02-01 and 2007-02-02
        DF2 <- subset(DF,DF$Date == "1/2/2007" | DF$Date == "2/2/2007")
       
        ## prepare variable on x-axis 
        Sys.setlocale("LC_TIME", "C")       ## independent from local langauage setting 				
        dt <- strptime(paste(DF2$Date, DF2$Time), "%d/%m/%Y %H:%M:%S")  ## posix class
        
        ## set character size
        par(ps=12)
        
        ## draw line chart with y-axis title   
        plot(dt,DF2$Global_active_power,type="l",xlab="",ylab="Global Active Power (kilowatts)", xaxt="n")
        
        ## draw x-axis label
        r <- as.POSIXct(round(range(dt), "days"))                       ## prepare x-axis lable
        axis.POSIXct(1, at=seq(r[1],r[2], by="day"), format="%a")       ## write x-axis lable   
        
        ## save it to a PNG file with a width of 480 pixels and a height of 480 pixels      
        dev.copy(png,file="plot2.png",width = 480, height = 480, units = "px")
        dev.off()
        
}