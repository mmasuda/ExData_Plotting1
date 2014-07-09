##plot4(file)
##
## argument     : file
##              :  indicates filename described by character
##              :  assume the file locates on the working directory
##
## return       : explicitly no returns
##              : implicitly save the plot into plot4.png on the working directory
##

plot4<- function(file){
        ## read file  seperation ";" , variables in all the column are assigned to character instead of factor   
        DF <- read.csv2(file, header = TRUE, sep = ";",stringsAsFactors=FALSE)
        
        ##  using data from the dates 2007-02-01 and 2007-02-02
        DF2 <- subset(DF,DF$Date == "1/2/2007" | DF$Date == "2/2/2007")
       
        ## prepare variable on x-axis 
        Sys.setlocale("LC_TIME", "C")       ## independent from local langauage setting 				
        dt <- strptime(paste(DF2$Date, DF2$Time), "%d/%m/%Y %H:%M:%S")  ## posix class
        
        ## set character size
        par(ps=10)
        
        ## set number of pictures
        par(mfcol= c(2,2))
        
        ## draw 1st picture  same as plot2
        plot(dt,DF2$Global_active_power,type="l",xlab="",ylab="Global Active Power", xaxt="n")
        r <- as.POSIXct(round(range(dt), "days"))                       ## prepare x-axis lable
        axis.POSIXct(1, at=seq(r[1],r[2], by="day"), format="%a")       ## write x-axis lable   
        
        ## draw 2nd picture same as plot3 except for transparent legend box    
        plot(dt,DF2$Sub_metering_1,type="l",xlab="",ylab="", ylim = c(0,40),col="black",xaxt="n",yaxt="n")
        par(new=T)  
        plot(dt,DF2$Sub_metering_2,type="l",xlab="",ylab="", ylim = c(0,40), col="red",xaxt="n",yaxt="n")
        par(new=T)  
        plot(dt,DF2$Sub_metering_3,type="l",xlab="",ylab="Energy sub metering",col="blue",  ylim = c(0,40),xaxt="n",yaxt="n")
        
        r <- as.POSIXct(round(range(dt), "days"))                       ## prepare x-axis lable
        axis.POSIXct(1, at=seq(r[1],r[2], by="day"), format="%a")       ## write x-axis lable
        
        ax <- c(0,10,20,30)
        axis(2,at=ax,labels=ax,las=0)
        
        legend("topright",lty=1,cex=0.667,col=c("black","red","blue"), legend =c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),bty="n")

        ## draw 3rd picture 
        plot(dt,DF2$Voltage,type="l",xlab="datetime",ylab="Voltage", xaxt="n")
        r <- as.POSIXct(round(range(dt), "days"))                       ## prepare x-axis lable
        axis.POSIXct(1, at=seq(r[1],r[2], by="day"), format="%a")       ## write x-axis lable   
        
        ## draw 4th picture 
        plot(dt,DF2$Global_reactive_power,type="l",xlab="datetime",ylab="Global_reactive_power", xaxt="n")
        r <- as.POSIXct(round(range(dt), "days"))                       ## prepare x-axis lable
        axis.POSIXct(1, at=seq(r[1],r[2], by="day"), format="%a")       ## write x-axis lable   
        
     
        ## save it to a PNG file with a width of 480 pixels and a height of 480 pixels      
        dev.copy(png,file="plot4.png",width = 480, height = 480, units = "px")
        dev.off()
        
}