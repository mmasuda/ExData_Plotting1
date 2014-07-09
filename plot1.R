##plot1(file)
##
## argument     : file
##              :  indicates filename described by character
##              :  assume the file locates on the working directory
##
## return       : explicitly no returns
##              : implicitly save the plot into plot1.png on the working directory
##

plot1<- function(file){
        ## read file  seperation ";" , variables in all the column are assigned to character instead of factor   
        DF <- read.csv2(file, header = TRUE, sep = ";",stringsAsFactors=FALSE)
        
        ##  using data from the dates 2007-02-01 and 2007-02-02
        DF2 <- subset(DF,DF$Date == "1/2/2007" | DF$Date == "2/2/2007")
        
        ## set character size
        par(ps=12)
        
        ## draw histgram in red colour and with titles
        hist(as.numeric(DF2$Global_active_power), col="red", main="Global Active Power",xlab="Global Active Power(kilowatts)")
        
        ## save it to a PNG file with a width of 480 pixels and a height of 480 pixels      
        dev.copy(png,file="plot1.png",width = 480, height = 480, units = "px")
        dev.off()
        
}