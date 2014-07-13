# downloaded and unzipped target file manually
print("Reading Text File")
h<-read.table("household_power_consumption.txt",sep=";",header=TRUE)
print("File Read Over")
# subset the file into smaller chunk
loa3=(as.character(h$Date)=="1/2/2007") | (as.character(h$Date)=="2/2/2007")
a5=h[loa3,]
#convert data into suitable formats
a5gap=as.numeric(as.character(a5$Global_active_power))
a5qw=as.numeric(as.character(a5$Sub_metering_1))
a5qwe=as.numeric(as.character(a5$Sub_metering_2))
a5qwer=as.numeric(as.character(a5$Sub_metering_3))
#converting date into suitable format
a5d=substr(a5$Date,1,1)
a5m=substr(a5$Date,3,3)
a5ds=paste("2007","-0",a5m,"-0",a5d,sep="")
a5myd=paste(a5ds,a5$Time,sep=" ")
a5mydf=as.POSIXlt(a5myd)

# draw the graphs

png(file="plot1.png")
hist(a5gap, col="red",xlab="Global Active Power (Kilowatts)", main='Global Active Power')
dev.off()


#png(file="plot2.png")
#with(a5,plot(a5mydf,a5gap, type="n",xlab="",ylab="Global Active Power (Kilowatts)"))
#lines(a5mydf,a5gap,lty=1, col="black")
#dev.off()

#png(file="plot3.png")
#with(a5,plot(a5mydf,a5qw,type="n",xlab="",ylab="Energy sub metering"))
#lines(a5mydf,a5qw,lty=1, col="black")
#lines(a5mydf,a5qwe,lty=1, col="red")
#lines(a5mydf,a5qwer,lty=1, col="blue")
#legend("topright",lwd=2,col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
#dev.off()