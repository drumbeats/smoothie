complete <- function(directory, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files

      ## 'id' is an integer vector indicating the monitor ID numbers
      ## to be used
        
      ## Return a data frame of the form:
      ## id nobs
      ## 1  117
      ## 2  1041
      ## ...
      ## where 'id' is the monitor ID number and 'nobs' is the
      ## number of complete cases
	
	mycol1 <-integer()
	mycol2 <-integer()
	for (i in id) {
		data<-getmonitor(i,"specdata")
		q1=NROW(na.omit(data))
		mycol1<-c(mycol1,i)
		mycol2<-c(mycol2,q1)
		#print(q1)
	}
	mydf=data.frame(id=mycol1,nobs=mycol2)
	return (mydf)
}

getmonitor <- function(id, directory, summarize = FALSE) {
	## 'id' is a vector of length 1 indicating the monitor ID
	## number. The user can specify 'id' as either an integer, a
	## character, or a numeric.  
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

  	## 'summarize' is a logical indicating whether a summary of
  	## the data should be printed to the console; the default is
  	## FALSE
        
  	## Your code here
	getmystr<-function(myid){
		v01=as.integer(myid)
		v02=as.character(v01)
		if (v01<10) {
			v03=paste("00",v02,sep="")
		}
		else if (v01<100){
			v03=paste("0",v02,sep="")
		}
		else {
			v03=v02
		}
		return (v03)
	}
	if (is.numeric(id) |is.integer(id) ){
		v03=getmystr(id)
	}
	if (is.character(id)){
		v04=as.integer(id)
		v03=getmystr(v04)
	}
	filename<-paste(directory,"/",v03,".csv",sep="")
  	data <- read.csv( filename )
	if (summarize) {
		print(summary(data))
	}
	return(data)
}
