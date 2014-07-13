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